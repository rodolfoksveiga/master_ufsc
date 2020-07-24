# load libraries and global environment ####
invisible({
  pkgs = c('data.table', 'dplyr', 'jsonlite', 'parallel',
           'purrr', 'reticulate', 'stringr', 'tibble')
  lapply(pkgs, library, character.only = TRUE)
  codes = c('build_case', 'calc_targ', 'tidy_sample', 'run_ep_sim')
  codes = paste0('./code/', codes, '.r')
  lapply(codes, source)
})

# variables ####
sobol_paths = paste0('./code/sobol_', c('train', 'test'), '.csv')
seeds_dir = './seed/'
models_dir = '~/rolante/master/model/'
inmet_path = './source/inmet_list.csv'
epws_dir = '~/rolante/weather/'
output_dir = '~/rolante/master/output/'
sample_path = './result/sample.csv'
cores_left = 0

# main code ####
# generate sample
py_run_file('./code/saltelli_sample.py')
# read and edit sample
inmet = read.csv(inmet_path, stringsAsFactors = FALSE)
sample = TidySample(sobol_paths, seeds_dir, models_dir, epws_dir, inmet)
# build cases
complements = list(construction, fill, setup, geometry)
invisible(
  mcmapply(BuildCase, sample$seed_path, sample$area, sample$ratio, sample$height, sample$azimuth,
           sample$shell_wall, sample$shell_roof, sample$abs_wall, sample$abs_roof, sample$wwr_liv,
           sample$wwr_dorm, sample$u_window, sample$shgc, sample$open_factor, sample$model_path,
           MoreArgs = complements, mc.cores = detectCores() - cores_left)
)
# run simulations
ProcessEPSims(sample = sample, load_files = FALSE, cores_left = 0, output_dir = output_dir)
# calculate targets and add them to the sample
sample = AddTargToSample(sample, output_dir, inmet)
# write sample file 
write.csv(sample, sample_path, row.names = FALSE)