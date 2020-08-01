# load libraries and global environment ####
invisible({
  pkgs = c('data.table', 'dplyr', 'jsonlite', 'parallel',
           'purrr', 'stringr', 'tibble')
  lapply(pkgs, library, character.only = TRUE)
  codes = c('build_model', 'calc_targ', 'tidy_sample', 'run_ep_sim')
  codes = paste0('./code/', codes, '.r')
  lapply(codes, source)
  geometry = read_json('./seed/geometry2.json')
  construction = read_json('./seed/construction.json')
  fill = read_json('./seed/fill.json')
  setup = read_json('./seed/setup.json')
})

# variables ####
sobol_paths = paste0('./code/sobol_', c('train', 'test'), '.csv')
seeds_dir = './seed/'
models_dir = '~/rolante/master/data/model/'
inmet_path = './source/inmet_list.csv'
epws_dir = '~/rolante/weather/'
output_dir = '~/rolante/master/data/output/'
sample_path = './result/sobol_sample2.csv'
cores_left = 0

# main code ####
# generate sample
py_run_file('./code/saltelli_sample.py')
# read and tidy up sample
sample = TidySample(sobol_paths, seeds_dir, models_dir, epws_dir, inmet)
# build cases
invisible(
  mcmapply(BuildModel,
           seed_path = sample$seed_path,
           area = , ratio = , height = , azimuth = ,
           shell_wall = sample$shell_wall, abs_wall = ,
           abs_roof = , shell_roof = sample$shell_roof,
           wwr_liv = , wwr_dorm = , u_window = , shgc = , open_factor = ,
           blind = , balcony = ,
           model_path = sample$model_path,
           MoreArgs = list(construction, fill, setup, geometry),
           mc.cores = detectCores() - cores_left)
)
# run simulations
ProcessEPSims(sample = sample, load_files = FALSE, cores_left = 0, output_dir = output_dir)
# calculate targets and add them to the sample
sample = AddTargToSample(sample, output_dir, inmet)
# write sample file 
write.csv(sample, sample_path, row.names = FALSE)