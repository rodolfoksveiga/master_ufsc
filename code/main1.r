# load global environment ####
pkgs = c('data.table', 'dplyr', 'jsonlite', 'parallel', 'purrr', 'stringr', 'tibble')
lapply(pkgs, library, character.only = TRUE)
codes = c('build_model', 'link_sample', 'process_output',
          'run_ep_sim', 'shrink_building', 'split_output')
codes = paste0('./code/', codes, '.r')
lapply(codes, source)
inmet = read.csv('./seed/inmet_list.csv')
occup = read.csv('./seed/occup.csv')
geometry = read_json('./seed/geometry.json')[[1]]
out_temp = read.csv('./seed/out_temp.csv')
construction = read_json('./seed/construction.json')
fill = read_json('./seed/fill.json')
setup = read_json('./seed/setup1.json')

# variables ####
seeds_dir = './seed/'
models_dir = '~/rolante/master/simp/model/'
epws_dir = '~/rolante/weather/'
weathers = c('curitiba', 'rio_de_janeiro', 'sao_paulo', 'sorriso', 'teresina')
output_dir = '~/rolante/master/simp/output/'
split_dir = '~/rolante/master/simp/split/'
sample_path = './result/sample1.csv'
result_dir = '~/rolante/master/simp/result/'
cores_left = 0

# main code ####
# generate sample
sample = expand.grid(simp = 0:1, typo = 'linear', stringsAsFactors = FALSE,
                     shell = c('ref17', 'ref8', 'tm', 'tv', 'sf'))
write.csv(sample, sample_path, row.names = FALSE)
# link sample to appropriate values
sample = LinkSample(sample_path, seeds_dir, models_dir)
# build cases
mcmapply(BuildModel,
         seed_path = sample$seed_path,
         area = NA, ratio = NA, height = NA, azimuth = 0, nstrs = 5,
         shell_wall = sample$shell_wall, abs_wall = 0.5,
         abs_roof = 0.6, shell_roof = sample$shell_roof,
         wwr_liv = NA, wwr_dorm = NA, u_window = 5.7, shgc = 0.87, open_factor = 0.45,
         blind = FALSE, balcony = 0,
         model_path = sample$model_path,
         boundary = sample$boundary, scale = FALSE,
         MoreArgs = list(construction, fill, setup, geometry),
         mc.cores = detectCores() - cores_left)
# shrink building
ApplyShkBuild(models_dir, 'linear', 5, models_dir, cores_left)
# run simulations
ProcessEPSims(NULL, models_dir, epws_dir, weathers, output_dir, cores_left, inmet)
# split outputs
ApplySplOut(output_dir, 'linear', 5, split_dir, cores_left)
# process outputs
ApplySplOut(split_dir, 'linear', 5, result_dir)