# avoid undesirable outputs on prompt
invisible({
  # load global environment ####
  pkgs = c('data.table', 'dplyr', 'jsonlite', 'parallel', 'purrr', 'stringr', 'tibble')
  lapply(pkgs, library, character.only = TRUE)
  codes = c('build_model', 'link_sample', 'process_output',
            'run_ep_sim', 'shrink_building', 'split_output')
  codes = paste0('./code/', codes, '.r')
  lapply(codes, source)
  inmet = read.csv('./source/inmet_list.csv')
  occup = read.csv('./source/occup.csv')
  geometry = read_json('./source/geometry.json')[[1]]
  out_temp = read.csv('./source/out_temp.csv')
  construction = read_json('./source/construction.json')
  fill = read_json('./source/fill.json')
  setup = read_json('./source/setup.json')
  
  # variables ####
  seeds_dir = './seed/'
  models_dir = '~/rolante/master/model/'
  shrink_dir = '~/rolante/master/shrink/'
  epws_dir = '~/rolante/weather/'
  weathers = c('curitiba', 'rio_de_janeiro', 'sao_paulo', 'sorriso', 'teresina')
  output_dir = '~/rolante/master/output/'
  split_dir = '~/rolante/master/split/'
  result_dir = '~/rolante/master/result/'
  cores_left = 0
  
  # functions ####
  ManageFiles = function(temp_dir, pattern) {
    file_names = dir(temp_dir, 'errors')
    file.rename(paste0(temp_dir, file_names), paste0(temp_dir, pattern, '_', file_names))
    file_names = dir(temp_dir, full.names = TRUE)
    file.rename(file_names, str_replace(file_names, paste0(pattern, '/'), ''))
    unlink(temp_dir, recursive = TRUE)
  }
  
  # main code ####
  # generate sample
  sample = expand.grid(simp = 0:1, typo = 'linear', stringsAsFactors = FALSE,
                       shell = c('ref17', 'ref8', 'tm', 'tv', 'sf'))
  # link sample to appropriate values
  sample = LinkSample(sample, seeds_dir, models_dir)
  # build cases
  outputs = c('mean_temp', 'op_temp', 'air_change', 'therm_bal', 'surf_temp')
  mcmapply(BuildModel,
           seed_path = sample$seed_path, nstrs = 5,
           area = NA, ratio = NA, height = NA, azimuth = 0,
           shell_wall = sample$shell_wall, abs_wall = 0.5,
           shell_roof = sample$shell_roof, abs_roof = 0.6,
           wwr_liv = NA, wwr_dorm = NA, u_window = 5.7, shgc = 0.87, open_factor = 0.45,
           blind = FALSE, balcony = 0, mirror = FALSE,
           scale = FALSE, boundary = sample$boundary,
           model_path = sample$model_path,
           MoreArgs = list(outputs, construction, fill, setup, geometry),
           mc.cores = detectCores() - cores_left)
  # shrink building
  ApplyShrinkBuild(models_dir, 'linear', 5, shrink_dir, cores_left)
  # run simulations
  sim_cores_left = detectCores() - 2
  ProcessEPSims(NULL, models_dir, epws_dir, weathers, output_dir, sim_cores_left, inmet)
  # ProcessEPSims(NULL, shrink_dir, epws_dir, weathers, split_dir, cores_left, inmet)
  # split outputs
  ApplySplOut(output_dir, 'linear', 5, split_dir, cores_left)
  # process outputs
  ApplyProcessOut(split_dir, 'linear', 5, result_dir)
})