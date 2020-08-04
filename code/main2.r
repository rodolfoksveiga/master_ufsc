# avoid undesirable outputs on prompt
invisible({
  # load libraries and global environment ####
  pkgs = c('dplyr', 'jsonlite', 'reticulate', 'parallel', 'purrr', 'stringr', 'tibble')
  lapply(pkgs, library, character.only = TRUE)
  codes = c('build_model', 'calc_targ', 'tidy_sample', 'run_ep_sim')
  codes = paste0('./code/', codes, '.r')
  lapply(codes, source)
  occup = read.csv('./seed/occup.csv')
  inmet = read.csv('./seed/inmet_list.csv')
  geometry = read_json('./seed/geometry.json')[[2]]
  construction = read_json('./seed/construction.json')
  fill = read_json('./seed/fill.json')
  setup = read_json('./seed/setup2.json')
  
  # variables ####
  sobol_path = './result/sobol_sample2.csv'
  seeds_dir = './seed/'
  models_dir = '~/rolante/master/model/'
  epws_dir = '~/rolante/weather/'
  output_dir = '~/rolante/master/output/'
  result_output = '~/rolante/master/result/'
  sample_path = './result/sample2.csv'
  cores_left = 0
  
  # main code ####
  # generate sample
  py_run_file('./code/saltelli_sample.py')
  # read and tidy up sample
  sample = TidySample(sobol_path, seeds_dir, models_dir, epws_dir, inmet)
  sample = head(sample, 100)
  # build cases
  mcmapply(BuildModel,
           seed_path = sample$seed_path, area = sample$area, ratio = sample$seed,
           height = sample$height, azimuth = sample$azimuth, shell_wall = sample$shell_wall,
           abs_wall = sample$abs_wall, shell_roof = sample$shell_roof, abs_roof = sample$abs_roof,
           wwr_liv = sample$wwr_liv, wwr_dorm = sample$wwr_dorm, u_window = sample$u_window,
           shgc = sample$shgc, open_factor = sample$open_factor, blind = sample$blind,
           balcony = sample$balcony, model_path = sample$model_path,
           MoreArgs = list(construction, fill, setup, geometry),
           mc.cores = detectCores() - cores_left)
  # run simulations
  ProcessEPSims(sample, NULL, NULL, output_dir, 0, inmet)
  # calculate targets and add them to the sample
  sample = AddTargToSample(sample, output_dir, inmet)
  # write sample file 
  write.csv(sample, sample_path, row.names = FALSE)
})