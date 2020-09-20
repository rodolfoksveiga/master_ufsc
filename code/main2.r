invisible({
  # load libraries and global environment ####
  pkgs = c('data.table', 'dplyr', 'jsonlite', 'reticulate',
           'parallel', 'purrr', 'stringr', 'tibble')
  lapply(pkgs, library, character.only = TRUE)
  codes = c('build_model', 'calc_target', 'run_ep_sim', 'tidy_sample')
  codes = paste0('./code/', codes, '.r')
  lapply(codes, source)
  occup = read.csv('./source/occup.csv')
  inmet = read.csv('./source/inmet_list.csv')
  geometry = read_json('./source/geometry.json')[[2]]
  construction = read_json('./source/construction.json')
  fill = read_json('./source/fill.json')
  setup = read_json('./source/setup.json')
  
  # variables ####
  saltelli_path = './result/saltelli_sample.csv'
  seeds_dir = './seed/'
  models_dir = '~/rolante/master/model/1/'
  epws_dir = '~/rolante/weather/'
  output_dir = '~/rolante/master/output/1/'
  sample_path = './result/1/sample.csv'
  cores_left = 0
  
  # main code ####
  # generate sample
  py_run_file('./code/saltelli_sample.py')
  # read and tidy up sample
  sample = TidySample(saltelli_path, seeds_dir, models_dir, epws_dir, inmet)
  sample = sample[1:(nrow(sample) %/% 10), ]
  # build cases
  with(sample, mcmapply(BuildModel, seed_path, nstrs, area, ratio, height, azimuth,
                        shell_wall, abs_wall, shell_roof, abs_roof, wwr_liv, wwr_dorm,
                        u_window, shgc, open_factor, blind, balcony, mirror, model_path,
                        MoreArgs = list('op_temp', construction, fill, setup, geometry),
                        mc.cores = detectCores() - cores_left))
  # run simulations
  ProcessEPSims(sample, NULL, NULL, NULL, output_dir, 0)
  # calculate targets and add them to the sample
  periods = c('year', 'month')
  samples = lapply(periods, ApplyCalcTarget, sample, output_dir, occup, inmet)
  # write sample file
  periods = paste0('_', periods, '.csv')
  sample_paths = sapply(periods, str_replace, string = sample_path, pattern = '\\.csv')
  mapply(write.csv, samples, sample_paths, MoreArgs = list(row.names = FALSE))
  # # join samples
  # JoinSamples(saltelli_path, sample_paths[1])
})