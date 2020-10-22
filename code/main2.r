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
  models_dir = '~/rolante/master/model/'
  epws_dir = '~/rolante/weather/'
  output_dir = '~/rolante/master/output/'
  result_dir = '~/rolante/master/result/'
  sample_path = './result/sample.csv'
  cores_left = 0
  cores = detectCores()
  
  # functions ####
  # bind 
  BindCSVs = function(period, sample_path) {
    file_paths = dir(result_dir, paste0(period, '.*\\.csv'), full.names = TRUE)
    file_paths %>%
      lapply(read.csv) %>%
      bind_rows() %>%
      write.csv(sample_path, row.names = FALSE)
  }
  # process slices of simulations
  ProcessSlices = function(sample, n, size, periods) {
    # run simulations
    ProcessEPSims(sample, output_dir, cores_left)
    # calculate targets and add them to the sample
    samples = lapply(periods, ApplyCalcTarget, sample, output_dir, occup, inmet)
    # write sample file
    case = str_pad(n, str_length(size), 'left', 0)
    result_paths = paste0(result_dir, case, '_sample_', periods, '.csv')
    mapply(write.csv, samples, result_paths, MoreArgs = list(row.names = FALSE))
    # remove simulation files
    file_paths = dir(output_dir, pattern = '\\.csv', full.names = TRUE)
    file.remove(file_paths)
    file_paths = dir(output_dir, pattern = '\\.txt', full.names = TRUE)
    case = str_pad(n, str_length(size), 'left', 0)
    file.rename(file_paths, paste0(result_dir, case, '_', basename(file_paths)))
  }
  
  # main code ####
  # generate sample
  py_run_file('./code/saltelli_sample.py')
  # read and tidy up sample
  sample = TidySample(saltelli_path, seeds_dir, models_dir, epws_dir, inmet)
  sample = sample[1:4, ]
  # build cases
  with(sample, mcmapply(BuildModel, seed_path, area, ratio, height, azimuth, shell_wall,
                        abs_wall, shell_roof, abs_roof, wwr_liv, wwr_dorm, u_window, shgc,
                        open_factor, blind, balcony, mirror, model_path, 'op_temp',
                        MoreArgs = list(construction, fill, setup, geometry),
                        mc.cores = cores - cores_left))
  # run simulations in slices
  size = nrow(sample) %/% cores
  n = 1:size
  sample = split(sample, c(rep(n, each = cores), rep(length(n + 1), nrow(sample) %% cores)))
  periods = c('year', 'month')
  mapply(ProcessSlices, sample, n, size, MoreArgs = list(periods))
  # pile up results
  sample_paths = paste0(dirname(sample_path), '/sample_', periods, '.csv')
  mapply(BindCSVs, periods, sample_paths)
  # join samples
  JoinSamples(saltelli_path, sample_paths[1])
})
