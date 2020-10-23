invisible({
  # load libraries and global environment ####
  pkgs = c('data.table', 'dplyr', 'jsonlite', 'reticulate',
           'parallel', 'purrr', 'stringr', 'tibble')
  lapply(pkgs, library, character.only = TRUE)
  codes = c('build_model', 'calc_target', 'make_slices', 'run_ep_sim', 'tidy_sample')
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
  
  # base function ####
  # process slices of simulations
  ProcessSlices = function(sample, n, size, periods) {
    # run simulations
    ProcessEPSims(sample, output_dir, cores_left)
    # calculate targets and add them to the sample
    samples = lapply(periods, ApplyCalcTarget, sample, output_dir, occup, inmet)
    # define case
    case = str_pad(n, str_length(size), 'left', 0)
    # write sample file
    mapply(WriteSlice, periods, samples, MoreArgs = list(result_dir, case))
    # remove simulation files
    RmCSVs(output_dir)
    # rename error files
    MvErrs(output_dir, result_dir, case)
  }
  
  # main code ####
  # generate sample
  py_run_file('./code/saltelli_sample.py')
  # read and tidy up sample
  sample = TidySample(saltelli_path, seeds_dir, models_dir, epws_dir, inmet)
  sample = sample[1:8, ]
  # build cases
  cores = detectCores() - cores_left
  with(sample, mcmapply(BuildModel, seed_path, area, ratio, height, azimuth, shell_wall,
                        abs_wall, shell_roof, abs_roof, wwr_liv, wwr_dorm, u_window, shgc,
                        open_factor, blind, balcony, mirror, model_path, 'op_temp',
                        MoreArgs = list(construction, fill, setup, geometry),
                        mc.cores = cores))
  # run simulations in slices
  size = nrow(sample) %/% cores
  n = 1:size
  sample = MakeSlices(sample, n, size, cores)
  periods = c('year', 'month')
  mapply(ProcessSlices, sample, n, MoreArgs = list(size, periods))
  # pile up results
  patterns = paste0(periods, '.*\\.csv')
  sample_paths = paste0(dirname(sample_path), '/sample_', periods, '.csv')
  mapply(WriteSample, patterns, sample_paths, result_dir)
  lapply(c('summary', 'description'), HandleSlices, result_dir)
  # join samples
  JoinSamples(saltelli_path, sample_paths[1])
})
