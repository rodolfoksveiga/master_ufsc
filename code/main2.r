invisible({
  ## libraries and global environment
  # define libraries to load
  pkgs = c('data.table', 'dplyr', 'jsonlite', 'reticulate',
           'parallel', 'purrr', 'stringr', 'tibble')
  # load libraries
  lapply(pkgs, library, character.only = TRUE)
  # define pipenv as the python virtual environment
  venv = system('pipenv --venv', inter = TRUE)
  use_virtualenv(venv, required = TRUE)
  py_config()
  # define external codes source
  codes = c('build_model', 'calc_target', 'make_slices', 'run_ep_sim', 'tidy_sample')
  codes = paste0('./code/', codes, '.r')
  # source codes
  lapply(codes, source)
  # load external data
  occup = read.csv('./source/occup.csv')
  inmet = read.csv('./source/inmet_list.csv')
  geometry = read_json('./source/geometry.json')[[2]]
  construction = read_json('./source/construction.json')
  fill = read_json('./source/fill.json')
  setup = read_json('./source/setup.json')
  
  ## variables
  # path to save the initial dataset
  saltelli_path = './result/saltelli_sample.csv'
  # directory of the seed simulation files
  seeds_dir = './seed/'
  # directory to save the energyplus simulation models
  models_dir = '~/Documents/master/model/'
  # directory of the weather files
  epws_dir = '~/Documents/weather/'
  # directory to save the simulations outputs
  output_dir = '~/Documents/master/output/'
  # directory to save the results
  result_dir = '~/Documents/master/result/'
  # path to save the tidy sample
  sample_path = './result/sample.csv'
  # number of cores not to use
  cores_left = 0
  
  ## functions
  # process slices of simulations
  ProcessSlices = function(sample, n, size) {
    # run simulations in parallel
    ProcessEPSims(sample, output_dir, cores_left)
    # calculate targets and add them to the sample
    sample = ApplyCalcTarget(sample, output_dir, occup, inmet)
    # define case
    case = str_pad(n, str_length(size), 'left', 0)
    # write sample file
    WriteSlice(sample, result_dir, case)
    # remove useless simulation outputs
    RmCSVs(output_dir)
    # rename simulation error files
    MvErrs(output_dir, result_dir, case)
  }
  
  ## main code
  # generate sample
  py_run_file('./code/saltelli_sample.py')
  # read and tidy up sample
  sample = TidySample(saltelli_path, seeds_dir, models_dir, epws_dir, inmet)
  # define number of cores to use
  cores = detectCores() - cores_left
  # build simulation files
  with(sample, mcmapply(BuildModel, seed_path, area, ratio, height, azimuth, shell_wall,
                        abs_wall, shell_roof, abs_roof, wwr_liv, wwr_dorm, u_window, shgc,
                        open_factor, blind, balcony, mirror, model_path, 'op_temp',
                        MoreArgs = list(construction, fill, setup, geometry),
                        mc.cores = cores))
  # define number of slices
  size = nrow(sample) %/% cores
  # define a vector to apply MakeSlices()
  n = 1:size
  # make a list with slices of the sample
  slices = MakeSlices(sample, n, size, cores)
  # apply ProcessSlices() on each element of the slices list
  mapply(ProcessSlices, slices, n, size)
  # pile up results
  WriteSample('sample.csv', sample_path, result_dir)
  # concatenate errors
  lapply(c('summary', 'description'), HandleSlices, result_dir)
  # join samples
  JoinSamples(saltelli_path, sample_path)
  # perform sensitivity analysis
  py_run_file('./code/saltelli_sample.py')
})
