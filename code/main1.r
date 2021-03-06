# avoid undesirable outputs on prompt
invisible({
  # load global environment ####
  pkgs = c('data.table', 'dplyr', 'jsonlite', 'parallel', 'purrr', 'stringr', 'tibble')
  lapply(pkgs, library, character.only = TRUE)
  codes = c('build_model', 'make_slices', 'link_sample', 'process_output',
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
  sample_path = './result/sample_simp.csv'
  nstrs = 5
  cores_left = 0
  
  # base function ####
  # process slices of simulations
  ProcessSlices = function(sample, n, size) {
    # run simulations
    ProcessEPSims(sample, output_dir, cores_left)
    # split outputs
    typos = sample$prefix %>% str_extract('(?<=_)(l|h)(?=_)') %>% unique()
    lapply(typos, ApplySplOut, output_dir, nstrs, split_dir, cores_left)
    # process outputs
    ApplyProcessOut(split_dir, result_dir, geometry, occup, out_temp)
    # remove simulation files
    sapply(c(output_dir, split_dir), RmCSVs)
    # rename and move error files
    case = str_pad(n, str_length(size), 'left', 0)
    MvErrs(output_dir, result_dir, case)
  }
  
  # main code ####
  # generate and link sample to appropriate values
  grid = expand.grid(simp = 0:1, typo = c('l', 'h'), stringsAsFactors = FALSE,
                     shell = c('ref17', 'ref8', 'tm', 'tv', 'sf')) %>%
    LinkSample(seeds_dir, models_dir)
  # build cases
  cores = detectCores() - cores_left
  outputs = c('mean_temp', 'op_temp', 'air_change', 'therm_bal', 'surf_temp')
  mcmapply(BuildModel,
           seed_path = grid$seed_path, nstrs = nstrs,
           area = NA, ratio = NA, height = NA, azimuth = 0,
           shell_wall = grid$shell_wall, abs_wall = 0.5,
           shell_roof = grid$shell_roof, abs_roof = 0.6,
           wwr_liv = NA, wwr_dorm = NA, u_window = 5.7, shgc = 0.87, open_factor = 0.45,
           blind = FALSE, balcony = 0, mirror = FALSE,
           scale = FALSE, boundary = grid$boundary,
           model_path = grid$model_path,
           MoreArgs = list(outputs, construction, fill, setup, geometry),
           mc.cores = cores)
  # shrink building
  lapply(c('l', 'h'), ApplyShrinkBuild, models_dir, nstrs, shrink_dir, cores_left)
  file_names = dir(shrink_dir, '\\.epJSON')
  file.rename(paste0(shrink_dir, file_names), paste0(models_dir, file_names))
  # define and split simulation grid
  sample = DefSimGrid(models_dir, epws_dir, weathers, inmet, '\\.epJSON')
  size = nrow(sample) %/% cores
  n = 1:size
  sample = MakeSlices(sample, n, size, cores)
  # run simulations in slices
  mapply(ProcessSlices, sample, n, size)
  # pile up results
  WriteSample('\\.csv', sample_path, result_dir)
  lapply(c('summary', 'description'), HandleSlices, result_dir)
})
