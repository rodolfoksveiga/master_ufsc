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
  nstrs = 5
  cores_left = 0
  
  # functions ####
  # handle results
  HandleResults = function(pattern, folder) {
    file_paths = dir(folder, pattern, full.names = TRUE)
    if (pattern == 'csv') {
      file_paths %>%
        lapply(read.csv) %>%
        bind_rows() %>%
        write.csv(paste0(result_dir, 'data_simp.csv'), row.names = FALSE)
    } else {
      files = lapply(file_paths, readLines)
      if (pattern == 'summary') {
        file = files %>%
          lapply(function(x) as.numeric(str_extract(x, '\\d'))) %>%
          as.data.frame() %>%
          slice(-1) %>%
          AddSumms()
      } else if (pattern == 'description') {
        file = unlist(files)
      } else {
        stop('Pattern is not supported!')
      }
      writeLines(file, paste0(result_dir, 'errors_', pattern, '.txt'))
    }
    file.remove(file_paths)
  }
  # process slices of simulations
  ProcessSlices = function(sample, n, size) {
    # run simulations
    ProcessEPSims(sample, output_dir, cores_left)
    # split outputs
    lapply(c('l', 'h'), ApplySplOut, output_dir, nstrs, split_dir, cores_left)
    # process outputs
    ApplyProcessOut(split_dir, result_dir, geometry, occup, out_temp)
    # remove simulation files
    sapply(c(output_dir, split_dir), RemoveCSVs)
    # rename error files
    file_paths = dir(output_dir, pattern = '\\.txt', full.names = TRUE)
    case = str_pad(n, str_length(size), 'left', 0)
    file.rename(file_paths, paste0(result_dir, case, '_', basename(file_paths)))
  }
  # remove csv files
  RemoveCSVs = function(folder) {
    file_paths = dir(folder, pattern = '\\.csv', full.names = TRUE)
    file.remove(file_paths)
  }
  # sum errors in summary files
  AddSumms = function(df) c('Number of simulations with:',
                            paste('    Terminal errors =', sum(df[1, ])),
                            paste('    Severe errors =', sum(df[2, ])),
                            paste('    Terminal errors =', sum(df[3, ])))
  
  # main code ####
  # generate and link sample to appropriate values
  sample = expand.grid(simp = 0:1, typo = c('l', 'h'), stringsAsFactors = FALSE,
                       shell = c('ref17', 'ref8', 'tm', 'tv', 'sf')) %>%
    LinkSample(seeds_dir, models_dir) %>%
    arrange(typo, simp, shell)
  # build cases
  outputs = c('mean_temp', 'op_temp', 'air_change', 'therm_bal', 'surf_temp')
  mcmapply(BuildModel,
           seed_path = sample$seed_path, nstrs = nstrs,
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
  lapply(c('l', 'h'), ApplyShrinkBuild, models_dir, nstrs, shrink_dir, cores_left)
  file_names = dir(shrink_dir, '\\.epJSON')
  file.rename(paste0(shrink_dir, file_names), paste0(models_dir, file_names))
  # define and split simulation grid
  sample = DefSimGrid(models_dir, epws_dir, weathers, inmet, '\\.epJSON')
  ncores = detectCores()
  size = nrow(sample) %/% ncores
  n = 1:size
  sample = split(sample, c(rep(n, each = ncores), rep(length(n + 1), nrow(sample) %% ncores)))
  # run simulations in slices
  mapply(ProcessSlices, sample, n, size)
  # pile up results
  lapply(c('csv', 'summary', 'description'), HandleResults, result_dir)
})
