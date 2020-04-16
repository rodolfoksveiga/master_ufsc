# load libraries ####
pkgs = c('dplyr', 'parallel', 'stringr')
lapply(pkgs, library, character.only = T)

# base functions ####
# compile errors into one file
CompErrs = function(dir, ind, comp_path) {
  # dir: error files directory
  # ind: terminal errors count index
  # comp_path: compilation file path
  
  errs_path = dir(dir, '.err', full.names = TRUE)
  file.create(comp_path)
  sapply(errs_path, LabelErr)
  file.append(comp_path, errs_path)
}

# count terminal and simulation errors
CountErrs = function(dir, ind, comp_path, summ_path) {
  # dir: error files directory
  # ind: terminal errors count index
  # comp_path: compilation file path
  # summ_path: summary file path
  
  file.create(summ_path)
  sum_errs = mapply(SumErrs, list('sev' = 'Warning\\; ',
                                  'warn' = 'Successfully\\-\\- '),
                    c('[1-9] Severe Errors\\;', '[1-9] Warning\\;'),
                    MoreArgs = list(comp_path))
  writeLines(c('Number of simulations with:',
               paste0('    Terminal errors = ', sum(ind)),
               paste0('    Severe errors = ', sum_errs[1]),
               paste0('    Warnings = ', sum_errs[2])), summ_path)
}

# create a grid with model and weather files
  # add a column epw's respective weathers
    # it's be used to name output files
DefSimGrid = function(models_dir, epws_dir, weathers, form) {
  # models_dir: models directory
  # epws_dir: weather files directory
  # weathers: weathers of interest
  # form: simulation files format
  
  models_path = dir(models_dir, form, full.names = TRUE)
  pattern = str_flatten(paste0(weathers, '.*\\.epw'), collapse = '|')
  epws_path = dir(epws_dir, pattern, full.names = TRUE)
  sims_grid = expand.grid('model' = models_path, 'epw' = epws_path,
                          stringsAsFactors = FALSE)
  for (weather in weathers) {
    sims_grid = NameWeather(weather, sims_grid)
  }
  return(sims_grid)
}

# label error files
LabelErr = function(path) {
  # path: error file path
  
  text = readLines(path)
  writeLines(c(paste0('Sim File: ', path), '', text, '\n'), path)
}

# create a weather column in simulation's grid
NameWeather = function(weather, grid) {
  # weather: 
  # grid: 
  
  grid$weather[grep(weather, grid$epw)] = weather
  return(grid)
}

# remove suffix ('out') from a file name
RnmFile <- function(path) {
  # path: file path/name
  
  file.rename(path, paste0(str_sub(path, 0, -8), str_sub(path, -4, -1)))
}

# remove unsefull files
RmUnsFiles <- function(dir, rm_all_but) {
  # dir: files directory
  # rm_all_but: files that shouldn't be removed
  
  rm_all_but = str_flatten(rm_all_but, collapse = '|')
  files_path = dir(dir, full.names = TRUE)
  index = !grepl(rm_all_but, files_path) | grepl('tbl.csv|sqlite.err', files_path)
  files_path = files_path[index]
  file.remove(files_path)
}

# sum the number of warnings and severe errors in all simulations
SumErrs = function(start, end, comp_path) {
  # start: pattern before the number of simulation errors
  # end: pattern after the number of simulation errors
  # comp_path: compilation file path
  
  sum_err = comp_path %>%
    readLines() %>%
    str_detect(paste0('(?<=', start, ').*?(?=', end, ')')) %>%
    sum()
  return(sum_err)
}

# simulation function ####
# run a single energyplus simulation
RunEPSim <- function(model_path, epw_path, weather, output_dir) {
  # model_path: full model file path
  # epw_path: full weather file path
  # weather: correspondent weather file
  # output_dir: output directory
  
  prefix = paste0(weather, '_', sub('.epJSON', '', basename(model_path)))
  args = c('-r', '-w', epw_path, '-d', output_dir, '-p', prefix, model_path)
  system2('energyplus', args, stdout = FALSE, stderr = FALSE)
}

# main function ####
ProcessEPSims = function(models_dir, epws_dir, weathers, output_dir,
                         form = '.epJSON',
                         comp_name = 'errors_description.txt',
                         summ_name = 'errors_summary.txt') {
  # models_dir:
  # epws_dir:
  # weathers: 
  # output_dir: 
  # form: 
  # comp_name: 
  # summ_name: 
  
  # list models and weather files path in a grid
  sims_grid = DefSimGrid(models_dir, epws_dir, weathers, form)
  # run simulations in parallel
  errs_ind = mcmapply(RunEPSim, sims_grid$model, sims_grid$epw,
                      sims_grid$weather, output_dir, mc.cores = detectCores())
  # remove all files but .csv and .err
  RmUnsFiles(output_dir, rm_all_but = c('.csv', '.err'))
  # list and rename the outputs left
  files_path = dir(output_dir, full.names = TRUE)
  sapply(files_path, RnmFile)
  # compile errors
  err_comp_path = paste0(output_dir, comp_name)
  CompErrs(output_dir, errs_ind, err_comp_path)
  # remove .err files
  RmUnsFiles(output_dir, rm_all_but = c('.csv', '.txt'))
  # count terminal and severe errors and warnings
  err_summ_path = paste0(output_dir, summ_name)
  CountErrs(output_dir, errs_ind, err_comp_path, err_summ_path)
}

# application ####
ProcessEPSims(models_dir = '',
             epws_dir = '',
             weathers = c(''),
             output_dir = '')




