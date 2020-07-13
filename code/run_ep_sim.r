# load global environment ####
load('~/git/handy/inmet.rds')

# base functions ####
# compile errors into one file
CompErrs = function(dir, ind,
                    comp_name = 'errors_description.txt') {
  # dir: error files directory
  # ind: terminal errors count index
  # comp_path: compilation file path
  comp_path = paste0(dir, comp_name)
  errs_path = dir(dir, '.err', full.names = TRUE)
  file.create(comp_path)
  sapply(errs_path, LabelErr)
  file.append(comp_path, errs_path)
}
# count terminal and simulation errors
CountErrs = function(dir, ind, comp_path,
                     comp_name = 'errors_description.txt',
                     summ_name = 'errors_summary.txt') {
  # dir: error files directory
  # ind: terminal errors count index
  # comp_path: compilation file path
  # summ_path: summary file path
  comp_path = paste0(dir, comp_name)
  summ_path = paste0(dir, summ_name)
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
DefSimGrid = function(models_dir, epws_dir, weathers, inmet, form) {
  # models_dir: models directory
  # epws_dir: weather files directory
  # weathers: weathers of interest
  # form: simulation files format
  models_path = dir(models_dir, form, full.names = TRUE)
  sims_grid = expand.grid('model_path' = models_path, 'prefix' = weathers,
                          stringsAsFactors = FALSE)
  index = sapply(weathers, function(x, y) which(x == y), inmet$municipio)
  sims_grid$epw_path = inmet$arquivo_climatico[index[sims_grid$prefix]]
  sims_grid$epw_path = paste0(epws_dir, sims_grid$epw_path, '.epw')
  zbs = inmet$zb[index[sims_grid$prefix]]
  index = grepl('ref17', sims_grid$model_path) & zbs == 8 |
    grepl('ref8', sims_grid$model_path) & zbs != 8
  sims_grid = sims_grid[!index, ]
  models_name = sims_grid$model_path %>%
    basename() %>% str_remove('\\.epJSON')
  sims_grid$prefix = paste0(sims_grid$prefix, '_', models_name)
  return(sims_grid)
}
# label error files
LabelErr = function(path) {
  # path: error file path
  text = readLines(path)
  writeLines(c(paste0('Sim File: ', path), '', text, '\n'), path)
}
# remove suffix ('out') from a file name
RnmFile = function(path) {
  # path: file path/name
  file.rename(path, paste0(str_sub(path, 0, -8), str_sub(path, -4, -1)))
}
# remove unsefull files
RmUnsFiles = function(dir, rm_all_but = c('.csv', '.err'),
                      rm_also = c('sqlite.err', 'tbl.csv', 'ssz.csv', 'zsz.csv')) {
  # dir: files directory
  # rm_all_but: files that shouldn't be removed
  rm_all_but = str_flatten(rm_all_but, collapse = '|')
  rm_also = str_flatten(rm_also, collapse = '|')
  files_path = dir(dir, full.names = TRUE)
  index = !grepl(rm_all_but, files_path) | grepl(rm_also, files_path)
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
RunEPSim = function(model_path, epw_path, prefix, output_dir) {
  # model_path: full model file path
  # epw_path: full weather file path
  # weather: correspondent weather file
  # output_dir: output directory
  args = c('-r', '-w', epw_path, '-d', output_dir, '-p', prefix, model_path)
  system2('energyplus', args, stdout = FALSE, stderr = FALSE)
}

# main function ####
ProcessEPSims = function(sample, load_files, models_dir, epws_dir, weathers,
                         output_dir, cores_left, inmet, form = '\\.epJSON') {
  # load_files: 'TRUE' (generate grid according to files inside models_dir and epws_dir) or
    # 'FALSE' (load the sample grid)
  # models_dir: energyplus simulation files directory
  # epws_dir: energyplus weather files directory
  # weathers: vector of chosen weathers
    # e.g.: c('rio_de_janeiro', 'sao_paulo')
  # sample: sample grid with simulation informations
  # output_dir: output directory
  # form: simulation file format (.epJSON or .idf)
  # list models and weather files path in a grid
  if (load_files) {
    sims_grid = DefSimGrid(models_dir, epws_dir, weathers, inmet, form)
  } else {
    sims_grid = select(sample, model_path, epw_path, prefix)
  }
  # run simulations in parallel
  errs_ind = mcmapply(RunEPSim, sims_grid$model_path, sims_grid$epw_path,
                      sims_grid$prefix, output_dir, mc.cores = detectCores() - cores_left)
  # remove all files but .csv and .err
  RmUnsFiles(output_dir)
  # list and rename the outputs left
  files_path = dir(output_dir, full.names = TRUE)
  sapply(files_path, RnmFile)
  # compile errors
  CompErrs(output_dir, errs_ind)
  # remove .err files
  RmUnsFiles(output_dir, rm_all_but = c('.csv', '.txt'))
  # count terminal and severe errors and warnings
  CountErrs(output_dir, errs_ind)
}