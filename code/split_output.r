# base functions ####
# define a vector with zones
DefZones = function(input_path, habs, nstrs) {
  case =  str_extract(input_path, '(?<=(ref(17|8)|tm|tv|sf)).*')
  count = str_count(case, '_')
  rooms = c('liv', 'dorm1', 'dorm2')
  if (count == 0) {
    zones = paste0('f', 1:nstrs) %>%
      expand.grid(habs, rooms) %>%
      apply(1, str_flatten, collapse = '_')
  } else {
    zones = str_sub(case, 2, -5)
    if (count == 2) {
      zones = paste0(zones, '_', rooms)
    }
  }
  return(zones)
}
# subset columns of the data frame for an specific zone
SubsetZone = function(zone, df) {
  # select the columns related to each interested zone
  index = grepl(paste0('^', toupper(zone)), colnames(df))
  df = df[, index]
  colnames(df) = str_remove(colnames(df), '^F[0-9]_')
  return(df)
}
# write file
WriteFile = function(zone, df, prefix, output_dir) {
  df_zone = SubsetZone(zone, df)
  prefix = str_remove(prefix, '_f[0-9].*')
  output_path = paste0(output_dir, prefix, '_', zone, '.csv')
  write.csv(df_zone, output_path, row.names = FALSE)
}

# main functions ####
# apply the SplitOutput()
ApplySplOut = function(input_dir, typo, nstrs, output_dir, cores_left) {
  if (typo == 'linear') {
    habs = c('csw', 'msw', 'mse', 'cse', 'cne', 'mne', 'mnw', 'cnw')
  }
  input_paths = dir(input_dir, '\\.csv$', full.names = T)
  zones = lapply(input_paths, DefZones, habs, nstrs)
  mapply(SplitOutput, input_paths, zones, output_dir, cores_left)
}
# split outputs for each zone and write files
SplitOutput = function(input_path, zones, output_dir, cores_left) {
  # input_dir: directory where the full floor simulation's outputs are located
  # zones: names of the zones to be splitted (by default it pick up all the zones in the
  # standard model)
  # output_dir: directory where the splitted files will be saved
  # load data
  df = input_path %>%
    fread() %>%
    as.data.frame()
  # subset and write csv file
  prefix = input_path %>% basename() %>% str_remove_all('(?<=ref)(17|8)|\\.csv$')
  mclapply(zones, WriteFile, df, prefix, output_dir, mc.cores = detectCores() - cores_left)
  # clean cache
  rm(df)
  gc()
}
