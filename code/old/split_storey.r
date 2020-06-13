# load libraries ####
pkgs = c('data.table', 'parallel', 'stringr')
lapply(pkgs, library, character.only = T)

# base functions ####
# load data
LoadData = function(x) as.data.frame(fread(x))
# remove columns in the data frame related to zones that must be ignored
RmIgnZone = function(df, ign_zones) {
  ign_zones = toupper(str_flatten(ign_zones, collapse = '|'))
  df[, grepl(toupper(ign_zones), colnames(df))] = NULL
  return(df)
}
# write files for each zone
WriteZoneFile = function(zone, df, file_name, output_dir) {
  # select the columns related to each interested zone
  col_names = c('Date/Time', 'Environment:Site Outdoor Air Drybulb',
                paste0('^', toupper(zone)))
  col_names = str_flatten(col_names, collapse = '|')
  df_zone = df[, grepl(col_names, colnames(df))]
  # write csv file
  output_path = paste0(output_dir, file_name, '_', zone, '.csv')
  write.csv(df_zone, output_path)
  # print '.csv' file name
  print(output_path)
}
# apply the function WriteZoneFile for all the interested zones
ApplyWZF = function(df, file_name, output_dir, zones) {
  lapply(zones, WriteZoneFile, df, file_name, output_dir)
}

# main function ####
# splits the full floor simulation's outputs in individual dwelings
SplitStorey = function(input_dir, output_dir, zones,
                       pattern = NULL, ign_zones = NULL) {
  # input_dir: directory where the full floor simulation's outputs are located
  # zone_names: names of the zones to be splitted (by default it pick up all the zones in the
    # standard model)
  # ign_zones: names of zones to be ignored (by default it ignores the 'core', 'cor' (corridor) and
    # 'bh' (bathroom))
  # output_dir: directory where the splitted files will be saved
  
  # number of available cores
  n_cores = detectCores()
  # load data
  files_paths = dir(input_dir, paste0('*', pattern, '.*csv'), full.names = T)
  dfs_list = mclapply(files_paths, LoadData, mc.cores = n_cores)
  # remove columns related to ignored zones
  if (!is.null(ign_zones)) {
    df = mclapply(dfs_list, RmIgnZone, ign_zones, mc.cores = n_cores)
  }
  # write files
  files_names = str_remove(basename(files_paths), '.csv')
  mcmapply(ApplyWZF, dfs_list, files_names, mc.cores = n_cores,
           MoreArgs = list(output_dir, zones))
}

# application ####
SplitStorey('~/in_progress/', '~/in_progress/split/',
            zones = c('sw_dorm_1', 'sw_liv', 'sw_dorm_2', 'se_dorm_2',
                      'se_liv', 'se_dorm_1', 'e_dorm_s', 'e_liv',
                      'e_dorm_n', 'ne_dorm_1', 'ne_liv', 'ne_dorm_2',
                      'nw_dorm_2', 'nw_liv', 'nw_dorm_1', 'w_dorm_n',
                      'w_liv', 'w_dorm_s'))