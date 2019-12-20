# main function ####
# split_storey()
# splits the full floor simulation's outputs in individual dwelings
split_storey = function(input_dir, pattern = NULL, zone_names, ignored_zones = NULL, output_dir) {
  # input_dir - directory where the full floor simulation's outputs are located
  # zone_names - names of the zones to be splitted (by default it pick up all the zones in the
    # standard model)
  # ignored_zones - names of zones to be ignored (by default it ignores the 'core', 'cor' (corridor)
    # and 'bh' (bathroom))
  # output_dir - directory where the splitted files will be saved
  
  # name '.csv' files
  csv_names = dir(input_dir, paste0('*', pattern, '.*csv'))
  # load, process, separet dwelings in zones and write the .csv files
  for (i in 1:length(csv_names)) {
    # count the splitting process
    print(paste('i =', i))
    # load files
    df = as.data.frame(data.table::fread(paste0(input_dir, csv_names[i])))
    # rename the list which contains the '.csv' files (remove '.csv' suffix)
    csv_names[i] = stringr::str_remove(csv_names[i], '.csv')
    # remove columns related to ignored zones
    if (!is.null(ignored_zones)) {
      for (ign in ignored_zones) {
        df[, grepl(toupper(ign), colnames(df))] = NULL
      }
    }
    # split the floor in zones
    for (zone in zone_names) {
      # select the columns related to each interested zone
      df_zone = df[, grepl('Date/Time', colnames(df)) |
                     grepl('Environment:Site Outdoor Air Drybulb', colnames(df)) |
                     grepl(paste0('^', toupper(zone)), colnames(df)) |
                     grepl(paste0('_', toupper(zone)), colnames(df))]
      # save '.csv' file for each zone
      file_path = paste0(output_dir, csv_names[i], '_', zone, '.csv')
      write.csv(df_zone, file_path)
      # print '.csv' file name
      print(file_path)
    }
  }
}

# application ####
simps = c('02')
wraps = c('c10', 'tv', 'sf')
storeys = c('floor', 'inter', 'roof')
conds = c('afn')
for (simp in simps) {
  m = 0
  for (wrap in wraps) {
    n = 0
    for (storey in storeys) {
      o = 0
      for (cond in conds) {
        print(paste(simp, '/', toupper(wrap), '/', toupper(cond)))
        split_storey(input_dir = c(paste0('/home/rodox/01.going_on/00.hive/00.hyp/', simp,
                                          '/0', m, '.', wrap, '/0', n, '.', storey, '/0',
                                          o, '.', cond, '/')),
                     pattern = '',
                     zone_names = c('sw_dorm_1', 'sw_liv', 'sw_dorm_2', 'se_dorm_2', 'se_liv',
                                    'se_dorm_1', 'e_dorm_s', 'e_liv', 'e_dorm_n', 'ne_dorm_1',
                                    'ne_liv', 'ne_dorm_2', 'nw_dorm_2', 'nw_liv', 'nw_dorm_1',
                                    'w_dorm_n', 'w_liv', 'w_dorm_s'),
                     ignored_zones = NULL,
                     output_dir = c(paste0('/home/rodox/01.going_on/00.hive/00.hyp/', simp,
                                           '/0', m, '.', wrap, '/0', n, '.', storey, '/0',
                                           o, '.', cond, '/')))
        gc()
        o = o + 1
      }
      n = n + 1
    }
    m = m + 1
  }
}
