# main function ####
# split_storey()
# splits the hive simulation's outputs into only the hive core
split_zone = function(input_dir, pattern = NULL, zone_names, output_dir) {
  # input_dir - directory where the full floor simulation's outputs are located
  # pattern - pattern to be found inside input_dir
  # zone_names - names of all the possible zones to be splitted
  # output_dir - directory where the splitted files will be saved
  
  # name '.csv' files
  csv_names = dir(input_dir, paste0('*', pattern, '.*csv'))
  names = rep(NA, length(csv_names))
  for (i in 1:length(csv_names)) {
    for (name in zone_names) {
      if (grepl(paste0('_', name), csv_names[i])) {
        names[i] = name
      }
    }
  }
  
  # load, process, separet dwelings in zone and write the .csv files
  for (i in 1:length(csv_names)) {
    # count the splitting process
    print(paste('i =', i))
    # load files
    df = as.data.frame(data.table::fread(paste0(input_dir, csv_names[i])))
    # filter the output file according to the input 'zone name'
    # select the columns related to 'zone names'
    df_zone = df[, grepl('Date/Time', colnames(df)) |
                   grepl('Environment:Site Outdoor Air Drybulb', colnames(df)) |
                   grepl(toupper(names[i]), colnames(df))]
    # save '.csv' file for each zone
    file_path = paste0(output_dir, csv_names[i])
    write.csv(df_zone, file_path)
    # print '.csv' file name
    print(file_path)
  }
}

# application ####
zone_names = c('sw_dorm_1', 'sw_liv', 'sw_dorm_2', 'se_dorm_2', 'se_liv', 'se_dorm_1', 'e_dorm_s',
                'e_liv', 'e_dorm_n', 'ne_dorm_1', 'ne_liv', 'ne_dorm_2', 'nw_dorm_2', 'nw_liv',
                'nw_dorm_1', 'w_dorm_n', 'w_liv', 'w_dorm_s')
simps = c('04')
wraps = c('c10', 'tv', 'sf')
storeys = c('floor', 'inter', 'roof')
conds = c('afn', 'hvac')
for (simp in simps) {
  m = 0
  for (wrap in wraps) {
    n = 0
    for (storey in storeys) {
      o = 0
      for (cond in conds) {
        print(paste(simp, '/', toupper(wrap), '/', toupper(storey), '/', toupper(cond)))
        split_zone(input_dir = paste0('/home/rodox/01.going_on/00.hive/00.hyp/', simp,
                                      '/0', m, '.', wrap, '/0', n, '.', storey, '/0',
                                      o, '.', cond, '/'),
                   zone_names = zone_names,
                   pattern = paste0('hyp_', wrap, '_v', simp, '_', storey, '_', cond),
                   output_dir = paste0('/home/rodox/01.going_on/00.hive/00.hyp/', simp,
                                       '/0', m, '.', wrap, '/0', n, '.', storey, '/0',
                                       o, '.', cond, '/'))
        gc()
        o = o + 1
      }
      n = n + 1
    }
    m = m + 1
  }
}

  
