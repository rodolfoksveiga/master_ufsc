# main function ####
# split_storey()
# splits the hive simulation's outputs into only the hive core
split_hive = function(input_dir, pattern = NULL, zone_name = 'hive_c', output_dir) {
  # input_dir - directory where the full floor simulation's outputs are located
  # pattern - pattern to be found inside input_dir
  # zone_name - name of the zone to be splitted
  # output_dir - directory where the splitted files will be saved
  
  # name '.csv' files
  csv_names = dir(input_dir, paste0('*', pattern, '.*csv'))
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
                   grepl(toupper(zone_name), colnames(df))]
    # save '.csv' file for each zone
    file_path = paste0(output_dir, csv_names[i])
    write.csv(df_zone, file_path)
    # print '.csv' file name
    print(file_path)
  }
}

# application ####
# simps = c('05', '06', '07', '08', '09', '10')
simps = '10'
wraps = c('c10', 'tv', 'sf')
storeys = c('floor', 'inter', 'roof')
# conds = c('afn', 'hvac')
conds = 'afn'
for (simp in simps) {
  m = 0
  for (wrap in wraps) {
    n = 0
    for (storey in storeys) {
      o = 0
      for (cond in conds) {
        print(paste(simp, '/', toupper(wrap), '/', toupper(storey), '/', toupper(cond)))
        split_hive(input_dir = paste0('/home/rodox/01.going_on/00.hive/0', m, '.', typo,
                                      '/', simp, '/0', n, '.', wrap, '/0', o, '.', storey,
                                      '/00.afn/'),
                   pattern = paste0('hyp_', wrap, '_v', simp, '_', storey, '_', cond),
                   zone_name = ifelse(simp != '08' & simp != '09' & simp != '10', 'hive_c',
                                      ifelse(storey == 'floor', 'hive_bc',
                                             ifelse(storey == 'inter', 'hive_mc', 'hive_tc'))),
                   output_dir = paste0('/home/rodox/01.going_on/00.hive/00.hyp/', simp, '/split/'))
        gc()
        o = o + 1
      }
      n = n + 1
    }
    m = m + 1
  }
}
