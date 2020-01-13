# main function ####
# split_building ()
  # splits the full simulation output in individual storeys
split_building = function(input_dir, pattern = NULL, storey_id, output_dir) {
  # input_dir - directory where the full floor simulation's outputs are located
  # pattern - pattern to be found inside the input directory
  # storey_id - list with the code of the storeys to be splitted and their correspondent storeys
    # e.g.: storey_id = list(c('F1', 'F3', 'F5'), c('floor', 'inter', 'roof'))
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
    # split the floor in zones
    for (j in 1:length(storey_id[[1]])) {
      # select the columns related to each interested storey
      df_storey = df[, grepl('Date/Time', colnames(df)) |
                     grepl('Environment:Site Outdoor Air Drybulb', colnames(df)) |
                     grepl(toupper(storey_id[[1]][j]), colnames(df))]
      # save '.csv' file for each storey
      if (grepl('afn', csv_names[i])) {
        file_path = paste0(output_dir, sub('_afn', '', csv_names[i]),
                           '_', storey_id[[2]][j], '_afn.csv')
      } else if (grepl('hvac', csv_names[i])) {
        file_path = paste0(output_dir, sub('_hvac', '', csv_names[i]),
                           '_', storey_id[[2]][j], '_hvac.csv')
      }
      write.csv(df_storey, file_path)
      # print '.csv' file name
      print(file_path)
    }
  }
}

# application ####
simps = c('00', '01')
wraps = c('c10', 'tv', 'sf')
conds = c('hvac')
for (simp in simps) {
  m = 0
  for (wrap in wraps) {
    for (cond in conds) {
      print(paste(simp, '/', toupper(wrap), '/', toupper(cond)))
      split_building(input_dir = paste0('/media/rodox/HD_EXTERNO/00.hive/00.hyp/', simp, '/0', m,
                                        '.', wrap, '/'),
                     pattern = cond,
                     storey_id = list(c('F1', 'F3', 'F5'), c('floor', 'inter', 'roof')),
                     output_dir = paste0('/media/rodox/HD_EXTERNO/00.hive/00.hyp/', simp, '/0', m,
                                         '.', wrap, '/'))
    }
    m = m + 1
  }
}


