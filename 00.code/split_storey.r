# the split_floor function splits the full floor simulation's outputs in individual dwelings

# libraries
library('dplyr')
library('Hmisc')
library('stringr')

# split_floor()
split_storey = function(input_dir, wrap_names = '', zone_names, ignored_zones = NULL, output_dir) {
  # input_dir: directory where the full floor simulation's outputs are located
  # wrap_names: types of possible wraps to be considered
  # zone_names: names of the zones to be splitted (by default it pick up all the zones in the
    # standard model)
  # ignored_zones: names of zones to be ignored (by default it ignores the 'core', 'cor' (corridor)
    # and 'bh' (bathroom))
  # output_dir: directory where the splitted files will be saved

  # create empty lists
  csv_names = csv_files = vector(mode = 'list',
                                 length = length(wrap_names))
  # name the list which contains the '.csv' files
  names(csv_names) = names(csv_files) = wrap_names
  # name '.csv' files
  for (i in 1:length(wrap_names)) {
    csv_names[[i]] = dir(input_dir, paste0(names(csv_names)[i], '.csv'))
  }
  # load, process, separet dwelings in zones and write the .csv files
  df = data.frame()
  for (i in 1:length(csv_names)) {
    for (j in 1:length(csv_names[[i]])) {
      # count the splitting process
      print(paste('i =', i, '/ j =', j))
      # load files
      csv_files[[i]][[j]] = as.data.frame(fread(paste0(input_dir, csv_names[[i]][j])))
      # rename the list which contains the '.csv' files (remove '.csv' suffix)
      names(csv_files[[i]])[j] = str_remove(csv_names[[i]][j], '.csv')
      # remove columns related to ignored zones
      if (!is.null(ignored_zones)) {
        for (ign in ignored_zones) {
          csv_files[[1]][[1]][, grepl(toupper(ign), colnames(csv_files[[1]][[1]]))] = NULL
        }
      }
      # split the floor in zones
      for (zone in zone_names) {
        # select the columns related to each interested zone
        df = csv_files[[i]][[j]][, grepl('Date/Time', colnames(csv_files[[i]][[j]])) |
                                   grepl('Environment:Site Outdoor Air Drybulb',
                                         colnames(csv_files[[i]][[j]])) |
                                   grepl(paste0('^', toupper(zone)),
                                         colnames(csv_files[[i]][[j]])) |
                                   grepl(paste0('_', toupper(zone)), colnames(csv_files[[i]][[j]]))]
        # save '.csv' file for each zone
        write.csv(df, paste0(output_dir, names(csv_files[[i]])[j], '_', zone, '.csv'))
        # print '.csv' file name
        print(paste0(names(csv_files[[i]])[j], '_', zone, '.csv'))
      }
    }
  }
}

# application ####
# v00
wraps = c('c10', 'tv', 'sf')
storeys = c('floor', 'inter', 'roof')
m = 0
for (i in 1:length(wraps)) {
  n = 0
  print(toupper(wraps[i]))
  for (storey in storeys) {
    split_storey(input_dir = paste0('/home/rodox/01.going_on/00.hive/00.hyp/00/0', m, '.', wraps[i],
                                    '/0', n, '.', storey, '/'),
                 zone_names = c('sw_dorm_1', 'sw_liv', 'sw_dorm_2', 'se_dorm_2', 'se_liv', 'se_dorm_1',
                                'e_dorm_s', 'e_liv', 'e_dorm_n', 'ne_dorm_1', 'ne_liv', 'ne_dorm_2',
                                'nw_dorm_2', 'nw_liv', 'nw_dorm_1', 'w_dorm_n', 'w_liv', 'w_dorm_s'),
                 output_dir = paste0('/home/rodox/01.going_on/00.hive/00.hyp/00/0', m, '.', wraps[i],
                                     '/0', n, '.', storey, '/00.split/'))
    n = n + 1
  }
  m = m + 1
}

# v01
wraps = c('c10', 'tv', 'sf')
storeys = c('floor', 'inter', 'roof')
m = 0
for (i in 1:length(wraps)) {
  n = 0
  print(toupper(wraps[i]))
  for (storey in storeys) {
    split_storey(input_dir = paste0('/home/rodox/01.going_on/00.hive/00.hyp/01/0', m, '.', wraps[i],
                                    '/0', n, '.', storey, '/'),
                 zone_names = c('sw_dorm_1', 'sw_liv', 'sw_dorm_2', 'se_dorm_2', 'se_liv', 'se_dorm_1',
                                'e_dorm_s', 'e_liv', 'e_dorm_n', 'ne_dorm_1', 'ne_liv', 'ne_dorm_2',
                                'nw_dorm_2', 'nw_liv', 'nw_dorm_1', 'w_dorm_n', 'w_liv', 'w_dorm_s'),
                 output_dir = paste0('/home/rodox/01.going_on/00.hive/00.hyp/01/0', m, '.', wraps[i],
                                     '/0', n, '.', storey, '/00.split/'))
    n = n + 1
  }
  m = m + 1
}

# v02
wraps = c('c10', 'tv', 'sf')
storeys = c('floor', 'inter', 'roof')
m = 0
for (i in 1:length(wraps)) {
  n = 0
  print(toupper(wraps[i]))
  for (storey in storeys) {
    split_storey(input_dir = paste0('/home/rodox/01.going_on/00.hive/00.hyp/02/0', m, '.', wraps[i],
                                    '/0', n, '.', storey, '/'),
                 zone_names = c('sw_dorm_1', 'sw_liv', 'sw_dorm_2', 'se_dorm_2', 'se_liv', 'se_dorm_1',
                                'e_dorm_s', 'e_liv', 'e_dorm_n', 'ne_dorm_1', 'ne_liv', 'ne_dorm_2',
                                'nw_dorm_2', 'nw_liv', 'nw_dorm_1', 'w_dorm_n', 'w_liv', 'w_dorm_s'),
                 output_dir = paste0('/home/rodox/01.going_on/00.hive/00.hyp/02/0', m, '.', wraps[i],
                                     '/0', n, '.', storey, '/00.split/'))
    n = n + 1
  }
  m = m + 1
}
