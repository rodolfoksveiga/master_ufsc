# the split_floor function splits the full floor simulation's outputs in individual dwelings

# libraries
library('dplyr')
library('Hmisc')
library('stringr')

# split_floor()
# input_dir = directory where the full floor simulation's outputs are located
# wrap_names = types of possible wraps to be considered
# zone_names = names of the zones to be splitted (by default it pick up all the zones in the standard model)
# ignored_zones = names of zones to be ignored (by default it ignores the 'core', 'cor' (corridor) and 'bh' (bathroom))
# output_dir = directory where the splitted files will be saved
split_floor = function(input_dir,
                       wrap_names,
                       zone_names,
                       ignored_zones = NULL,
                       output_dir) {
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
      print(paste0('i = ', i, ' / j = ', j))
      # load files
      csv_files[[i]][[j]] = read.csv(paste0(input_dir, csv_names[[i]][j]))
      # rename the list which contains the '.csv' files (remove '.csv' suffix)
      names(csv_files[[i]])[j] = str_remove(csv_names[[i]][j], '.csv')
      # remove columns related to ignored zones
      for (ign in ignored_zones) {
        csv_files[[i]][[j]][, grepl(toupper(ign), colnames(csv_files[[i]][[j]]))] = NULL
      }
      # split the floor in zones
      for (zone in zone_names) {
        # select the columns related to each interested zone
        df = csv_files[[i]][[j]][, grepl('Date.Time', colnames(csv_files[[i]][[j]])) |
                                   grepl('Environment.Site.Outdoor.Air.Drybulb.Temperature..C..TimeStep.',
                                         colnames(csv_files[[i]][[j]])) |
                                   grepl(paste0('^', toupper(zone)), colnames(csv_files[[i]][[j]])) |
                                   grepl(paste0('_', toupper(zone)), colnames(csv_files[[i]][[j]]))]
        # save '.csv' file for each zone
        write.csv(df, paste0(output_dir, names(csv_files[[i]])[j], '_', zone, '.csv'))
        # print '.csv' file name
        print(paste0(names(csv_files[[i]])[j], '_', zone, '.csv'))
      }
    }
  }
}

# application
split_floor(input_dir = '/home/rodox/Dropbox/TEMP/00.single_zone/03.validation/01.multi/01.result/',
            wrap_names = 'tv',
            zone_names = c('sw_dorm_w', 'sw_living', 'sw_dorm_e', 'se_dorm_w', 'se_living',
                           'se_dorm_e', 'e_dorm_s', 'e_living', 'e_dorm_n', 'ne_dorm_e',
                           'ne_living', 'ne_dorm_w', 'nw_dorm_e', 'nw_living', 'nw_dorm_w',
                           'w_dorm_n', 'w_living', 'w_dorm_s'),
            output_dir = '/home/rodox/Dropbox/TEMP/00.single_zone/03.validation/01.multi/01.result/00.1st_multi/')
