# load libraries
library(ggplot2)

# with single zone results directory (first) and the directories of the real cases
input_dirs = list('sz' = '/home/rodox/00.git/00.master_ufsc/00.single_zone/03.validation/00.sz/01.result/',
                  'multi' = '/home/rodox/00.git/00.master_ufsc/00.single_zone/03.validation/01.multi/01.result/00.1st_multi/')

# create empty lists to be filled with 'csv' files
csv_names = csv_files = results = vector('list', length = length(input_dirs))
# name the lists
names(csv_names) = names(csv_files) = names(results) = names(input_dirs)

# pick 'csv' names inside input directory
for (i in 1:length(csv_names)) {
  csv_names[[i]] = dir(input_dirs[[i]], '.csv')
  # extend results
  results[[i]] = vector('list', length(csv_names[[i]]))
}

# extend and name results
for (i in 1:length(csv_names)) {
  
}

# load files
for (i in 1:length(csv_names)) {
  for (j in 1:length(csv_names[[i]])) {
    # count the files while they're loaded
    print(paste('i =', i, '/ j =', j))
    # load the files themselves
    csv_files[[i]][[j]] = read.csv(paste0(input_dirs[[i]], csv_names[[i]][[j]]))
  }
  # define proper names to the list
  names(csv_files[[i]]) = names(results[[i]]) = sub('.csv', '', csv_names[[i]])
}

# delete columns related to the hives
csv_files$sz = lapply(csv_files$sz, function(x) x[, grepl('CORE', colnames(x)) |
                                                    grepl('Date.Time', colnames(x)) |
                                                    grepl('Drybulb', colnames(x))])

# rename column names
sz_cn = c('date_time', 'site_drybulb_temp', 'int_conv_he', 'occup_count', 'conv_hge_surf_1',
          'conv_hge_surf_2', 'conv_hge_surf_3', 'conv_hge_surf_4', 'conv_hge_surf_5',
          'conv_hge_surf_6', 'conv_hge_surf_7', 'conv_hge_surf_8', 'conv_hge_surf_9',
          'mean_temp', 'op_temp', 'afn_vent_door_1', 'afn_vent_door_2', 'afn_vent_window',
          'afn_inf_sens_hge', 'afn_inf_sens_hle', 'afn_inf_air_change', 'hvac_sens_he',
          'hvac_sens_ce', 'hvac_total_he', 'hvac_total_ce', 'sch_afn', 'sch_hvac')
multi_dorm_cn = c('date_time', 'site_drybulb_temp', 'int_conv_he', 'occup_count', 'conv_hge_surf_1',
                  'conv_hge_surf_2', 'conv_hge_surf_3', 'conv_hge_surf_4', 'conv_hge_surf_5',
                  'conv_hge_surf_6', 'conv_hge_surf_7', 'conv_hge_surf_8', 'mean_temp', 'op_temp',
                  'afn_vent_window', 'afn_inf_sens_hge', 'afn_inf_sens_hle', 'afn_inf_air_change',
                  'hvac_sens_he', 'hvac_sens_ce', 'hvac_total_he', 'hvac_total_ce', 'sch_afn', 'sch_hvac')
multi_ew_liv_cn = c('date_time', 'site_drybulb_temp', 'int_conv_he', 'occup_count', 'conv_hge_surf_1',
                    'conv_hge_surf_2', 'conv_hge_surf_3', 'conv_hge_surf_4', 'conv_hge_surf_5',
                    'conv_hge_surf_6', 'conv_hge_surf_7', 'conv_hge_surf_8', 'conv_hge_surf_9',
                    'mean_temp', 'op_temp', 'afn_vent_window', 'afn_vent_door_1',
                    'afn_vent_door_2', 'afn_inf_sens_hge', 'afn_inf_sens_hle', 'afn_inf_air_change',
                    'hvac_sens_he', 'hvac_sens_ce', 'hvac_total_he', 'hvac_total_ce', 'sch_afn',
                    'sch_hvac')
multi_sn_liv_cn = c('date_time', 'site_drybulb_temp', 'int_conv_he', 'occup_count', 'conv_hge_surf_1',
                    'conv_hge_surf_2', 'conv_hge_surf_3', 'conv_hge_surf_4', 'conv_hge_surf_5',
                    'conv_hge_surf_6', 'conv_hge_surf_7', 'conv_hge_surf_8', 'conv_hge_surf_9',
                    'conv_hge_surf_10', 'mean_temp', 'op_temp', 'afn_vent_window', 'afn_vent_door_1',
                    'afn_vent_door_2', 'afn_inf_sens_hge', 'afn_inf_sens_hle', 'afn_inf_air_change',
                    'hvac_sens_he', 'hvac_sens_ce', 'hvac_total_he', 'hvac_total_ce', 'sch_afn', 'sch_hvac')

# rename columns
for (i in 1:length(csv_files$sz)) {
  # single zone
  colnames(csv_files$sz[[i]]) = sz_cn
  # remove first column related to an x variable created when multi 'csv' files were splitted
  csv_files$multi[[i]][, 1] = NULL
  # multi
  colnames(csv_files$multi[[i]]) = ifelse(
    grepl('dorm', names(csv_files$multi)[i]),
    multi_dorm_cn, ifelse(
      grepl('_e_living', names(csv_files$multi)[i]) | grepl('_w_living', names(csv_files$multi)[i]),
      multi_ew_liv_cn, multi_sn_liv_cn
    )
  )
}

# compile the results
for (i in 1:length(csv_files)) {
  for (j in 1:length(csv_files[[i]])) {
    results[[i]][[j]][['thermal_loads']] = c('cooling' = sum(csv_files[[i]][[j]]$hvac_total_ce)/3600000,
                                             'heating' = sum(csv_files[[i]][[j]]$hvac_total_ce)/3600000)
    results[[i]][[j]]['thermal_balance']
  }
}


month_thermal_balance = function() {
  
}


rownames(csv_files[[1]][[3]]) = seq(ISOdate(19, 1, 1, 0, 10, 0), by='10 min',
                                    length.out = 365*24*6, tz='')
  
