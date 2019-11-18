# load libraries
library(ggplot2)

# functions
# month_timestep()
  # define interval of timesteps for each month
month_timestep = function(timestep) {
  # timestep - number of timesteps per hour in the 'csv' (simulation) file
  # number of timesteps for each month
  end_month = c(31*24*timestep, (28+31)*24*timestep, (31+28+31)*24*timestep,
                (30+31+28+31)*24*timestep, (31+30+31+28+31)*24*timestep,
                (30+31+30+31+28+31)*24*timestep, (31+30+31+30+31+28+31)*24*timestep,
                (31+31+30+31+30+31+28+31)*24*timestep, (30+31+31+30+31+30+31+28+31)*24*timestep,
                (31+30+31+31+30+31+30+31+28+31)*24*timestep,
                (30+31+30+31+31+30+31+30+31+28+31)*24*timestep,
                (31+30+31+30+31+31+30+31+30+31+28+31)*24*timestep)
  # interval of timesteps for each month
  year = list('jan' = 1:end_month[1], 'feb' = (end_month[1] + 1):end_month[2],
              'mar' = (end_month[2] + 1):end_month[3], 'apr' = (end_month[3] + 1):end_month[4],
              'may' = (end_month[4] + 1):end_month[5], 'jun' = (end_month[5] + 1):end_month[6],
              'jul' = (end_month[6] + 1):end_month[7], 'ago' = (end_month[7] + 1):end_month[8],
              'sep' = (end_month[8] + 1):end_month[9], 'oct' = (end_month[9] + 1):end_month[10],
              'nov' = (end_month[10] + 1):end_month[11], 'dec' = (end_month[11] + 1):end_month[12])
  return(year)
}
# surf_rename()
  # rename the surfaces removing zone name and unecessary information
surf_rename = function(col_name) {
  surf = ifelse(grepl('FLOOR', col_name), '_floor',
                ifelse(grepl('ROOF', col_name), '_roof',
                       ifelse(grepl('WALL', col_name), '_wall',
                              ifelse(grepl('WINDOW', col_name), '_window',
                                     ifelse(grepl('DOOR', col_name), '_door', '')))))
  side = ifelse(grepl('_S\\.', col_name) & !grepl('M_S.', col_name), '_s',
                ifelse(grepl('_E\\.', col_name) & !grepl('M_E.', col_name), '_e',
                       ifelse(grepl('_N\\.', col_name) & !grepl('M_N.', col_name), '_n',
                              ifelse(grepl('_W\\.', col_name) & !grepl('M_W.', col_name), '_w',
                                     ''))))
  surf_rename = paste0(surf, side)
  return(surf_rename)
}

# report()
  # splits the 'csv' simulation report into data frames for each thermal fenom. and other metrics
    # and calculates thermal balance and other metrics monthly and annual
report = function(csv, timestep = 6, unit = 'kwh') {
  # csv - 'csv' simulation file from energyplus simulation
  # timestep - number of timesteps per hour in the 'csv' simulation file
  # unit - output's units
    # possible values: 'kwh' or 'kj'

  # test
  # csv = csv_files[[1]][[1]]
  # timestep = 6
  # unit = 'kwh'
  
  # define unites
  div = ifelse(unit == 'kwh', 3600000, 1000)
  # define months
  year = month_timestep(timestep)
  # thermal balance vectors
  int_conv_he = csv$int_conv_he/div
  conv_hge_floor = csv[, grepl('floor', colnames(csv)) & grepl('conv_hge', colnames(csv))]/div
  conv_hge_roof = csv[, grepl('roof', colnames(csv)) & grepl('conv_hge', colnames(csv))]/div
  conv_hge_walls = csv[, grepl('wall', colnames(csv)) & grepl('conv_hge', colnames(csv))]/div
  conv_hge_windows = csv[, grepl('window', colnames(csv)) & grepl('conv_hge', colnames(csv))]/div
  conv_hge_doors = csv[, grepl('door', colnames(csv)) & grepl('conv_hge', colnames(csv))]/div
  hvac_sens_he = csv$hvac_sens_he/div
  hvac_sens_ce = csv$hvac_sens_ce/div
  afn_inf_sens_hge = csv$afn_inf_sens_hge/div
  afn_inf_sens_hle = csv$afn_inf_sens_hle/div
  # other evaluation metrics vectors
  afn_inf_air_change = csv[, c('afn_inf_air_change', 'sch_afn')]
  hvac_total_he = csv$hvac_total_he/div
  hvac_total_ce = csv$hvac_total_ce/div
  # throw all the vectors inside the report list
  report = list('int_conv_he' = int_conv_he, 'conv_hge_floor' = conv_hge_floor,
                'conv_hge_roof' = conv_hge_roof, 'conv_hge_walls' = conv_hge_walls,
                'conv_hge_windows' = conv_hge_windows, 'conv_hge_doors' = conv_hge_doors,
                'hvac_sens_he' = hvac_sens_he, 'hvac_sens_ce' = hvac_sens_ce,
                'afn_inf_sens_hge' = afn_inf_sens_hge, 'afn_inf_sens_hle' = afn_inf_sens_hle,
                'afn_inf_air_change' = afn_inf_air_change, 'hvac_total_he' = hvac_total_he,
                'hvac_total_ce' = hvac_total_ce, 'df' = NULL)
  report[['df']] = as.data.frame(matrix(NA, 12, length(report) - 1))
  colnames(report[['df']]) = c('int_conv_he', 'conv_hge_floor', 'conv_hge_roof', 'conv_hge_walls',
                               'conv_hge_windows', 'conv_hge_doors', 'hvac_sens_he', 'hvac_sens_ce',
                               'afn_inf_sens_hge', 'afn_inf_sens_hle', 'afn_inf_air_change',
                               'hvac_total_he', 'hvac_total_ce')
  rownames(report[['df']]) = names(year)
  no_air_change = colnames(report[['df']]) != 'afn_inf_air_change'
  for (month in names(year)) {
    afn_month = afn_inf_air_change[year[[month]], ]
    report[['df']][month, 'afn_inf_air_change'] =
      mean(subset(afn_month, sch_afn != 0)$afn_inf_air_change)
    for (col in colnames(report[['df']])[no_air_change]) {
      report[['df']][month, col] = ifelse(is.null(dim(report[[col]])),
                                             sum(report[[col]][year[[month]]]),
                                             sum(apply(report[[col]][year[[month]], ], 2, sum)))
    }
  }
  report[['df']]['year', no_air_change] = apply(report[['df']][1:12, no_air_change], 2, sum)
  report[['df']]['year', 'afn_inf_air_change'] = mean(report[['df']][1:12, 'afn_inf_air_change'])
  report[['df']]$hvac_total_he = ifelse(report[['df']]$hvac_total_he < 0.01, 0,
                                        report[['df']]$hvac_total_he)
  report[['df']] = apply(report[['df']], 2, round, 1)
  return(report)
}

# with single zone results directory (first) and the directories of the real cases
input_dirs = list('sz' = '/home/rodox/Dropbox/00.master_ufsc/00.single_zone/01.validation/00.sz/01.result/',
                  'multi' = '/home/rodox/Dropbox/00.master_ufsc/00.single_zone/01.validation/01.multi/01.result/00.1st_multi/')

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
sz_cn = c('date_time', 'site_drybulb_temp', 'int_conv_he', 'occup_count', rep('conv_hge', 9),
          'mean_temp', 'op_temp', rep('afn_open_fac', 3), 'afn_inf_sens_hge', 'afn_inf_sens_hle',
          'afn_inf_air_change', 'hvac_sens_he', 'hvac_sens_ce', 'hvac_total_he', 'hvac_total_ce',
          'sch_afn', 'sch_hvac')
multi_dorm_cn = c('date_time', 'site_drybulb_temp', 'int_conv_he', 'occup_count',
                  rep('conv_hge', 8), 'mean_temp', 'op_temp', 'afn_open_fac', 'afn_inf_sens_hge',
                  'afn_inf_sens_hle', 'afn_inf_air_change', 'hvac_sens_he', 'hvac_sens_ce',
                  'hvac_total_he', 'hvac_total_ce', 'sch_afn', 'sch_hvac')
multi_ew_liv_cn = c('date_time', 'site_drybulb_temp', 'int_conv_he', 'occup_count',
                    rep('conv_hge', 9), 'mean_temp', 'op_temp', rep('afn_open_fac', 3),
                    'afn_inf_sens_hge', 'afn_inf_sens_hle', 'afn_inf_air_change', 'hvac_sens_he',
                    'hvac_sens_ce', 'hvac_total_he', 'hvac_total_ce', 'sch_afn', 'sch_hvac')
multi_sn_liv_cn = c('date_time', 'site_drybulb_temp', 'int_conv_he', 'occup_count',
                    rep('conv_hge', 10), 'mean_temp', 'op_temp', rep('afn_open_fac', 3),
                    'afn_inf_sens_hge', 'afn_inf_sens_hle', 'afn_inf_air_change', 'hvac_sens_he',
                    'hvac_sens_ce', 'hvac_total_he', 'hvac_total_ce', 'sch_afn', 'sch_hvac')

# rename columns
for (i in 1:length(csv_files$sz)) {
  # remove first column related to an x variable created when multi 'csv' files were splitted
  csv_files$multi[[i]][, 1] = NULL
  # multi
  for (j in 1:dim(csv_files$multi[[i]])[2]) {
    col = colnames(csv_files$multi[[i]])[j]
    col = ifelse(
      grepl('dorm', names(csv_files$multi)[i]),
      paste0(multi_dorm_cn[j], surf_rename(col)), 
      ifelse(
        grepl('_e_living',names(csv_files$multi)[i]) | grepl('_w_living',
                                                             names(csv_files$multi)[i]),
        paste0(multi_ew_liv_cn[j], surf_rename(col)),
        paste0(multi_sn_liv_cn[j], surf_rename(col))
      )
    )
    colnames(csv_files$multi[[i]])[j] = col
  }
    # single zone
    for (j in 1:dim(csv_files$sz[[i]])[2]) {
    col = colnames(csv_files$sz[[i]])[j]
    col = paste0(sz_cn[j], surf_rename(col))
    colnames(csv_files$sz[[i]])[j] = col
  }
}

# configure 'date_time' colum
for (i in 1:length(csv_files)) {
  for (j in 1:length(csv_files[[i]])) {
    csv_files[[i]][[j]]$date_time = seq(ISOdate(19, 1, 1, 0, 10, 0), by = '10 min',
                                        length.out = 365*24*6, tz='')
  }
}

# compile the results
for (i in 1:length(csv_files)) {
  for (j in 1:length(csv_files[[i]])) {

  }
}

