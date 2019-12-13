# load libraries ####
library(ggplot2)

# base functions ####
# cap_str()
# capitalize all the words in a string
cap_str = function(str) {
  # str - string to capitalize
  
  spl = strsplit(str, ' ')[[1]]
  cap_spl = Hmisc::capitalize(spl)
  cap_str = cap_spl[1]
  for (s in cap_spl[-1]) {
    cap_str = paste(cap_str, s)
  }
  return(cap_str)
}

# diff()
# calculate difference of results from simplified simulations (single zone) to full simulations
df_diff = function(df_simp, df_base, df_area) {
  # df_simp - data frame with single zone results
  # df_base - data frame with full simulation results
  
  # calculate absolute difference
  # formula: diff_abs = (val_simp - val_base) / floor_area
  df_diff_abs = df_simp[, is_label(df_simp)[[2]]] - df_base[, is_label(df_base)[[2]]]
  # calculate relative difference
  # formula: diff_rel = (val_simp - val_base) / val_base
  df_diff_rel = df_diff_abs*100 / abs(df_base[, is_label(df_base)[[2]]])
  # add labels to the data frame
  df_diff_abs = cbind(df_diff_abs, df_simp[, is_label(df_simp)[[1]]])
  df_diff_rel = cbind(df_diff_rel, df_simp[, is_label(df_simp)[[1]]])
  return(list('abs' = df_diff_abs, 'rel' = df_diff_rel))
}

# isnt_label()
is_label = function(df) {
  # df - data frame with the columns to destinguish
  
  # yes, it is!
  yes_label = grepl('dwel', colnames(df)) | grepl('room', colnames(df)) |
    grepl('weather', colnames(df))
  # no, it is not!
  no_label = !grepl('sim', colnames(df)) & !grepl('dwel', colnames(df)) &
    !grepl('room', colnames(df)) & !grepl('weather', colnames(df))
  return(list(yes_label, no_label))
}

# label_df()
# label data frames according to dweling, room, front and weather
label_df = function(df, file_name) {
  df$dwel = ifelse(grepl('_sw_', file_name), 'SW',
                   ifelse(grepl('_se_', file_name), 'SE',
                          ifelse(grepl('_e_', file_name), 'E',
                                 ifelse(grepl('_ne_', file_name), 'NE',
                                        ifelse(grepl('_nw_', file_name), 'NW', 'W')))))
  side = ifelse(grepl('rm_s', file_name), 'S',
                ifelse(grepl('rm_n', file_name), 'N',
                       ifelse(grepl('rm_1', file_name), '1', '2')))
  df$room = ifelse(grepl('liv', file_name), 'Living', paste('Dorm.', side))
  df$weather = ifelse(grepl('rio_de_janeiro', file_name), 'Rio de Janeiro', 'São Paulo')
  return(df)
}

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

# report()
# splits the 'csv' simulation report in data frames for each thermal fenom. and other metrics and
# calculates thermal balance and other metrics monthly and annual
report = function(csv, timestep = 6, unit = 'kwh') {
  # csv - 'csv' simulation file from energyplus simulation
  # timestep - number of timesteps per hour in the 'csv' simulation file
  # unit - output's units
  # possible values: 'kwh' or 'kj'
  
  # define unites
  div = ifelse(unit == 'kwh', 3600000, 1000)
  # define months
  year = month_timestep(timestep)
  # thermal balance vectors
  int_conv_he = csv$int_conv_he/div
  conv_hge_floor = -csv[, grepl('conv_hge', colnames(csv)) & grepl('floor', colnames(csv))]/div
  conv_hge_roof = -csv[, grepl('conv_hge', colnames(csv)) & grepl('roof', colnames(csv))]/div
  conv_hge_wall_s = -csv[, grepl('conv_hge', colnames(csv)) & grepl('wall_s', colnames(csv))]/div
  conv_hge_wall_e = -csv[, grepl('conv_hge', colnames(csv)) & grepl('wall_e', colnames(csv))]/div
  conv_hge_wall_n = -csv[, grepl('conv_hge', colnames(csv)) & grepl('wall_n', colnames(csv))]/div
  conv_hge_wall_w = -csv[, grepl('conv_hge', colnames(csv)) & grepl('wall_w', colnames(csv))]/div
  conv_hge_walls = -csv[, grepl('conv_hge', colnames(csv)) & grepl('wall', colnames(csv))]/div
  conv_hge_windows = -csv[, grepl('conv_hge', colnames(csv)) & grepl('window', colnames(csv))]/div
  conv_hge_doors = -csv[, grepl('conv_hge', colnames(csv)) & grepl('door', colnames(csv))]/div
  hvac_sens_he = csv$hvac_sens_he/div
  hvac_sens_ce = -csv$hvac_sens_ce/div
  afn_inf_sens_hge = csv$afn_inf_sens_hge/div
  afn_inf_sens_hle = -csv$afn_inf_sens_hle/div
  # other evaluation metrics vectors
  hvac_total_he = csv$hvac_total_he/div
  hvac_total_ce = csv$hvac_total_ce/div
  # throw all the vectors inside the report list
  report = list('int_conv_he' = int_conv_he, 'conv_hge_floor' = conv_hge_floor,
                'conv_hge_roof' = conv_hge_roof, 'conv_hge_wall_s' = conv_hge_wall_s,
                'conv_hge_wall_e' = conv_hge_wall_e, 'conv_hge_wall_n' = conv_hge_wall_n,
                'conv_hge_wall_w' = conv_hge_wall_w, 'conv_hge_walls' = conv_hge_walls,
                'conv_hge_windows' = conv_hge_windows, 'conv_hge_doors' = conv_hge_doors,
                'hvac_sens_he' = hvac_sens_he, 'hvac_sens_ce' = hvac_sens_ce,
                'afn_inf_sens_hge' = afn_inf_sens_hge, 'afn_inf_sens_hle' = afn_inf_sens_hle,
                'hvac_total_he' = hvac_total_he, 'hvac_total_ce' = hvac_total_ce, 'df' = NULL)
  # create data frame
  # the data frame number of columns is 1 size smaller than the length of report because 1 item of
  # report's list correspond to 'df'
  report[['df']] = as.data.frame(matrix(NA, 12, length(report) - 1))
  colnames(report[['df']]) = c('int_conv_he', 'conv_hge_floor', 'conv_hge_roof', 'conv_hge_walls',
                               'conv_hge_wall_s', 'conv_hge_wall_e', 'conv_hge_wall_n',
                               'conv_hge_wall_w', 'conv_hge_windows', 'conv_hge_doors',
                               'hvac_sens_he', 'hvac_sens_ce', 'afn_inf_sens_hge',
                               'afn_inf_sens_hle', 'hvac_total_he', 'hvac_total_ce')
  rownames(report[['df']]) = names(year)
  for (month in names(year)) {
    for (col in colnames(report[['df']])) {
      report[['df']][month, col] = ifelse(is.null(dim(report[[col]])),
                                          sum(report[[col]][year[[month]]]),
                                          sum(apply(report[[col]][year[[month]], ], 2, sum)))
    }
  }
  report[['df']]['year', ] = apply(report[['df']][1:12, ], 2, sum)
  report[['df']]$hvac_total_he = ifelse(report[['df']]$hvac_total_he < 0.01, 0,
                                        report[['df']]$hvac_total_he)
  report[['df']] = apply(report[['df']], 2, round, 1)
  report[['df']] = cbind(report[['df']], data.frame('sim' = NA, 'dwel' = NA, 'room' = NA,
                                                    'weather' = NA))
  return(report)
}

# surf_rename()
# rename the surfaces removing zone name and unecessary information
surf_rename = function(col_name) {
  # col_name - name of the surface column name to rename
  
  surf = ifelse(grepl('FLOOR', col_name), '_floor',
                ifelse(grepl('ROOF', col_name), '_roof',
                       ifelse(grepl('WALL', col_name), '_wall',
                              ifelse(grepl('WINDOW', col_name), '_window',
                                     ifelse(grepl('DOOR', col_name), '_door', '')))))
  side = ifelse(grepl('_S\\.', col_name) & !grepl('M_S\\.', col_name), '_s',
                ifelse(grepl('_E\\.', col_name), '_e',
                       ifelse(grepl('_N\\.', col_name) & !grepl('M_N\\.', col_name), '_n',
                              ifelse(grepl('_W\\.', col_name), '_w',
                                     ''))))
  surf_rename = paste0(surf, side)
  return(surf_rename)
}

# main function ####
valid = function(input_dirs, cond, storey, version_base, version_simp, df_area) {

  # create empty lists to be filled with 'csv' files
  csv_names = csv_files = results = vector('list', length(input_dirs))
  # name the lists
  names(csv_names) = names(csv_files) = names(results) = names(input_dirs)
  
  # load files ####
  # pick 'csv' names inside input directory
  for (i in 1:length(csv_names)) {
    csv_names[[i]] = dir(input_dirs[[i]], '.csv')
    # extend results
    results[[i]] = vector('list', length(csv_names[[i]]))
  }
  
  # read files
  # data frame with zone areas
  df_area = read.csv(df_area)
  # multiply data frace twice, because there is two weathers
  df_area = rbind(df_area, df_area, df_area)
  # 'csv' files from simulations
  for (i in 1:length(csv_names)) {
    for (j in 1:length(csv_names[[i]])) {
      # count the files while they're loaded
      print(paste('i =', i, '/ j =', j))
      # load the files themselves
      csv_files[[i]][[j]] = read.csv(paste0(input_dirs[[i]], csv_names[[i]][[j]]))
      # define proper names to the list
      if (i == 1) {
        names(csv_files[[i]])[j] = ifelse(cond == 'hvac',
                                       sub(paste0('_', version_base, '_v00_', storey), '',
                                           sub('.csv', '', csv_names[[i]][j])),
                                       sub(paste0('_', version_base, '_v00_', storey), '',
                                           sub('.csv', '', csv_names[[i]][j])))
      } else {
        names(csv_files[[i]])[j] = ifelse(cond == 'hvac',
                                       sub(paste0('_', version_base, '_v', version_simp, '_',
                                                  storey), '', sub('.csv', '', csv_names[[i]][j])),
                                       sub(paste0('_', version_base, '_v', version_simp, '_',
                                                  storey), '', sub('.csv', '', csv_names[[i]][j])))
      }
    }
    names(results[[i]]) = names(csv_files[[i]])
  }

  # rename columns ####
  # define new column names
  # hvac
  if (cond == 'hvac') {
    if (version_base == 'hyp') {
      base_dorm_cn = c('date_time', 'site_drybulb_temp', 'int_conv_he', 'occup_count',
                       rep('conv_hge', 8), 'mean_temp', 'op_temp', 'afn_inf_sens_hge',
                       'afn_inf_sens_hle', 'hvac_sens_he', 'hvac_sens_ce', 'hvac_total_he',
                       'hvac_total_ce')
      base_ew_liv_cn = c('date_time', 'site_drybulb_temp', 'int_conv_he', 'occup_count',
                         rep('conv_hge', 9), 'mean_temp', 'op_temp', 'afn_inf_sens_hge',
                         'afn_inf_sens_hle', 'hvac_sens_he', 'hvac_sens_ce', 'hvac_total_he',
                         'hvac_total_ce')
      base_sn_liv_cn = c('date_time', 'site_drybulb_temp', 'int_conv_he', 'occup_count',
                         rep('conv_hge', 10), 'mean_temp', 'op_temp', 'afn_inf_sens_hge',
                         'afn_inf_sens_hle', 'hvac_sens_he', 'hvac_sens_ce', 'hvac_total_he',
                         'hvac_total_ce')
      if (version_simp == '01' | version_simp == '02') {
        simp_dorm_cn = base_dorm_cn
        simp_ew_liv_cn = base_ew_liv_cn
        simp_sn_liv_cn = base_sn_liv_cn
      }
    }
  }

  # remove first column related to an x variable created when base 'csv' files were splitted
  for (i in 1:length(csv_files)) {
    for (j in 1:length(csv_files[[i]])) {
      csv_files[[i]][[j]][, 1] = NULL
    }
  }
  # rename 'csv' simulation files
  for (i in 1:length(csv_files[['base']])) {
    # base
    for (j in 1:dim(csv_files[['base']][[i]])[2]) {
      col = colnames(csv_files[['base']][[i]])[j]
      col = ifelse(
        grepl('dorm', names(csv_files[['base']])[i]),
        paste0(base_dorm_cn[j], surf_rename(col)), 
        ifelse(
          grepl('_e_liv',names(csv_files[['base']])[i]) | grepl('_w_liv',
                                                                names(csv_files[['base']])[i]),
          paste0(base_ew_liv_cn[j], surf_rename(col)),
          paste0(base_sn_liv_cn[j], surf_rename(col))
        )
      )
      colnames(csv_files[['base']][[i]])[j] = col
    }
    # simp
    for (j in 1:dim(csv_files[['simp']][[i]])[2]) {
      col = colnames(csv_files[['simp']][[i]])[j]
      col = ifelse(
        grepl('dorm', names(csv_files[['simp']])[i]),
        paste0(simp_dorm_cn[j], surf_rename(col)), 
        ifelse(
          grepl('_e_liv',names(csv_files[['simp']])[i]) | grepl('_w_liv',
                                                                names(csv_files[['simp']])[i]),
          paste0(simp_ew_liv_cn[j], surf_rename(col)),
          paste0(simp_sn_liv_cn[j], surf_rename(col))
        )
      )
      colnames(csv_files[['simp']][[i]])[j] = col
    }
  }
  
  # configure 'date_time' column ####
  for (i in 1:length(csv_files)) {
    for (j in 1:length(csv_files[[i]])) {
      csv_files[[i]][[j]]$date_time = seq(ISOdate(19, 1, 1, 0, 10, 0), by = '10 min',
                                          length.out = 365*24*6, tz='')
    }
  }
  
  # compile results ####
  for (i in 1:length(csv_files)) {
    results[[i]] = lapply(csv_files[[i]], report)
    for (j in 1:length(csv_files[[i]])) {
      results[[i]][[j]][['df']]$sim = ifelse(grepl('simp', names(results)[i]), 'Simp.', 'Base')
      results[[i]][[j]][['df']]$hvac_total_ce =
        results[[i]][[j]][['df']]$hvac_total_ce / df_area[j, 2]
      results[[i]][[j]][['df']] = label_df(results[[i]][[j]][['df']], names(results[[i]])[j])
      results[['combo']][['raw']] = rbind(results[['combo']][['raw']],
                                          results[[i]][[j]][['df']]['year', ])
    }
  }
  
  # compile differences
  for (i in 1:length(results[['simp']])) {
    for (type in c('abs', 'rel')) {
      results[['diff']][[type]][[i]] =
        df_diff(results[['simp']][[i]][['df']], results[['base']][[i]][['df']])[[type]]
      results[['diff']][['combo']][[type]] =
        rbind(results[['diff']][['combo']][[type]], results[['diff']][[type]][[i]]['year', ])
    }
  }

  # name diff list
  names(results[['diff']][['abs']]) = names(results[['diff']][['rel']]) = names(results[['simp']])
  
  # set a data frame for plotting thermal balance
  vars = c('int_conv_he', 'conv_hge_floor', 'conv_hge_roof', 'conv_hge_walls', 'conv_hge_windows',
           'conv_hge_doors', 'hvac_sens_ce', 'afn_inf_sens_hle')
  results[['combo']][['tb']] = data.frame('val' = NA, 'var' = NA, 'sim' = NA, 'dwel' = NA,
                                          'room' = NA, 'weather' = NA)
  results[['diff']][['combo']][['tb']][['abs']] = results[['diff']][['combo']][['tb']][['rel']] =
    data.frame('val' = NA, 'var' = NA, 'dwel' = NA, 'room' = NA, 'weather' = NA)
  
  # add data to data frames
  for (var in vars) {
    # thermal balance
    df = data.frame('val' = results[['combo']][['raw']][, var],
                    'var' = var, 'dwel' = results[['combo']][['raw']]$dwel,
                    'sim' = results[['combo']][['raw']]$sim,
                    'room' = results[['combo']][['raw']]$room,
                    'weather' = results[['combo']][['raw']]$weather)
    results[['combo']][['tb']] = rbind(results[['combo']][['tb']], df)
    # absolute difference of thermal balance between simplified and 'original' models
    df = data.frame('val' = results[['diff']][['combo']][['abs']][, var],
                    'var' = var, 'dwel' = results[['diff']][['combo']][['abs']]$dwel,
                    'room' = results[['diff']][['combo']][['abs']]$room,
                    'weather' = results[['diff']][['combo']][['abs']]$weather)
    results[['diff']][['combo']][['tb']][['abs']] =
      rbind(results[['diff']][['combo']][['tb']][['abs']], df)
    # relative difference of thermal balance between simplified and 'original' models
    df = data.frame('val' = results[['diff']][['combo']][['rel']][, var],
                    'var' = var, 'dwel' = results[['diff']][['combo']][['rel']]$dwel,
                    'room' = results[['diff']][['combo']][['rel']]$room,
                    'weather' = results[['diff']][['combo']][['rel']]$weather)
    results[['diff']][['combo']][['tb']][['rel']] =
      rbind(results[['diff']][['combo']][['tb']][['rel']], df)
  }
  results[['combo']][['tb']] = subset(results[['combo']][['tb']], !is.na(val))
  
return(results)
}

# application ####
# # v00 - v01
# storeys = c('floor', 'inter', 'roof')
# n = 0
# cond = 'hvac'
# storey = 'floor'
# version_base = 'hyp'
# version_simp = '01'
# for (i in 1:length(storeys)) {
#   results = valid(input_dirs = list('base' = paste0('/home/rodox/01.going_on/00.hive/00.hyp/',
#                                                     '00.hyp_v00/0', n, '.', storeys[i], '/00.split/'),
#                                     'simp' = paste0('/home/rodox/01.going_on/00.hive/00.hyp/',
#                                                     '01.hyp_v01/0', n, '.', storeys[i], '/00.split/')),
#                   cond, storey, version_base, version_simp,
#                   df_area = '/home/rodox/Dropbox/00.master_ufsc/01.validation/03.area/area_hyp.csv')
#   write.csv(results$combo$raw, paste0('/home/rodox/01.going_on/00.hive/00.hyp/01.hyp_v01/03.result/',
#                                       version_base, '_v', version_simp, '_', storeys[i], '_raw.csv'))
#   write.csv(results$combo$tb, paste0('/home/rodox/01.going_on/00.hive/00.hyp/01.hyp_v01/03.result/',
#                                       version_base, '_v', version_simp, '_', storeys[i], '_tb.csv'))
#   write.csv(results$diff$combo$abs, paste0('/home/rodox/01.going_on/00.hive/00.hyp/01.hyp_v01/',
#                                            '03.result/', version_base, '_v', version_simp, '_',
#                                            storeys[i], '_diff_abs.csv'))
#   write.csv(results$diff$combo$rel, paste0('/home/rodox/01.going_on/00.hive/00.hyp/01.hyp_v01/',
#                                            '03.result/', version_base, '_v', version_simp, '_',
#                                            storeys[i], '_diff_rel.csv'))
#   n = n + 1
# }

# v00 - v02
storeys = c('floor', 'inter', 'roof')
n = 0
cond = 'hvac'
storey = 'floor'
version_base = 'hyp'
version_simp = '02'
for (i in 1:length(storeys)) {
  results = valid(input_dirs = list('base' = paste0('/home/rodox/01.going_on/00.hive/00.hyp/',
                                                    '00.hyp_v00/0', n, '.', storeys[i], '/00.split/'),
                                    'simp' = paste0('/home/rodox/01.going_on/00.hive/00.hyp/',
                                                    '02.hyp_v02/0', n, '.', storeys[i], '/00.split/')),
                  cond, storey, version_base, version_simp,
                  df_area = '/home/rodox/Dropbox/00.master_ufsc/01.validation/03.area/area_hyp.csv')
  write.csv(results$combo$raw, paste0('/home/rodox/01.going_on/00.hive/00.hyp/02.hyp_v02/03.result/',
                                      version_base, '_v', version_simp, '_', storeys[i], '_raw.csv'))
  write.csv(results$combo$tb, paste0('/home/rodox/01.going_on/00.hive/00.hyp/02.hyp_v02/03.result/',
                                     version_base, '_v', version_simp, '_', storeys[i], '_tb.csv'))
  write.csv(results$diff$combo$abs, paste0('/home/rodox/01.going_on/00.hive/00.hyp/02.hyp_v02/',
                                           '03.result/', version_base, '_v', version_simp, '_',
                                           storeys[i], '_diff_abs.csv'))
  write.csv(results$diff$combo$rel, paste0('/home/rodox/01.going_on/00.hive/00.hyp/02.hyp_v02/',
                                           '03.result/', version_base, '_v', version_simp, '_',
                                           storeys[i], '_diff_rel.csv'))
  n = n + 1
}
