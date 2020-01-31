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
df_diff = function(df_base, df_simp, df_area) {
  # df_base - data frame with full simulation results
  # df_simp - data frame with single zone results
  
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
  yes_label = grepl('typo', colnames(df)) | grepl('simp', colnames(df)) |
    grepl('wrap', colnames(df)) | grepl('storey', colnames(df)) | grepl('cond', colnames(df)) |
    grepl('dwel', colnames(df)) | grepl('room', colnames(df)) | grepl('weather', colnames(df))
  # no, it is not!
  no_label = !yes_label
  
  return(list(yes_label, no_label))
}

# label()
  # create a label data frame according to building typology, simplification version, wrap system,
    # storey and type of conditioning
labels = function(input_dirs) {
  # input_dirs -  
  
  characs = c('typo', 'simp', 'wrap', 'storey', 'cond')
  labels = vector('list', length = length(input_dirs))
  names(labels) = names(input_dirs)
  labels = lapply(labels, function(x) x = vector('list', length = length(input_dirs[[1]])))
  
  for (i in 1:length(labels)) {
    names(labels[[i]]) = c('afn', 'hvac')
    labels[[i]] = lapply(labels[[i]], function(x) x = vector('list', length = length(characs)))
  }
  
  for (i in 1:length(labels)) {
    for (j in 1:length(labels[[i]])) {
      names(labels[[i]][[j]]) = characs
      if (grepl('/00.hyp/', input_dirs[[i]][j])) {
        labels[[i]][[j]]$typo = c('hyp', 'Hip.')
      } else if (grepl('/01.lin/', input_dirs[[i]][j])) {
        labels[[i]][[j]]$typo = c('lin', 'Linear')
      } else {
        labels[[i]][[j]]$typo = c('h', 'H')
      }
      if (grepl('/00/', input_dirs[[i]][j])) {
        labels[[i]][[j]]$simp = rep('00', 2)
      } else if (grepl('/01/', input_dirs[[i]][j])) {
        labels[[i]][[j]]$simp = rep('01', 2)
      } else if (grepl('/02/', input_dirs[[i]][j])) {
        labels[[i]][[j]]$simp = rep('02', 2)
      } else if (grepl('/03/', input_dirs[[i]][j])) {
        labels[[i]][[j]]$simp = rep('03', 2)
      } else if (grepl('/04/', input_dirs[[i]][j])) {
        labels[[i]][[j]]$simp = rep('04', 2)
      } else if (grepl('/05/', input_dirs[[i]][j])) {
        labels[[i]][[j]]$simp = rep('05', 2)
      } else if (grepl('/06/', input_dirs[[i]][j])) {
        labels[[i]][[j]]$simp = rep('06', 2)
      } else {
        labels[[i]][[j]]$simp = rep('07', 2)
      }
      if (grepl('/00.c10/', input_dirs[[i]][j])) {
        labels[[i]][[j]]$wrap = c('c10', 'C10')
      } else if (grepl('/01.tv/', input_dirs[[i]][j])) {
        labels[[i]][[j]]$wrap = c('tv', 'TV')
      } else {
        labels[[i]][[j]]$wrap = c('sf', 'SF')
      }
      if (grepl('/00.floor/', input_dirs[[i]][j])) {
        labels[[i]][[j]]$storey = c('floor', 'Térreo')
      } else if (grepl('/01.inter/', input_dirs[[i]][j])) {
        labels[[i]][[j]]$storey = c('inter', 'Intermediário')
      } else {
        labels[[i]][[j]]$storey = c('roof', 'Cobertura')
      }
      if (grepl('/00.afn/', input_dirs[[i]][[j]])) {
        labels[[i]][[j]]$cond = c('afn', 'AFN')
      } else {
        labels[[i]][[j]]$cond = c('hvac', 'HVAC')
      }
    }
  }
  
  return(labels)
}

# label_df()
  # label data frames according to dweling, room, side, weather, typology wrap and
    # simplification version
label_df = function(df, labels, file_name) {
  df$typo = labels$typo[2]
  df$simp = labels$simp[2]
  df$wrap = labels$wrap[2]
  df$storey = labels$storey[2]
  df$cond = labels$cond[2]
  df$dwel = ifelse(grepl('_sw_', file_name), 'SO',
                   ifelse(grepl('_se_', file_name), 'SE',
                          ifelse(grepl('_e_', file_name), 'L',
                                 ifelse(grepl('_ne_', file_name), 'NE',
                                        ifelse(grepl('_nw_', file_name), 'NO', 'O')))))
  side = ifelse(grepl('rm_s', file_name), 'S',
                ifelse(grepl('rm_n', file_name), 'N',
                       ifelse(grepl('rm_1', file_name), '1', '2')))
  df$room = ifelse(grepl('liv', file_name), 'Sala', paste('Dorm.', side))
  df$weather = ifelse(grepl('curitiba', file_name), 'Curitiba',
                      ifelse(grepl('rio_de_janeiro', file_name), 'Rio de Janeiro', 'São Paulo'))
  
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
    # calculates thermal balance and other metrics monthly and annually
report = function(csv, timestep = 6, unit = 'kwh', cond) {
  # csv - 'csv' simulation file from energyplus simulation
  # timestep - number of timesteps per hour in the 'csv' simulation file
  # unit - output's units
  # possible values: 'kwh' or 'kj'
  
  # unites
  div = ifelse(unit == 'kwh', 3600000, 1000)
  # months
  year = month_timestep(timestep)
  
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
  afn_inf_sens_hge = csv$afn_inf_sens_hge/div
  afn_inf_sens_hle = -csv$afn_inf_sens_hle/div
  if (cond == 'hvac') {
    hvac_sens_he = csv$hvac_sens_he/div
    hvac_sens_ce = -csv$hvac_sens_ce/div
    hvac_total_he = csv$hvac_total_he/div
    hvac_total_ce = csv$hvac_total_ce/div
  } else {
    mean_temp = csv$mean_temp
    comf = csv[, grepl('occup_count', colnames(csv)) | grepl('op_temp', colnames(csv))]
    afn_air_change = csv$afn_air_change
  }
  
  # throw all the vectors inside the report list
  report = list('int_conv_he' = int_conv_he, 'conv_hge_floor' = conv_hge_floor,
                'conv_hge_roof' = conv_hge_roof, 'conv_hge_wall_s' = conv_hge_wall_s,
                'conv_hge_wall_e' = conv_hge_wall_e, 'conv_hge_wall_n' = conv_hge_wall_n,
                'conv_hge_wall_w' = conv_hge_wall_w, 'conv_hge_walls' = conv_hge_walls,
                'conv_hge_windows' = conv_hge_windows, 'conv_hge_doors' = conv_hge_doors,
                'afn_inf_sens_hge' = afn_inf_sens_hge, 'afn_inf_sens_hle' = afn_inf_sens_hle)
  if (cond == 'hvac') {
    report[['hvac_sens_he']] = hvac_sens_he
    report[['hvac_sens_ce']] = hvac_sens_ce
    report[['hvac_total_he']] = hvac_total_he
    report[['hvac_total_ce']] = hvac_total_ce
  } else {
    report[['mean_temp']] = mean_temp
    report[['comf']] = comf
    report[['afn_air_change']] = afn_air_change
  }
  report[['df']] = NULL
  
  # create data frame
    # the data frame number of columns is 1 size smaller than the length of report because 1 item of
      # report's list correspond to 'df'
  if (cond == 'hvac') {
    cond_vars = c('hvac_sens_he', 'hvac_sens_ce', 'hvac_total_he', 'hvac_total_ce')
    report[['df']] = as.data.frame(matrix(NA, 12, length(report)))
  } else {
    cond_vars = c('afn_air_change', 'uncomf_hot', 'uncomf_cold', 'comf', 'temp_max', 'temp_95')
    report[['df']] = as.data.frame(matrix(NA, 12, length(report) + 3))
  }
  
  colnames(report[['df']]) = c('int_conv_he', 'conv_hge_floor', 'conv_hge_roof', 'conv_hge_wall_s',
                               'conv_hge_wall_e', 'conv_hge_wall_n', 'conv_hge_wall_w',
                               'conv_hge_walls', 'conv_hge_windows', 'conv_hge_doors',
                               'afn_inf_sens_hge', 'afn_inf_sens_hle', cond_vars)
  rownames(report[['df']]) = names(year)
  for (month in names(year)) {
    if (cond == 'hvac') {
      for (col in colnames(report[['df']])) {
        report[['df']][month, col] = ifelse(is.null(dim(report[[col]])),
                                            sum(report[[col]][year[[month]]]),
                                            sum(apply(report[[col]][year[[month]], ], 2, sum)))
      }
    } else {
      for (col in colnames(report[['df']][, 1:12])) {
        report[['df']][month, col] = ifelse(is.null(dim(report[[col]])),
                                            sum(report[[col]][year[[month]]]),
                                            sum(apply(report[[col]][year[[month]], ], 2, sum)))
      }
      report[['df']][month, 'afn_air_change'] = mean(report[['afn_air_change']][year[[month]]])
      report[['df']][month, 'uncomf_hot'] = uncomf(df = report[['comf']][year[[month]], ],
                                                   feel = 'hot', hot_weather = F)
      report[['df']][month, 'uncomf_cold'] = uncomf(df = report[['comf']][year[[month]], ],
                                                    feel = 'cold', hot_weather = F)
      report[['df']][month, 'comf'] =
        100 - (report[['df']][month, 'uncomf_hot'] + report[['df']][month, 'uncomf_cold'])
      report[['df']][month, 'temp_max'] = max(report[['comf']][year[[month]], 'op_temp'])
      report[['df']][month, 'temp_95'] = quantile(report[['mean_temp']][year[[month]]],
                                                   probs = 0.95, names = F)
    }
  }

  if (cond == 'hvac') {
    report[['df']]['year', ] = apply(report[['df']][1:12, ], 2, sum)
    report[['df']]$hvac_total_he = ifelse(report[['df']]$hvac_total_he < 0.01, 0,
                                          report[['df']]$hvac_total_he)
    report[['df']]$hvac_total_ce = ifelse(report[['df']]$hvac_total_ce < 0.01, 0,
                                          report[['df']]$hvac_total_ce)
  } else {
    report[['df']]['year', 1:12] = apply(report[['df']][1:12, 1:12], 2, sum)
    report[['df']]['year', 'afn_air_change'] = mean(report[['df']][1:12, 'afn_air_change'])
    report[['df']]['year', 'uncomf_hot'] = uncomf(df = report[['comf']],
                                                  feel = 'hot', hot_weather = F)
    report[['df']]['year', 'uncomf_cold'] = uncomf(df = report[['comf']],
                                                   feel = 'cold', hot_weather = F)
    report[['df']]['year', 'comf'] =
      100 - (report[['df']]['year', 'uncomf_hot'] + report[['df']]['year', 'uncomf_cold'])
    report[['df']]['year', 'temp_max'] = max(report[['df']][1:12, 'temp_max'])
    report[['df']]['year', 'temp_95'] = quantile(report[['mean_temp']], probs = 0.95, names = F)
  }
  
  report[['df']] = apply(report[['df']], 2, round, 1)
  report[['df']] = cbind(report[['df']], data.frame('typo' = NA, 'simp' = NA, 'wrap' = NA,
                                                    'storey' = NA, 'cond' = NA, 'dwel' = NA,
                                                    'room' = NA, 'weather' = NA))
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
  side = ifelse(grepl('_S\\:', col_name) & !grepl('M_S\\:', col_name), '_s',
                ifelse(grepl('_E\\:', col_name), '_e',
                       ifelse(grepl('_N\\:', col_name) & !grepl('M_N\\:', col_name), '_n',
                              ifelse(grepl('_W\\:', col_name), '_w',
                                     ''))))
  surf_rename = paste0(surf, side)
  return(surf_rename)
}

# uncomf()
  # calculate percentage of hours feeling uncomfortable (hot or cold)
uncomf = function (df, feel, hot_weather) {
  # df - data frame containing raw info about natural ventilation (vn)
  # hot_weather - TRUE for brazilian climate zone 8, which means hot weather
  # feel - 'hot' or 'cold'
  
  if (hot_weather == T) {
    uncomf_hot = sum(df$occup_count > 0 & df$op_temp > 28) / sum(df$occup_count > 0) * 100
  } else {
    uncomf_hot = sum(df$occup_count > 0 & df$op_temp > 26) / sum(df$occup_count > 0) * 100
  }
  uncomf_cold = sum(df$occup_count > 0 & df$op_temp < 18) / sum(df$occup_count > 0) * 100
  
  ifelse(feel == 'hot', return(uncomf_hot), return(uncomf_cold))
}

# main function ####
# valid = function(input_dirs, df_area, write_results = F, output_dir) {
  # input_dirs - 
  # df_area - 
  # output_dir - 
  # write_results - if it's f' the results are assigned to a variable, if it's 't' the results are
    # writen into 'csv' files
  

input_dirs = list('base' = c('/media/rodox/01.going_on/00.hive/00.hyp/00/00.c10/01.inter/00.afn/',
                             '/media/rodox/01.going_on/00.hive/00.hyp/00/00.c10/01.inter/01.hvac/'),
                  'simp' = c('/media/rodox/01.going_on/00.hive/00.hyp/01/00.c10/01.inter/00.afn/',
                             '/media/rodox/01.going_on/00.hive/00.hyp/01/00.c10/01.inter/01.hvac/'))
df_area = '/home/rodox/00.git/00.master_ufsc/02.model/00.hyp/area_hyp.csv'
write_results = T

  # create a data frame with simplification labels
  labels = labels(input_dirs)

  # create empty lists to be filled with 'csv' files
  csv_names = csv_files = results = vector('list', length(input_dirs))
  # name the lists
  names(csv_names) = names(csv_files) = names(results) = names(input_dirs)
  
  # load files
  # pick 'csv' names inside input directory
  for (i in 1:length(csv_names)) {
    csv_names[[i]] = dir(input_dirs[[i]], '.csv')
    # extend results
    results[[i]] = vector('list', length(csv_names[[i]]))
  }
  
  # read files
  # data frame with zone areas
  df_area = read.csv(df_area)
  # multiply data frame three times, because there are three weathers
  df_area = rbind(df_area, df_area, df_area)
  # 'csv' files from simulations
  for (i in 1:length(csv_names)) {
    for (j in 1:length(csv_names[[i]])) {
      # count the files while they're loaded
      print(paste('i =', i, '/ j =', j))
      # load the files themselves
      csv_files[[i]][[j]] = as.data.frame(data.table::fread(paste0(input_dirs[[i]],
                                                                   csv_names[[i]][[j]])))
      # define proper names to the list
      names(csv_files[[i]])[j] = sub(paste0('_', labels[[i]]$typo[1], '_',
                                            labels[[i]]$wrap[1], '_v',
                                            labels[[i]]$simp[1], '_',
                                            labels[[i]]$storey[1], '_',
                                            labels[[i]]$cond[1]), '',
                                     sub('.csv', '', csv_names[[i]][j]))
    }
    names(results[[i]]) = names(csv_files[[i]])
  }
  
  # rename columns
  # define new column names
  if (grepl('hyp', labels[[1]]$typo[1])) {
    mult_base_dorm_cn = 8
    mult_base_ew_liv_cn = 10
    mult_base_sn_liv_cn = 11
    if (labels[[2]]$simp[1] == '01' | labels[[2]]$simp[1] == '03') {
      mult_simp_dorm_cn = 8
      mult_simp_ew_liv_cn = 9
      mult_simp_sn_liv_cn = 10
    } else if (labels[[2]]$simp[1] == '02') {
      mult_simp_dorm_cn = 8
      mult_simp_ew_liv_cn = 10
      mult_simp_sn_liv_cn = 11
    }
  }
  if (labels[[1]]$cond[1] == 'hvac') {
    out_cond = c('hvac_sens_he', 'hvac_sens_ce', 'hvac_total_he', 'hvac_total_ce')
  } else {
    out_cond = 'afn_air_change'
  }
  base_dorm_cn = c('date_time', 'site_drybulb_temp', 'int_conv_he', 'occup_count',
                   rep('conv_hge', mult_base_dorm_cn), 'mean_temp', 'op_temp', 'afn_inf_sens_hge',
                   'afn_inf_sens_hle', out_cond)
  base_ew_liv_cn = c('date_time', 'site_drybulb_temp', 'int_conv_he', 'occup_count',
                     rep('conv_hge', mult_base_ew_liv_cn), 'mean_temp', 'op_temp',
                     'afn_inf_sens_hge', 'afn_inf_sens_hle', out_cond)
  base_sn_liv_cn = c('date_time', 'site_drybulb_temp', 'int_conv_he', 'occup_count',
                     rep('conv_hge', mult_base_sn_liv_cn), 'mean_temp', 'op_temp',
                     'afn_inf_sens_hge', 'afn_inf_sens_hle', out_cond)
  simp_dorm_cn = c('date_time', 'site_drybulb_temp', 'int_conv_he', 'occup_count',
                   rep('conv_hge', mult_simp_dorm_cn), 'mean_temp', 'op_temp', 'afn_inf_sens_hge',
                   'afn_inf_sens_hle', out_cond)
  simp_ew_liv_cn = c('date_time', 'site_drybulb_temp', 'int_conv_he', 'occup_count',
                     rep('conv_hge', mult_simp_ew_liv_cn), 'mean_temp', 'op_temp',
                     'afn_inf_sens_hge', 'afn_inf_sens_hle', out_cond)
  simp_sn_liv_cn = c('date_time', 'site_drybulb_temp', 'int_conv_he', 'occup_count',
                     rep('conv_hge', mult_simp_sn_liv_cn), 'mean_temp', 'op_temp',
                     'afn_inf_sens_hge', 'afn_inf_sens_hle', out_cond)
  
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
  
  # configure 'date_time' column
  for (i in 1:length(csv_files)) {
    for (j in 1:length(csv_files[[i]])) {
      csv_files[[i]][[j]]$date_time = seq(ISOdate(19, 1, 1, 0, 10, 0), by = '10 min',
                                          length.out = 365*24*6, tz='')
    }
  }
  
  # compile results
  results[['combo']][['raw']] = vector('list', length = length(csv_files))
  names(results[['combo']][['raw']]) = names(csv_files)
  for (i in 1:length(csv_files)) {
    results[[i]] = lapply(csv_files[[i]], report, cond = labels[[i]]$cond[1])
    for (j in 1:length(csv_files[[i]])) {
      if (labels[[i]]$cond[1] == 'hvac') {
        results[[i]][[j]][['df']] = results[[i]][[j]][['df']] / df_area[j, 2]
      }
      results[[i]][[j]][['df']] = label_df(results[[i]][[j]][['df']], labels[[i]],
                                           names(results[[i]])[j])
      results[['combo']][['raw']][[i]] = rbind(results[['combo']][['raw']][[i]],
                                          results[[i]][[j]][['df']]['year', ])
    }
  }
  
  # compile differences
  for (type in c('abs', 'rel')) {
    for (i in 1:length(results[['simp']])) {
      results[['diff']][[type]][[i]] =
        df_diff(results[['base']][[i]][['df']], results[['simp']][[i]][['df']])[[type]]
      results[['combo']][['diff']][[type]] =
        rbind(results[['combo']][['diff']][[type]], results[['diff']][[type]][[i]]['year', ])
    }
  }
  # name diff list
  names(results[['diff']][['abs']]) = names(results[['diff']][['rel']]) = names(results[['simp']])
  
  # write result files
  if (write_results == T) {
    for (i in 1:length(results[['combo']])) {
      for (j in 1:length(results[['combo']][[i]])) {
        write.csv(results[['combo']][[i]][[j]],
                  paste0(output_dir, '/', labels[['base']]$typo[1], '_v',
                         ifelse(names(results[['combo']][[i]])[j] == 'base',
                                labels[['base']]$simp[1], labels[['simp']]$simp[1]), '_',
                         labels[['base']]$wrap[1], '_', labels[['base']]$storey[1], '_',
                         labels[['base']]$cond[1], '_', names(results[['combo']])[i],
                         ifelse(names(results[['combo']])[i] == 'raw', '',
                                paste0('_', names(results[['combo']][[i]])[j])),
                         '.csv'), row.names = F)
      }
    }
  } else {
    return(results)
  }
}


# application ####
typos = c('hyp')
simps = c('01', '02', '03')
wraps = c('c10', 'tv', 'sf')
storeys = c('floor', 'inter', 'roof')
conds = c('afn', 'hvac')
m = 0
for (typo in typos) {
  for (simp in simps) {
    n = 0
    for (wrap in wraps) {
      o = 0
      for (storey in storeys) {
        p = 0
        for (cond in conds) {
          print(paste(Hmisc::capitalize(typo), '/', simp, '/', toupper(wrap), '/',
                      Hmisc::capitalize(storey), '/', toupper(cond)))
          valid(
            input_dirs = list('base' = paste0('/media/rodox/HD_EXTERNO/00.hive/0', m, '.', typo,
                                              '/00/0', n, '.', wrap, '/0', o, '.', storey,
                                              '/0', p, '.', cond, '/'),
                              'simp' = paste0('/media/rodox/HD_EXTERNO/00.hive/0', m, '.', typo,
                                              '/', simp, '/0', n, '.', wrap, '/0', o, '.', storey,
                                              '/0', p, '.', cond, '/')),
            df_area = paste0('/home/rodox/00.git/00.master_ufsc/02.model/0', m, '.', typo,
                             '/area_', typo, '.csv'),
            write_results = T,
            output_dir = paste0('/home/rodox/00.git/00.master_ufsc/03.result/')
            )
          gc()
          p = p + 1
        }
        o = o + 1
      }
      n = n + 1
    }
  }
  m = m + 1
}
