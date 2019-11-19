# load libraries
library(ggplot2)

# functions ####
# diff()
# calculate difference of results from simplified simulations (single zone) to full simulations
df_diff = function(df_sz, df_full) {
  # df_sz - data frame with single zone results
  # df_full - data frame with full simulation results
  
  # calculate absolute difference
  # formula: diff_abs = val_sz - val_full
  df_diff_abs = df_sz[, is_label(df_sz)[[2]]] - df_full[, is_label(df_full)[[2]]]
  # calculate relative difference
  # formula: diff_rel = (val_sz - val_full) / val_full
  df_diff_rel = df_diff_abs*100 / df_full[, is_label(df_sz)[[2]]]
  # add labels to the data frame
  df_diff_abs = cbind(df_diff_abs, df_sz[, is_label(df_sz)[[1]]])
  df_diff_rel = cbind(df_diff_rel, df_sz[, is_label(df_sz)[[1]]])
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
                ifelse(grepl('rm_e', file_name), 'E',
                       ifelse(grepl('rm_n', file_name), 'N', 'W')))
  df$room = ifelse(grepl('living', file_name), 'Living', paste('Dorm.', side))
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
  conv_hge_floor = -csv[, grepl('floor', colnames(csv)) & grepl('conv_hge', colnames(csv))]/div
  conv_hge_roof = -csv[, grepl('roof', colnames(csv)) & grepl('conv_hge', colnames(csv))]/div
  conv_hge_walls = -csv[, grepl('wall', colnames(csv)) & grepl('conv_hge', colnames(csv))]/div
  conv_hge_windows = -csv[, grepl('window', colnames(csv)) & grepl('conv_hge', colnames(csv))]/div
  conv_hge_doors = -csv[, grepl('door', colnames(csv)) & grepl('conv_hge', colnames(csv))]/div
  hvac_sens_he = csv$hvac_sens_he/div
  hvac_sens_ce = -csv$hvac_sens_ce/div
  afn_inf_sens_hge = csv$afn_inf_sens_hge/div
  afn_inf_sens_hle = -csv$afn_inf_sens_hle/div
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
  # create data frame
  # the data frame number of columns is 1 size smaller than the length of report because 1 item of
  # report's list correspond to 'df'
  report[['df']] = as.data.frame(matrix(NA, 12, length(report) - 1))
  colnames(report[['df']]) = c('int_conv_he', 'conv_hge_floor', 'conv_hge_roof', 'conv_hge_walls',
                               'conv_hge_windows', 'conv_hge_doors', 'hvac_sens_he', 'hvac_sens_ce',
                               'afn_inf_sens_hge', 'afn_inf_sens_hle', 'afn_inf_air_change',
                               'hvac_total_he', 'hvac_total_ce')
  rownames(report[['df']]) = names(year)
  isnt_air_change = colnames(report[['df']]) != 'afn_inf_air_change'
  for (month in names(year)) {
    afn_month = afn_inf_air_change[year[[month]], ]
    report[['df']][month, 'afn_inf_air_change'] =
      mean(subset(afn_month, sch_afn != 0)$afn_inf_air_change)
    for (col in colnames(report[['df']])[isnt_air_change]) {
      report[['df']][month, col] = ifelse(is.null(dim(report[[col]])),
                                          sum(report[[col]][year[[month]]]),
                                          sum(apply(report[[col]][year[[month]], ], 2, sum)))
    }
  }
  report[['df']]['year', isnt_air_change] = apply(report[['df']][1:12, isnt_air_change], 2, sum)
  report[['df']]['year', 'afn_inf_air_change'] = mean(report[['df']][1:12, 'afn_inf_air_change'])
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
  side = ifelse(grepl('_S\\.', col_name) & !grepl('M_S.', col_name), '_s',
                ifelse(grepl('_E\\.', col_name) & !grepl('M_E.', col_name), '_e',
                       ifelse(grepl('_N\\.', col_name) & !grepl('M_N.', col_name), '_n',
                              ifelse(grepl('_W\\.', col_name) & !grepl('M_W.', col_name), '_w',
                                     ''))))
  surf_rename = paste0(surf, side)
  return(surf_rename)
}

# variables to run the code ####
# with single zone results directory (first) and the directories of the real cases
input_dirs = list('sz' = paste0('/home/rodox/Dropbox/00.master_ufsc/00.single_zone/01.validation/',
                                '00.sz/01.result/'),
                  'multi' = paste0('/home/rodox/Dropbox/00.master_ufsc/00.single_zone/',
                                   '01.validation/01.multi/01.result/00.1st_multi/'))

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
# remove unuseful variables
rm(i)

# read files
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
# remove unuseful variables
rm(input_dirs, i, j)

# rename columns ####
# delete columns related to the hives
csv_files[['sz']] = lapply(csv_files$sz, function(x) x[, grepl('CORE', colnames(x)) |
                                                         grepl('Date.Time', colnames(x)) |
                                                         grepl('Drybulb', colnames(x))])
# define new column names
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

# rename 'csv' simulation files
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
# remove unuseful variables
rm(sz_cn, multi_dorm_cn, multi_ew_liv_cn, multi_sn_liv_cn, i, j, col)

# configure 'date_time' column ####
for (i in 1:length(csv_files)) {
  for (j in 1:length(csv_files[[i]])) {
    csv_files[[i]][[j]]$date_time = seq(ISOdate(19, 1, 1, 0, 10, 0), by = '10 min',
                                        length.out = 365*24*6, tz='')
  }
}
# remove unuseful variables
rm(i, j)

# compile results ####
for (i in 1:length(csv_files)) {
  results[[i]] = lapply(csv_files[[i]], report)
  for (j in 1:length(csv_files[[i]])) {
    results[[i]][[j]][['df']]$sim = ifelse(grepl('sz', names(results)[i]), 'SZ', 'Multi.')
    results[[i]][[j]][['df']] = label_df(results[[i]][[j]][['df']], names(results[[i]])[j])
    results[['combo']][['raw']] = rbind(results[['combo']][['raw']],
                                        results[[i]][[j]][['df']]['year', ])
  }
}
# remove unuseful variables
rm(i, j)

# compile differences
for (i in 1:length(results[['sz']])) {
  for (type in c('abs', 'rel')) {
    results[['diff']][[type]][[i]] = df_diff(results[['sz']][[i]][['df']],
                                             results[['multi']][[i]][['df']])[[type]]
    results[['diff']][['combo']][[type]] =
      rbind(results[['diff']][['combo']][[type]],
            df_diff(results[['sz']][[i]][['df']],
                    results[['multi']][[i]][['df']])[[type]]['year', ])
  }
}
# remove unuseful variables
rm(i, type)
# name diff list
names(results[['diff']][['abs']]) = names(results[['diff']][['rel']]) = names(results[['sz']])

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
# remove unuseful variables
rm(var, vars, df)

# detailed anaylis
details = list('max' = NULL)
for (i in 1:length(results[['diff']][['abs']])) {
  details[['max']][[i]] =
    apply(results[['diff']][['abs']][[i]][1:12, is_label(results[['diff']][['abs']][[i]])[[2]]], 2,
          function(x) which(x == max(x)))
}
# remove unuseful variables
rm(i)
# name details
names(details[['max']]) = names(results[['sz']])

# plot ####
# plot_cgtr()
# plot cooling thermal load
plot_cgtr = function(df, plot_dir) {
  # df - 
  # plot_dir - 
  
  # start plotting
  # associate conditions to plot
  png(filename = paste0(plot_dir, 'cgtr.png'),
      width = 33.8, height = 19, units = 'cm', res = 500)
  plot(
    # define main data frame used in the plot
    ggplot(data = df, aes(x = dwel, y = hvac_total_ce)) +
      # create one grid for each weather
      facet_grid(sim ~ weather) +
      # insert a bar geometry plot using 'total cooling load x dweling'
      geom_bar(stat = 'identity', position = 'dodge', aes(x = dwel, y = hvac_total_ce, fill = room)) +
      # define labs (title, x and y labs)
      labs(title = 'Carga Térmica de Refrigeração',
           x = NULL,
           y = 'CgTR (kWh)',
           fill = 'Room:') +
      # edit all kind of text in the plot
      theme(legend.text = element_text(size = 14),
            legend.title = element_text(size = 15),
            legend.position = 'bottom',
            plot.title = element_text(size = 20, hjust = 0.5),
            axis.title.y = element_text(size=15),
            axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size=14),
            strip.text.x = element_text(size = 17),
            strip.text.y = element_text(size = 17))
  )
  # finish plotting
  dev.off()
}

# plot_diff_cgtr()
# plot difference of cooling thermal load between simplified model and 'original' model
plot_diff_cgtr = function(df, rel = F, plot_dir) {
  # df - 
  # rel - 
  # plot_dir - 
  
  # start plotting
  # associate conditions to plot and name the plot and name the plot
  plot_name = ifelse(rel == F, 'cgtr_diff_abs.png', 'cgtr_diff_rel.png')
  png(filename = paste0(plot_dir, plot_name), width = 33.8, height = 19, units = 'cm', res = 500)
  plot(
    # define main data frame used in the plot
    ggplot(data = df, aes(x = dwel, y = hvac_total_ce)) +
      # create one grid for each weather
      facet_grid(. ~ weather) +
      # insert a bar geometry plot using 'total cooling load x dweling'
      geom_bar(stat = 'identity', position = 'dodge', aes(x = dwel, y = hvac_total_ce, fill = room)) +
      # define labs (title, x and y labs)
      labs(title = paste('Diferença', ifelse(rel == F, 'Absoluta', 'Relativa'),
                          'de Carga Térmica de Refrigeração'),
           subtitle = 'Diff = SZ - "Original"',
           x = NULL,
           y = ifelse(rel == F, 'Diff. CgTR (kWh)', 'Diff. CgTR (%)'),
           fill = 'Room:') +
      # edit all kind of text in the plot
      theme(legend.text = element_text(size = 14),
            legend.title = element_text(size = 15),
            legend.position = 'bottom',
            plot.title = element_text(size = 20, hjust = 0.5),
            plot.subtitle = element_text(size = 18, hjust = 0.5),
            axis.title.y = element_text(size=15),
            axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size=14),
            strip.text.x = element_text(size = 17),
            strip.text.y = element_text(size = 17))
  )
  # finish plotting
  dev.off()
}

# plot_tb()
# plot thermal balance
plot_tb = function(df, Dwel, Room, plot_dir) {
  # df - 
  # Dwel - 
  # Room - 
  
  # pre-process
  df = subset(subset(df, dwel == Dwel), room == Room)
  # start plotting
  # associate conditions to plot and name plot
  plot_name = paste0('tb_', tolower(Dwel), '_', tolower(sub('. ', '_', Room)), '.png')
  png(filename = paste0(plot_dir, plot_name), width = 33.8, height = 19, units = 'cm', res = 500)
  plot(
    # define main data frame used in the plot
    ggplot(data = df, aes(x = var, y = val, fill = var)) +
      # create one grid for each weather
      facet_grid(sim ~ weather) +
      # insert a bar geometry plot using 'total cooling load x dweling'
      geom_bar(stat = 'identity', position = 'dodge') +
      # define labs (title, x and y labs)
      labs(title = 'Balanço Térmico',
           subtitle = paste('Apto.', Dwel, Room),
           x = NULL,
           y = 'Carga (kWh)') +
      # edit legend
      scale_fill_discrete(name = 'Troca\nde Calor:',
                          labels = c('VN', 'Portas', 'Piso', 'Cobertura', 'Paredes', 'Janelas',
                                     'HVAC', 'Cargas Internas')) +
      # edit all kind of text in the plot
      theme(legend.text = element_text(size = 14),
            legend.title = element_text(size = 15),
            legend.position = 'bottom',
            plot.title = element_text(size = 20, hjust = 0.5),
            plot.subtitle = element_text(size = 18, hjust = 0.5),
            axis.title.y = element_text(size=15),
            axis.text.x = element_blank(),
            axis.text.y = element_text(size=14),
            strip.text.x = element_text(size = 17),
            strip.text.y = element_text(size = 17)))
  # finish plotting
  dev.off()
}

# plot_diff_tb()
# plot differences in thermal balance between simplified model and 'original' model
plot_diff_tb = function(df, Dwel, Room, rel = F, plot_dir) {
  # df - 
  # Dwel - 
  # Room - 
  # plot_dir - 

  # pre-process
  df = subset(subset(df, dwel == Dwel), room == Room)
  # start plotting
  # associate conditions to plot and name plot
  plot_name = paste0('tb_diff_', ifelse(rel == F, 'abs_', 'rel_'), tolower(Dwel), '_',
                     tolower(sub('. ', '_', Room)), '.png')
  png(filename = paste0(plot_dir, plot_name), width = 33.8, height = 19, units = 'cm', res = 500)
  plot(
    # define main data frame used in the plot
    ggplot(data = df, aes(x = var, y = val, fill = var)) +
      # create one grid for each weather
      facet_grid(. ~ weather) +
      # insert a bar geometry plot using 'total cooling load x dweling'
      geom_bar(stat = 'identity', position = 'dodge') +
      # define labs (title, x and y labs)
      labs(title = paste('Diferenças', ifelse(rel == F, 'Absolutas', 'Relativas'),
                         'no Balanço Térmico'),
           subtitle = paste('Apto.', Dwel, Room, '\nDiff = SZ - "Original"'),
           x = NULL,
           y = ifelse(rel == F, 'Diff. Carga (kWh)', 'Diff. Carga (%)')) +
      # edit legend
      scale_fill_discrete(name = 'Troca\nde Calor:',
                          labels = c('VN', 'Portas', 'Piso', 'Cobertura', 'Paredes', 'Janelas',
                                     'HVAC', 'Cargas Internas')) +
      # edit all kind of text in the plot
      theme(legend.text = element_text(size = 14),
            legend.title = element_text(size = 15),
            legend.position = 'bottom',
            plot.title = element_text(size = 20, hjust = 0.5),
            plot.subtitle = element_text(size = 18, hjust = 0.5),
            axis.title.y = element_text(size=15),
            axis.text.x = element_blank(),
            axis.text.y = element_text(size=14),
            strip.text.x = element_text(size = 17),
            strip.text.y = element_text(size = 17)))
  # finish plotting
  dev.off()
}

# plot daily detailed analysis
# plot_detail = function(csv, day, plot_name, plot_dir) {
  # csv - 
  # day - 
  # file_name - 
  # plot_dir - 
  
  # test
  csv_sz = csv_files[[1]][[1]]
  csv_full = csv_files[[2]][[1]]
  day = '19-01-02'
  plot_name = 'detail_rio_de_janeiro_e_dorm_n.png'
  plot_dir = '/home/rodox/Dropbox/00.master_ufsc/00.single_zone/02.plot/'
  unit = 'kj'
  
  # pre-process
  # define unites
  div = ifelse(unit == 'kwh', 3600000, 1000)
  # define columns in analysis
  cols = c('date_time', 'site_drybulb_temp', 'hvac_sens_ce', 'afn_inf_sens_hge', 'afn_inf_sens_hle') 
  
  csv_sz$sim = 'SZ'
  csv_full$sim = 'Multi.'
  csv = list(csv_sz, csv_full)
  
  for (i in 1:length(csv)) {
    csv[[i]] = csv[[i]][c(start_day:(start_day + 24*6 - 1)), ]
    conv_hge = vector('list', length(surfs))
    names(conv_hge) = c('floor', 'roof', 'wall', 'window', 'door')
    for (surf in names(conv_hge)) {
      conv_hge[[surf]] = -csv[[i]][, grepl(surf, colnames(csv[[i]])) &
                                     grepl('conv_hge', colnames(csv[[i]]))]/div
      if (is.data.frame(conv_hge[[surf]])) {
        fill = conv_hge[[surf]][, 1]
        for (j in 2:length(conv_hge[[surf]])) {
          fill = fill + conv_hge[[surf]][, j]
        }
        conv_hge[[surf]] = fill
      }
      csv[[i]][, paste0('conv_', surf)] = conv_hge[[surf]]
    }
    csv[[i]][, grepl('conv_hge', colnames(csv[[i]]))] = NULL
  }
  
  # hvac_sens_he = csv$hvac_sens_he/div
  # hvac_sens_ce = -csv$hvac_sens_ce/div
  # afn_inf_sens_hge = csv$afn_inf_sens_hge/div
  # afn_inf_sens_hle = -csv$afn_inf_sens_hle/div
  
  
  start_day = which(as.character(csv_sz$date_time) == paste(day, '00:10:00'))
  
  csv = csv[c(start_day:(start_day + 24*6 - 1)), ]
  # start plotting
  # associate conditions to plot and name plot
  png(filename = paste0(plot_dir, plot_name), width = 33.8, height = 19, units = 'cm', res = 500)
  plot(
    # define main data frame used in the plot
    ggplot(data = csv) +
      # create one grid for each kind of simulation
      facet_grid(sim ~ .) +
      # insert a bar geometry for schedules hvac and afn
      geom_bar(data = csv, aes(x = date_time, y = sch_hvac), stat = 'identity', fill = 'red') +
      geom_bar(data = csv, aes(x = date_time, y = sch_afn), stat = 'identity', fill = 'blue') +
      # define labs (title, x and y labs)
      labs(title = 'Análise diária detalhada',
           subtitle = paste('Apto.\n Dia', day),
           x = 'Hora',
           y = 'Joules') +
      # edit legend
      # scale_fill_discrete(name = 'Troca\nde Calor:',
      #                     labels = c('VN', 'Portas', 'Piso', 'Cobertura', 'Paredes', 'Janelas',
      #                                'HVAC', 'Cargas Internas')) +
      scale_x_datetime(date_breaks = '2 hour', date_labels = '%Hh')) +
      # edit all kind of text in the plot
      theme(legend.text = element_text(size = 14),
            legend.title = element_text(size = 15),
            legend.position = 'bottom',
            plot.title = element_text(size = 20, hjust = 0.5),
            plot.subtitle = element_text(size = 18, hjust = 0.5),
            axis.title.x = element_text(size=15),
            axis.title.y = element_text(size=15),
            axis.text.x = element_text(size=14),
            axis.text.y = element_text(size=14),
            strip.text.x = element_text(size = 17),
            strip.text.y = element_text(size = 17))
  # finish plotting
  dev.off()
# }

# plot application ####
# cgtr
plot_cgtr(df = results[['combo']][['raw']],
          plot_dir = '/home/rodox/Dropbox/00.master_ufsc/00.single_zone/02.plot/')

# diff cgtr
for (type in c('abs', 'rel')) {
  plot_diff_cgtr(df = results[['diff']][['combo']][[type]], rel = ifelse(type == 'rel', T, F),
                 plot_dir = '/home/rodox/Dropbox/00.master_ufsc/00.single_zone/02.plot/')
}
# remove unuseful variables
rm (type)

# thermal balance
for (D in c('SW', 'SE', 'E', 'NE', 'NW', 'W')) {
  if (grepl('S', D) | grepl('N', D)) {
    for (R in c('Living', 'Dorm. E', 'Dorm. W')) {
      plot_tb(df = results[['combo']][['tb']], Dwel = D, Room = R,
              plot_dir = '/home/rodox/Dropbox/00.master_ufsc/00.single_zone/02.plot/')
    }
  } else {
    for (R in c('Living', 'Dorm. S', 'Dorm. N')) {
      plot_tb(df = results[['combo']][['tb']], Dwel = D, Room = R,
              plot_dir = '/home/rodox/Dropbox/00.master_ufsc/00.single_zone/02.plot/')
    }
  }
}
# remove unuseful variables
rm (D, R)

# diff thermal balance
for (type in c('abs', 'rel')) {
  for (D in c('SW', 'SE', 'E', 'NE', 'NW', 'W')) {
    if (grepl('S', D) | grepl('N', D)) {
      for (R in c('Living', 'Dorm. E', 'Dorm. W')) {
        plot_diff_tb(results[['diff']][['combo']][['tb']][[type]], Dwel = D, Room = R,
                     ifelse(type == 'rel', T, F),
                     plot_dir = '/home/rodox/Dropbox/00.master_ufsc/00.single_zone/02.plot/')
      }
    } else {
      for (R in c('Living', 'Dorm. S', 'Dorm. N')) {
        plot_diff_tb(results[['diff']][['combo']][['tb']][[type]], Dwel = D, Room = R,
                     ifelse(type == 'rel', T, F),
                     plot_dir = '/home/rodox/Dropbox/00.master_ufsc/00.single_zone/02.plot/')
      }
    }
  }
}
# remove unuseful variables
rm(type, D, R)

