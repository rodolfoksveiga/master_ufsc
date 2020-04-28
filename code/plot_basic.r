# load libraries ####
library(ggplot2)

# auxiliar functions ####
FindDoubleWalls = function(col_names) {
  orient = c('s', 'e', 'n', 'w')
  dw = list('wall' = paste0('WALL_', toupper(orient)),
             'orient' = c('s', 'e', 'n', 'w'))
  count = sapply(dw$wall, function(x, y) sum(grepl(x, y)), col_names)
  dw = lapply(dw, function(x, y) x[which(y > 1)], count)
  
  return(dw)
}

RmDoubleWalls = function(df) {
  dw = FindDoubleWalls(colnames(df))
  if (length(dw$wall)  != 0) {
    df[paste0('surf_temp_wall_', dw$orient)] =
      apply(df[, grepl(dw$wall, colnames(df))], 1, mean)
    mark = lapply(dw$wall, function(x, y) grep(x, y), colnames(df))
    mark = unlist(mark)
    df[mark] = NULL
  }
  
  return(df)
}

PickCols = function(df, zone_name) {
  col_names = colnames(df)
  zone_name = toupper(zone_name)
  df = df[grepl('Date/Time', col_names) |
            grepl('Environment:Site Outdoor Air Drybulb', col_names) |
            ((grepl(paste0(zone_name, '_WALL'), col_names) |
                grepl(paste0(zone_name, '_FLOOR'), col_names) |
                grepl(paste0(zone_name, '_ROOF'), col_names)) &
               grepl('Inside Face Temperature', col_names)) |
            grepl(paste0(zone_name, ':Zone Mean Air Temperature'), col_names)]
  
  return(df)
}

FixColNames = function(df) {
  col_names = colnames(df)
  col_names = ifelse(grepl('Dat', col_names), 'date_time',
                     ifelse(grepl('Env', col_names), 'out_temp',
                            ifelse(grepl('Zon', col_names), 'zone_temp', col_names)))
  col_names = ifelse(grepl('WALL', col_names), paste0('surf_temp_wall_',
                                    ifelse(grepl('_S:', col_names), 's',
                                           ifelse(grepl('_E:', col_names), 'e',
                                                  ifelse(grepl('_N:', col_names), 'n', 'w')))),
                     ifelse(grepl('FLOOR', col_names), 'surf_temp_floor',
                            ifelse(grepl('ROOF', col_names), 'surf_temp_roof', col_names)))
  colnames(df) = col_names
  
  return(df)
}

OrgDF = function(df, simp) {
  df$simp = ifelse(simp == 0, 'Modelo Base', paste('Simplificação n°', as.character(simp)))
  df = reshape2::melt(df, id.vars = c('date_time', 'simp'),
                      measure.vars = colnames(df)[c(2:9)])
  df$variable = factor(df$variable,
                       levels = c('out_temp', 'zone_temp', 'surf_temp_floor', 'surf_temp_roof',
                                  'surf_temp_wall_s', 'surf_temp_wall_e', 'surf_temp_wall_n',
                                  'surf_temp_wall_w'))
  
  return(df)
}

SplApril3rd = function(df) {
  df$date_time = seq(ISOdate(19, 1, 1, 0, 10, 0), by = '10 min',
                     length.out = 365*24*6, tz = '')
  df = df[13248:(13248 + 2*24*6), ]
  
  return(df)
}

SavePlot = function(plot, plot_name, output_dir) {
  png(filename = paste0(output_dir, plot_name, '.png'),
      width = 33.8, height = 19, units = 'cm', res = 500)
  plot(plot)
  dev.off()
}

# plot functions ####
PlotHVACCount = function(input_paths, output_dir, save_plot = T) {
  dfs_list = lapply(input_paths, read.csv)
  dfs_list[[1]]$Date.Time = seq(ISOdate(2019, 1, 1, 0, 10, 0),
                                by = '10 min', length.out = 365*24*6, tz = '')
  df = cbind(dfs_list[[1]][5185:(5185+2*24*6-1), c(2, 5, 16)],
             dfs_list[[2]][5185:(5185+2*24*6-1), c(21, 22)])
  colnames(df) = c('date_time', 'occup_count', 'temp_op', 'heating', 'cooling')
  temp_op_orig = df$temp_op
  df$temp_op = df$temp_op - 24
  df$con = (df$temp_op < (26 - 24)) & (df$occup_count > 0)
  df$occup = df$occup_count > 0
  
  plot = plot(
    ggplot(data = df) +
      geom_area(aes(x = date_time, y = cooling/3600,
                    fill = 'Carga Térmica de Refrigeração (2)')) +
      geom_line(aes(x = date_time, y = temp_op*225/4,
                    colour = 'Temp. Operativa (1)'), linetype = 1) +
      geom_hline(aes(yintercept = 2*225/4, colour = 'Temp. Operativa Lim.'), linetype = 2) +
      geom_vline(aes(xintercept = as.numeric(date_time[14*6]),
                     colour = 'Schedule'), linetype = 2) +
      geom_vline(aes(xintercept = as.numeric(date_time[22*6]),
                     colour = 'Schedule'), linetype = 2) +
      geom_vline(aes(xintercept = as.numeric(date_time[24*6 + 14*6]),
                     colour = 'Schedule'), linetype = 2) +
      geom_vline(aes(xintercept = as.numeric(date_time[24*6 + 22*6]),
                     colour = 'Schedule'), linetype = 2) +
      # geom_vline(aes(xintercept = as.numeric(date_time[14*6]),
      #                colour = 'AC Contabilizado'), linetype = 2) +
      # geom_vline(aes(xintercept = as.numeric(date_time[14*6 + 1]),
      #                colour = 'AC Contabilizado'), linetype = 2) +
      geom_vline(aes(xintercept = as.numeric(date_time[24*6 + 14*6]),
                     colour = 'AC Contabilizado'), linetype = 2) +
      geom_vline(aes(xintercept = as.numeric(date_time[24*6 + 20*6 + 3]),
                     colour = 'AC Contabilizado'), linetype = 2) +
      scale_x_datetime(date_breaks = '3 hour', date_labels = '%Hh') +
      scale_y_continuous(limits = c(0, 225), breaks = seq(0, 225, 25),
                         sec.axis = sec_axis(~ . *4/225+24, breaks = seq(24, 28, 0.5),
                                             name = 'Temperatura (°C)\n')) +
      scale_fill_manual(values = c('lightblue')) +
      scale_colour_manual(values = c('purple', 'darkgreen', 'black', 'red')) +
      theme(legend.position='bottom', axis.text.x = element_text(angle = 90)) +
      labs(x = '\nHora (6 e 7 de fevereiro)',
           y = 'Carga Térmica de Refrigeração (Wh)\n',
           fill = 'Legenda:',
           colour = element_blank()) +
      theme(legend.text = element_text(size = 14),
            legend.position = 'bottom',
            legend.title = element_text(size = 15),
            axis.title.x = element_text(size = 15),
            axis.title.y = element_text(size = 15),
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12),
            strip.text.x = element_text(size = 17),
            strip.text.y = element_text(size = 17))
  )
  if (save_plot) {
    SavePlot(plot, 'hvac_count', output_dir)
  } else {
    return(plot)
  }
}

PlotSch = function(input_path, output_dir, save_plot = T) {
  df = read.csv(input_path)
  df$schedule = factor(df$schedule, levels = c('Equipamentos', 'Iluminação', 'Ocupação'))
  df$room = factor(df$room, levels = c('Sala', 'Dormitório'))
  plot = plot(
    ggplot(data = df, aes(x = time, y = value, fill = room)) +
      geom_bar(stat = 'identity', position = 'dodge') +
      facet_grid(. ~ schedule) +
      labs(x = 'Hora do Dia',
           y = 'Valor da Schedule (Adim.)',
           fill = 'Ambiente:') +
      # scale_y_discrete(limits = c(0, 1), breaks = seq(0, 1, 0.5)) +
      # scale_x_continuous(breaks = seq(0, 24, 6)) +
      theme(legend.text = element_text(size = 14),
            legend.position = 'bottom',
            legend.title = element_text(size = 15),
            axis.title.x = element_text(size = 15),
            axis.title.y = element_text(size = 15),
            axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 14),
            strip.text.x = element_text(size = 17),
            strip.text.y = element_text(size = 17))
  )
  if (save_plot) {
    SavePlot(plot, 'schedules', output_dir)
  } else {
    return(plot)
  }
}

PlotTemp = function(input_paths, zone_name, simp, output_dir, save_plot = T) {
  dfs_list = lapply(input_paths, function(x) as.data.frame(data.table::fread(x)))
  colnames(dfs_list[[2]]) = sub('HIVE_C', toupper(zone_name), colnames(dfs_list[[2]]))
  dfs_list = lapply(dfs_list, PickCols, zone_name)
  dfs_list = lapply(dfs_list, RmDoubleWalls)
  dfs_list = lapply(dfs_list, FixColNames)
  dfs_list = lapply(dfs_list, SplApril3rd)
  dfs_list = mapply(OrgDF, dfs_list, c(0, simp), SIMPLIFY = FALSE)
  df = rbind(dfs_list[[1]], dfs_list[[2]])
  
  plot = plot(
    ggplot(data = df, aes(x = date_time, y = value, colour = variable)) +
      geom_line() +
      facet_wrap(. ~ simp) +
      labs(x = 'Hora do dia (03-04/04)',
           y = 'Temperatura (°C)') +
      scale_x_datetime(date_breaks = '6 hour', date_labels = '%Hh') +
      scale_colour_manual(name = 'Temperatura:',
                          labels = c('Externa', 'Zona', 'Piso', 'Cobertura', 'Parede Sul',
                                     'Parede Leste', 'Parede Norte', 'Parede Oeste'),
                          values = c('black', 'firebrick2', 'darkgoldenrod2', 'royalblue3',
                                     'chartreuse3', 'peachpuff4', 'mediumpurple2', 'pink')) +
      theme(legend.text = element_text(size = 14),
            legend.title = element_text(size = 15),
            legend.position = 'bottom',
            axis.title.x = element_text(size=15),
            axis.title.y = element_text(size=15),
            axis.text.x = element_text(size=14),
            axis.text.y = element_text(size=14),
            strip.text.x = element_text(size = 17),
            strip.text.y = element_text(size = 17))
  )
  
  if (save_plot) {
    SavePlot(plot, paste0('case_', as.character(simp)), output_dir)
  } else {
    return(plot)
  }
}

# application ####
# hvac count
PlotHVACCount(c('', ''), '~/00.git/00.master_ufsc/04.plot_table')
# schedules
PlotSch('~/00.git/00.master_ufsc/04.plot_table/schedules.csv',
        '~/00.git/00.master_ufsc/04.plot_table')
# temperature analysis
PlotTemp(c('~/Desktop/case_3b.csv', '~/Desktop/case_3s.csv'), 'nw_liv', 3,
         '~/00.git/00.master_ufsc/04.plot_table/')
PlotTemp(c('~/Desktop/case_4b.csv', '~/Desktop/case_4s.csv'), 'sw_dorm_1', 4,
         '~/00.git/00.master_ufsc/04.plot_table/')
PlotTemp(c('~/Desktop/case_6b.csv', '~/Desktop/case_6s.csv'), 'sw_liv', 6,
         '~/00.git/00.master_ufsc/04.plot_table/')
