x = read.csv('/home/rodox/01.going_on/00.hive/00.hyp/06/02.sf/00.floor/00.afn/rio_de_janeiro_hyp_sf_v06_floor_afn_sw_liv.csv')
x[, 1] = NULL
# dorm
colnames(x) = c('date_time', 'site_drybulb_temp', 'int_conv_he', 'occup_count', 'conv_hge_wall_e',
                'conv_hge_door_e', 'conv_hge_wall_n', 'conv_hge_wall_s', 'conv_hge_window_s',
                'conv_hge_wall_w', 'conv_hge_floor', 'conv_hge_roof', 'mean_temp', 'op_temp',
                'afn_inf_sens_hge', 'afn_inf_sens_hle', 'afn_air_change')
# liv
colnames(x) = c('date_time', 'site_drybulb_temp', 'int_conv_he', 'occup_count', 'conv_hge_wall_e',
                'conv_hge_door_e', 'conv_hge_wall_n', 'conv_hge_wall_s', 'conv_hge_window_s',
                'conv_hge_wall_w', 'conv_hge_door_w', 'conv_hge_floor', 'conv_hge_roof',
                'mean_temp', 'op_temp', 'afn_inf_sens_hge', 'afn_inf_sens_hle', 'afn_air_change')
x[, 5:13] = -x[, 5:13]
x$conv_hge_walls = x[, 5] + x[, 7] + x[, 8] + x[, 10]
x[, c(5, 7, 8, 10)] = NULL
x$conv_hge_doors = x[, 5] + x[, 7]
x[, c(5, 7)] = NULL
x$afn_inf_sens_hge = x$afn_inf_sens_hge - x$afn_inf_sens_hle
x$afn_inf_sens_hle = NULL
x$date_time = seq(ISOdate(19, 1, 1, 0, 10, 0), by = '10 min', length.out = 365*24*6, tz='')
colnames(x)
x = x[12240:(12240+2*24*6), ]
x[, c(3, 5:8, 11, 13)] = x[, c(3, 5:8, 11, 13)]/1000
x$label = 'simp'
x = reshape2::melt(x, id.vars = c('date_time', 'label'), measure.vars = colnames(x)[c(3, 5, 6, 7, 8, 11, 13)])

# sala
y = read.csv('/home/rodox/01.going_on/00.hive/00.hyp/00/02.sf/00.floor/00.afn/rio_de_janeiro_hyp_sf_v00_floor_afn_sw_liv.csv')
y[, 1] = NULL
# dorm
colnames(y) = c('date_time', 'site_drybulb_temp', 'int_conv_he', 'occup_count', 'conv_hge_wall_e',
                'conv_hge_door_e', 'conv_hge_wall_n', 'conv_hge_wall_s', 'conv_hge_window_s',
                'conv_hge_wall_w', 'conv_hge_floor', 'conv_hge_roof', 'mean_temp', 'op_temp',
                'afn_inf_sens_hge', 'afn_inf_sens_hle', 'afn_air_change')
# liv
colnames(x) = c('date_time', 'site_drybulb_temp', 'int_conv_he', 'occup_count', 'conv_hge_wall_e',
                'conv_hge_door_e', 'conv_hge_wall_n', 'conv_hge_wall_s', 'conv_hge_window_s',
                'conv_hge_wall_w', 'conv_hge_door_w', 'conv_hge_floor', 'conv_hge_roof',
                'mean_temp', 'op_temp', 'afn_inf_sens_hge', 'afn_inf_sens_hle', 'afn_air_change')
y[, 5:12] = -y[, 5:12]
y$conv_hge_walls = y[, 5] + y[, 7] + y[, 8] + y[, 10]
y[, c(5, 7, 8, 10)] = NULL
x$conv_hge_doors = x[, 6] + x[, 11]
x[, c(6, 11)] = NULL
y$afn_inf_sens_hge = y$afn_inf_sens_hge - y$afn_inf_sens_hle
y$afn_inf_sens_hle = NULL
y$date_time = seq(ISOdate(19, 1, 1, 0, 10, 0), by = '10 min', length.out = 365*24*6, tz='')
colnames(y)
y = y[12240:(12240+2*24*6), ]
y[, c(3, 5:8, 11, 13)] = y[, c(3, 5:8, 11, 13)]/1000
y$label = 'base'
y = reshape2::melt(y, id.vars = c('date_time', 'label'), measure.vars = colnames(y)[c(3, 5, 6, 7, 8, 11, 13)])

xy = rbind(x, y)

# define main data frame used in the plot
ggplot(data = xy) +
  geom_line(data = xy, aes(x = date_time, y = value, color = variable)) +
  facet_grid(. ~ label) +
  scale_color_manual(name = 'Troca\nde Calor:',
                     labels = c('int_load', 'door', 'window', 'floor', 'roof',
                                'afn', 'walls'),
                     values = c('chartreuse3', 'darkslategray3', 'darkgoldenrod2', 'firebrick2',
                                'mediumpurple2', 'peachpuff4', 'royalblue3')) +
  scale_x_datetime(date_breaks = '6 hour', date_labels = '%Hh')
