library(ggplot2)

# define directory to save plots
plot_dir = '/home/rodox/GoogleDrive/01.labeee/00.master_project/02.basic_plots/'

# estimativa dos outputs
df_vn = read.csv(paste0('/home/rodox/Desktop/99/00.c10/01.inter/00.afn/',
                        'curitiba_hyp_c10_v99_inter_afn_nw_liv.csv'))
df_vn$Date.Time = seq(ISOdate(2019, 1, 1, 0, 10, 0), by = '10 min', length.out = 365*24*6, tz = '')
df_ac = read.csv(paste0('/home/rodox/Desktop/99/00.c10/01.inter/01.hvac/',
                        'curitiba_hyp_c10_v99_inter_hvac_nw_liv.csv'))
df = cbind(df_vn[5185:(5185+2*24*6-1), c(2, 5, 16)], df_ac[5185:(5185+2*24*6-1), c(21, 22)])
rm(df_vn, df_ac)
colnames(df) = c('date_time', 'occup_count', 'temp_op', 'heating', 'cooling')
temp_op_orig = df$temp_op
df$temp_op = df$temp_op - 24
df$con = (df$temp_op < (26 - 24)) & (df$occup_count > 0)
df$occup = df$occup_count > 0

plot_name = 'hvac_count.png'
png(filename = paste0(plot_dir, plot_name), width = 33.8, height = 16, units = 'cm', res = 500)
plot(
  ggplot(data = df) +
    geom_area(aes(x = date_time, y = cooling/3600, fill = 'Carga Térmica de Refrigeração (2)')) +
    geom_line(aes(x = date_time, y = temp_op*225/4, colour = 'Temp. Operativa (1)'), linetype = 1) +
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
dev.off()

# schedules
df = read.csv('/home/rodox/GoogleDrive/01.labeee/00.master_project/02.basic_plots/schedules.csv')
df$schedule = factor(df$schedule, levels = c('Equipamentos', 'Iluminação',
                                             'Ocupação', 'Climatização'))
df$room = factor(df$room, levels = c('Sala', 'Dormitório'))

plot_name = 'schedules.png'
png(filename = paste0(plot_dir, plot_name), width = 33.8, height = 11, units = 'cm', res = 500)
plot(
  ggplot(data = df, aes(x = time, y = value, fill = room)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    facet_grid(. ~ schedule) +
    labs(x = 'Hora do Dia',
         y = 'Valor da Schedule (Adim.)',
         fill = 'Ambiente:') +
    scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.5)) +
    scale_x_continuous(breaks = seq(0, 24, 6)) +
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

dev.off()
