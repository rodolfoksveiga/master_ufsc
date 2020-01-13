# load libraries
library(ggplot2)

# base function ####
# process()
  # load files and define them proper names
process = function(input_dir, cond = c('afn', 'hvac'),
                   pattern = c('raw', 'diff_abs', 'diff_rel')) {
  # input_dir - 
  # cond - 
  # pattern - 'raw', 'diff_abs' or 'diff_rel'

# # test
# input_dir = '/home/rodox/00.git/00.master_ufsc/03.result/'
# cond = c('afn', 'hvac')
# pattern = c('raw', 'diff_abs', 'diff_rel')

  # create empty lists to be filled with results files
  data = vector('list', length(cond))
  data = lapply(data, function(x) x = vector('list', length = length(pattern)))
  for (i in 1:length(data)) {
    names(data)[i] = cond[i]
    for (j in 1:length(data[[i]])) {
      names(data[[i]])[j] = pattern[j]
    }
  }
  file_names = data[['combo']] = data

  # read result files
  for (i in 1:length(file_names)) {
    for (j in 1:length(file_names[[i]])) {
      # count the files while they're loaded
      print(paste('i =', i, '/ j =', j))
      # pick files names inside input directory
      file_names[[i]][[j]] = dir(input_dir, paste0('*', cond[i], '_', pattern[j], '.*csv'))
      for (k in 1:length(file_names[[i]][[j]])) {
        # load the files themselves
        data[[i]][[j]][[k]] = read.csv(paste0(input_dir, file_names[[i]][[j]][k]))
        # define proper names to the list
        names(data[[i]][[j]])[k] = sub(paste0('_', cond[i], '_', pattern[j]), '',
                                       file_names[[i]][[j]][k])
        data[['combo']][[i]][[j]] = rbind(data[['combo']][[i]][[j]], data[[i]][[j]][[k]])
      }
    }
  }
}
  

# plot functions ####
# plot_simps()
  # box plot the simplifications from the original building until the extended hive model
# bp_comf = function(df, rel = F) {
  # define main data frame used in the plot

# test
df = data$combo$afn$diff_abs
rel = F
  
  # some pre-process
  df$room = ifelse(grepl('Dorm', df$room), 'Dormitório', 'Sala')

  ggplot(data = df, aes(x = simp, y = comf, group = simp)) +
    # create one grid for each weather
    facet_grid(storey ~ weather) +
    # insert a bar box plot using 'total cooling load x dweling'
    geom_boxplot(outlier.shape = NA) +
    geom_jitter(aes(colour = wrap, shape = room)) +
    # define labs (title, x and y labs)
    labs(title = paste('Diferença', ifelse(rel == F, 'Absoluta', 'Relativa'),
                       'da Temperatura Operativa'),
         subtitle = 'Limites: ZB 1 ~ 7 (18 < Top < 26) / ZB 8 (18 < Top < 28)',
         x = 'Simplificação',
         y = ifelse(rel == F, 'Diff. Abs. PHFT (%)', 'Diff. Rel. PHFT (%)'),
         colour = 'Envoltória:',
         shape = 'Ambiente:') +
    scale_shape_manual(values = c(4, 19)) +
    # edit all kind of text in the plot
    theme(legend.text = element_text(size = 14),
          legend.title = element_text(size = 15),
          legend.position = 'bottom',
          plot.title = element_text(size = 20, hjust = 0.5),
          plot.subtitle = element_text(size = 16, hjust = 0.5),
          axis.title.x = element_text(size = 15),
          axis.title.y = element_text(size = 15),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          strip.text.x = element_text(size = 17),
          strip.text.y = element_text(size = 17))
# }




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
           y = ifelse(rel == F, 'Diff. CgTR (kWh/m²)', 'Diff. CgTR (%)'),
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