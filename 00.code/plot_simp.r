# load library
library(ggplot2)

# process function ####
# process()
process = function(input_dirs, type) {
  # input_dirs - 
  # type - 'raw', 'diff_abs', 'diff_rel' or 'tb'
  
# # test
# input_dirs = list('s1' = '/home/rodox/00.git/00.master_ufsc/03.result/01/',
#                   's2' = '/home/rodox/00.git/00.master_ufsc/03.result/02/')
# type = 'diff_abs'
  
  # create empty lists to be filled with results files
  file_names = data = vector('list', length(input_dirs))
  # name the lists
  names(file_names) = names(data) = names(input_dirs)
  
  # read result files
  for (i in 1:length(file_names)) {
    # pick files names inside input directory
    file_names[[i]] = dir(input_dirs[[i]], paste0(type, '.csv'))
    for (j in 1:length(file_names[[i]])) {
      # count the files while they're loaded
      print(paste('i =', i, '/ j =', j))
      # load the files themselves
      data[[i]][[j]] = read.csv(paste0(input_dirs[[i]], file_names[[i]][j]))
    }
    # define proper names to the list
    names(data[[i]]) = gsub(paste0('_', type, '.csv'), '', file_names[[i]])
  }
  
  
  
}



# plot functions ####
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