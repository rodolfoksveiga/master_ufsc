# load libraries ####
library(ggplot2)
library(dplyr)

# base function ####
# process()
  # load files and define them proper names
process = function(input_dir, cond = c('afn', 'hvac'),
                   pattern = c('raw', 'diff_abs', 'diff_rel')) {
  # input_dir - 
  # cond - 
  # pattern - 'raw', 'diff_abs' or 'diff_rel'
  
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
      file_names[[i]][[j]] = dir(input_dir, paste0(cond[i], '_', pattern[j], '.csv'))
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
  
  return(data)
}

# plot functions ####
# plot_simps()
  # box plot the simplifications from the original building until the extended hive model
bp_comf = function(df, rel = F, output_dir) {
  # df - 
  # rel - 
  
  # some pre-process
  df$room = ifelse(grepl('Dorm', df$room), 'Dormitório', 'Sala')
  
  # start plotting
  # associate conditions to plot and name the plot and name the plot
  plot_name = ifelse(rel == F, 'phto_diff_abs.png', 'phto_diff_rel.png')
  png(filename = paste0(output_dir, plot_name), width = 33.8, height = 19, units = 'cm', res = 500)
  plot(
    # plot characteristics
    ggplot(data = df, aes(x = simp, y = comf, group = simp)) +
      # create one grid for each weather
      facet_grid(storey ~ weather) +
      # insert a bar box plot using 'total cooling load x dweling'
      geom_boxplot(outlier.shape = NA) +
      geom_jitter(aes(colour = wrap, shape = room)) +
      # define labs (title, x and y labs)
      labs(x = 'Simplificação',
           y = ifelse(rel == F, 'Diff. Abs. PHTO18-26 (%)', 'Diff. Rel. PHTO18-26 (%)'),
           colour = 'Envoltória:',
           shape = 'Ambiente:') +
      scale_shape_manual(values = c(4, 19)) +
      scale_x_continuous(breaks = 1:7) +
      # edit all kind of text in the plot
      theme(legend.text = element_text(size = 14),
            legend.title = element_text(size = 15),
            legend.position = 'bottom',
            axis.title.x = element_text(size = 15),
            axis.title.y = element_text(size = 15),
            axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 14),
            strip.text.x = element_text(size = 17),
            strip.text.y = element_text(size = 17))
  )
  # finish plotting
  dev.off()
}

# bp_cgt()
  # box plot the simplifications from the original building until the extended hive model
bp_cgt = function(df, rel = F, output_dir) {
  # df - 
  # rel - 
  
  # some pre-process
  df$room = ifelse(grepl('Dorm', df$room), 'Dormitório', 'Sala')
  
  # start plotting
  # associate conditions to plot and name the plot and name the plot
  plot_name = ifelse(rel == F, 'cgt_diff_abs.png', 'cgt_diff_rel.png')
  png(filename = paste0(output_dir, plot_name), width = 33.8, height = 19, units = 'cm', res = 500)
  plot(
    # plot characteristics
    ggplot(data = df, aes(x = simp, y = hvac_total_ce, group = simp)) +
      # create one grid for each weather
      facet_grid(storey ~ weather) +
      # insert a bar box plot using 'total cooling load x dweling'
      geom_boxplot(outlier.shape = NA) +
      geom_jitter(aes(colour = wrap, shape = room)) +
      # define labs (title, x and y labs)
      labs(x = 'Simplificação',
           y = ifelse(rel == F, 'Diff. Abs. CTR (kWh/m²)', 'Diff. Rel. CTR (%)'),
           colour = 'Envoltória:',
           shape = 'Ambiente:') +
      scale_shape_manual(values = c(4, 19)) +
      scale_x_continuous(breaks = 1:7) +
      # edit all kind of text in the plot
      theme(legend.text = element_text(size = 14),
            legend.title = element_text(size = 15),
            legend.position = 'bottom',
            axis.title.x = element_text(size = 15),
            axis.title.y = element_text(size = 15),
            axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 14),
            strip.text.x = element_text(size = 17),
            strip.text.y = element_text(size = 17))
  )
  # finish plotting
  dev.off()
}

# table functions ####
summ_table = function(data, output_dir) {
  df = as.data.frame(matrix(nrow = 7*2*6, ncol = 12))
  data_summ = data.frame('afn_inf_sens' = data$combo$afn$raw$afn_inf_sens_hge +
                           data$combo$afn$raw$afn_inf_sens_hle,
                         'int_conv_he' = data$combo$afn$raw$int_conv_he,
                         'conv_hge_walls' = data$combo$afn$raw$conv_hge_walls,
                         'conv_hge_floor' = data$combo$afn$raw$conv_hge_floor,
                         'conv_hge_roof' = data$combo$afn$raw$conv_hge_roof,
                         'conv_hge_fens' = data$combo$afn$raw$conv_hge_windows +
                           data$combo$afn$raw$conv_hge_doors,
                         'afn_air_change' = data$combo$afn$raw$afn_air_change,
                         'temp_95' = data$combo$afn$raw$temp_95,
                         'uncomf_cold' = data$combo$afn$raw$uncomf_cold,
                         'uncomf_hot' = data$combo$afn$raw$uncomf_hot,
                         'comf' = data$combo$afn$raw$comf,
                         'hvac_total_ce' = data$combo$hvac$raw$hvac_total_ce,
                         'simp' = data$combo$afn$raw$simp,
                         'room' = data$combo$afn$raw$room)
  colnames(df) = colnames(data_summ[, 1:12])
  data_summ$room = ifelse(grepl('Dorm', data_summ$room), 'Dormitório', 'Sala')
  
  n = 0
  for (s in unique(data_summ$simp)) {
    for (r in unique(data_summ$room)) {
      data_filt = filter(data_summ, simp == s & room == r)
      data_filt = data_filt[, 1:12]
      for (col in colnames(data_filt)) {
        for (i in 1:6) {
          df[n + i, col] = summary(data_filt[, col])[i]
        }
      }
      n = n + 6
    }
  }
  df = round(df, 1)
  
  write.csv(df, paste0(output_dir, 'summary_table_raw.csv'))
}


# application ####
data = process('/home/rodox/00.git/00.master_ufsc/03.result/')
bp_comf(df = data$combo$afn$diff_abs, rel = F,
        output_dir = '/home/rodox/00.git/00.master_ufsc/04.plot/')
bp_cgt(df = data$combo$hvac$diff_abs, rel = F,
       output_dir = '/home/rodox/00.git/00.master_ufsc/04.plot/')
create_table(data, output_dir = '/home/rodox/00.git/00.master_ufsc/04.plot/')





# scatter plot!

data$combo$afn$raw$room = ifelse(grepl('Dorm', data$combo$afn$raw$room), 'Dorm', 'Sala')

ggplot(data = subset(data$combo$afn$raw, simp == 6)) +
  # create one grid for each weather
  facet_grid(storey ~ weather) +
  # insert a bar box plot using 'total cooling load x dweling'
  geom_point(aes(x = subset(data$combo$afn$raw, simp == 0)$comf,
                 y = subset(data$combo$afn$raw, simp == 6)$comf,
                 colour = wrap, shape = room)) +
  geom_abline(intercept = 0, slope = 1, colour = 'black', linetype = 'dashed') +
  # define labs (title, x and y labs)
  labs(x = 'PHTO18-26 (%) - Simulação',
       y = 'PHTO18-26 (%) - Simplificação',
       colour = 'Envoltória:',
       shape = 'Ambiente:') +
  scale_shape_manual(values = c(4, 19)) +
  # edit all kind of text in the plot
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 15),
        legend.position = 'bottom',
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        axis.text.x = element_text(size = 14),
        axis.text.y = element_text(size = 14),
        strip.text.x = element_text(size = 17),
        strip.text.y = element_text(size = 17))

