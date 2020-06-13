# load libraries ####
library(ggplot2)
library(dplyr)

# base function ####
# custom_mae()
custom_mae = function(var) {
  # var - 
  mae = sum(abs(var))/length(var)
  return(mae)
}

# custom_rmse()
custom_rmse = function(var) {
  # var - 
  rmse = sqrt(sum(var^2)/length(var))
  return(rmse)
}

# process()
  # load files and define them proper names
process = function(input_dir, cond = c('afn', 'hvac'),
                   pattern = c('raw', 'diff_abs', 'diff_rel', 'red')) {
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
      data[['combo']][[i]][[j]]$storey = factor(data[['combo']][[i]][[j]]$storey,
                                                levels = c('Cobertura', 'Intermediário',
                                                           'Térreo'))
      data[['combo']][[i]][[j]]$weather = factor(data[['combo']][[i]][[j]]$weather,
                                                 levels = c('Curitiba', 'São Paulo',
                                                            'Rio de Janeiro'))
      if (j != 3) {
        data[['combo']][[i]][[j]]$afn_inf_sens_hge =
          data[['combo']][[i]][[j]]$afn_inf_sens_hge + data[['combo']][[i]][[j]]$afn_inf_sens_hle
      } else {
        data[['combo']][[i]][[j]]$afn_inf_sens_hge = data[['combo']][[i]][[j]]$afn_inf_sens_hle
      }
      data[['combo']][[i]][[j]]$afn_inf_sens_hle = NULL
    }
  }
  
  return(data)
}

# plot functions ####
# plot_comf()
  # box plot the simplifications from the original building until the extended hive model
box_plot_comf = function(df, output_dir) {
  # df - 
  # rel - 
  
  # some pre-process
  df$room = ifelse(grepl('Dorm', df$room), 'Dormitório', 'Sala')
  df$wrap = ifelse(df$wrap == 'C10', 'Concreto\n(10 cm)',
                   ifelse(df$wrap == 'TV', 'Tijolo\nVazado', 'Steel\nFrame'))
  
  # start plotting
  # associate conditions to plot and name the plot and name the plot
  plot_name = 'phft_simp_bp.png'
  png(filename = paste0(output_dir, plot_name), width = 20, height = 31, units = 'cm', res = 500)
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
           y = 'PHFT (%)',
           colour = 'Envoltória:',
           shape = 'Ambiente:') +
      scale_shape_manual(values = c(4, 19)) +
      scale_x_continuous(breaks = 0:8) +
      # edit all kind of text in the plot
      theme(legend.text = element_text(size = 11),
            legend.title = element_text(size = 12),
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

# plot_diff_simps()
  # box plot the simplifications from the original building until the extended hive model
box_plot_diff_comf_stor = function(df, output_dir, rel = F) {
  # df - 
  # rel - 
  
  # some pre-process
  df$room = ifelse(grepl('Dorm', df$room), 'Dormitório', 'Sala')
  df$wrap = ifelse(df$wrap == 'C10', 'Concreto\n(10 cm)',
                   ifelse(df$wrap == 'TV', 'Tijolo\nVazado', 'Steel\nFrame'))
  
  # start plotting
  # associate conditions to plot and name the plot and name the plot
  plot_name = ifelse(rel == F, 'phft_diff_abs_stor_bp.png', 'phft_diff_rel_stor_bp.png')
  png(filename = paste0(output_dir, plot_name), width = 20, height = 31, units = 'cm', res = 500)
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
           y = ifelse(rel == F, 'Diff. Abs. PHFT (%)', 'Diff. Rel. PHFT (%)'),
           colour = 'Envoltória:',
           shape = 'Ambiente:') +
      scale_shape_manual(values = c(4, 19)) +
      scale_x_continuous(breaks = 1:10) +
      # edit all kind of text in the plot
      theme(legend.text = element_text(size = 11),
            legend.title = element_text(size = 12),
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

# plot_diff_simps()
# box plot the simplifications from the original building until the extended hive model
box_plot_diff_comf = function(df, output_dir, rel = F) {
  # df - 
  # rel - 
  
  # some pre-process
  df$room = ifelse(grepl('Dorm', df$room), 'Dormitório', 'Sala')
  df$wrap = ifelse(df$wrap == 'C10', 'Concreto\n(10 cm)',
                   ifelse(df$wrap == 'TV', 'Tijolo\nVazado', 'Steel\nFrame'))
  
  # start plotting
  # associate conditions to plot and name the plot and name the plot
  plot_name = ifelse(rel == F, 'phft_diff_abs_bp.png', 'phft_diff_rel_bp.png')
  png(filename = paste0(output_dir, plot_name), width = 20, height = 31, units = 'cm', res = 500)
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
           y = ifelse(rel == F, 'Diff. Abs. PHFT (%)', 'Diff. Rel. PHFT (%)'),
           colour = 'Envoltória:',
           shape = 'Ambiente:') +
      scale_shape_manual(values = c(4, 19)) +
      scale_x_continuous(breaks = 1:10) +
      # edit all kind of text in the plot
      theme(legend.text = element_text(size = 11),
            legend.title = element_text(size = 12),
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

# bp_diff_cgt()
  # box plot the simplifications from the original building until the extended hive model
box_plot_diff_cgt = function(df, rel = F, output_dir) {
  # df - 
  # rel - 
  
  # some pre-process
  df$room = ifelse(grepl('Dorm', df$room), 'Dormitório', 'Sala')
  df$wrap = ifelse(df$wrap == 'C10', 'Concreto\n(10 cm)',
                   ifelse(df$wrap == 'TV', 'Tijolo\nVazado', 'Steel\nFrame'))
  
  # start plotting
  # associate conditions to plot and name the plot and name the plot
  plot_name = ifelse(rel == F, 'cgt_diff_abs_bp.png', 'cgt_diff_rel_bp.png')
  png(filename = paste0(output_dir, plot_name), width = 20, height = 31, units = 'cm', res = 500)
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
      scale_x_continuous(breaks = 1:10) +
      # edit all kind of text in the plot
      theme(legend.text = element_text(size = 11),
            legend.title = element_text(size = 12),
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

# plot_tb()
# plot thermal balance
bar_plot_tb = function(df, si, wr, st, we, dw, ro, output_dir) {
  # df - 
  # dwel - 
  # room - 
  
  # some pre-process
  df = filter(df, (simp == 0 | simp == si) & wrap == wr & storey == st & dwel == dw &
                room == ro & weather == we)
  df$simp = paste0(ifelse(df$simp == 0, 'Modelo Base',
                          paste0('Simplificação n° ', as.numeric(df$simp))))
  # associate conditions to plot and name plot
  plot_name = tolower(paste0('tb_hyp_v0', as.numeric(si), '_', wr, '_',
                             ifelse(st == 'Térreo', 'floor',
                                    ifelse(st == 'Intermediário', 'inter', 'roof')),
                             '_', sub('. ', '_', ro), '_', dw, '_',
                             gsub(' ', '_', ifelse(we == 'São Paulo', 'sao paulo', we)), '.png'))
  # melt data to ggplot
  df = reshape2::melt(df, id.vars = 'simp', measure.vars = colnames(df)[c(1:3, 4:7, 9:11)])
  
  # start plotting
  
  png(filename = paste0(output_dir, plot_name), width = 33.8, height = 19, units = 'cm', res = 500)
  # define main data frame used in the plot
  plot(
    ggplot(data = df, aes(x = variable, y = value, fill = variable)) +
      # create one grid for each weather
      facet_grid(~ simp) +
      # insert bar columns
      geom_bar(stat = 'identity', position = 'dodge') +
      # define labs (x and y labs)
      labs(x = NULL,
           y = 'Carga Térmica (kWh/m²)') +
      # edit legend
      scale_fill_discrete(name = 'Troca de calor:',
                          labels = c('Cargas Internas', 'Piso', 'Cobertura', 'Parede Sul',
                                     'Parede Leste', 'Parede Norte', 'Parede Oeste', 'Janelas',
                                     'Portas', 'Ventilação Natural')) +
      # edit all kind of text in the plot
      theme(legend.text = element_text(size = 14),
            legend.title = element_text(size = 15),
            legend.position = 'bottom',
            axis.title.y = element_text(size=15),
            axis.text.x = element_blank(),
            axis.text.y = element_text(size=14),
            strip.text.x = element_text(size = 17),
            strip.text.y = element_text(size = 17)))
  # finish plotting
  dev.off()
}

# plot_comf()
  # box plot the simplifications from the original building until the extended hive model
scatter_plot_comf = function(df, output_dir, col) {
  # df - 
  # output_dir - 
  # col - 
  
  # define colours for plotting
  if (col == 'wrap') {
    cols = c('purple', 'yellow')
    leg = c('Envoltória:')
  } else {
    cols = c('red', 'blue', 'green')
    leg = ifelse(col == 'storey', 'Pavimento:', 'Clima:')
  }
  
  # some pre-process
  df = data$combo$afn$red
  df_simp = filter(df, simp != 0)[, c('simp', 'comf', 'room', 'wrap', 'storey', 'weather')]
  colnames(df_simp)[2] = 'comf_simp'
  df_base = filter(df, simp == 0)[, c('simp', 'comf')]
  df = cbind('comf_base' = df_base[, 2], df_simp)
  df$simp = paste('Simp.', as.character(df$simp))
  df$room = ifelse(grepl('Dorm', df$room), 'Dorm.', 'Sala')
  df$wrap = ifelse(df$wrap == 'C10', 'Concreto (10 cm)',
                   ifelse(df$wrap == 'TV', 'Tijolo Vazado', 'Steel Frame'))
  
  # start plotting
  # associate conditions to plot and name the plot and name the plot
  plot_name = paste0('red_phft_simp_scat_', col, '.png')
  png(filename = paste0(output_dir, plot_name), width = 20, height = 31, units = 'cm', res = 500)
  plot(
    # plot characteristics
    ggplot(data = df, aes(x = comf_base, y = comf_simp)) +
      # create one grid for each weather
      facet_wrap(~ simp) +
      # insert a bar box plot using 'total cooling load x dweling'
      geom_point(aes(colour = df[, col], shape = room)) +
      geom_abline(intercept = 0, slope = 1, colour = 'black', linetype = 'dashed') +
      # define labs (title, x and y labs)
      labs(x = 'Redução do PHFT da simulação (%)',
           y = 'Redução do PHFT das simplificações (%)',
           colour = leg,
           shape = 'Ambiente:') +
      scale_shape_manual(values = c(4, 19)) +
      scale_colour_manual(values = cols) +
      # edit all kind of text in the plot
      theme(legend.text = element_text(size = 11),
            legend.title = element_text(size = 12),
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
summ_table = function(df, output_dir) {
  # df - 
  # output_dir - 
  
  summ_raw = as.data.frame(matrix(nrow = 8*2*6, ncol = 13))
  data_summ = df[, c(1:3, 8:17, 19, 24)]
  colnames(summ_raw) = colnames(data_summ[, 1:13])
  data_summ$room = ifelse(grepl('Dorm', data_summ$room), 'Dormitório', 'Sala')
  
  n = 0
  for (s in unique(data_summ$simp)) {
    for (r in unique(data_summ$room)) {
      data_filt = filter(data_summ, simp == s & room == r)
      data_filt = data_filt[, 1:13]
      for (col in colnames(data_filt)) {
        for (i in 1:6) {
          summ_raw[n + i, col] = summary(data_filt[, col])[i]
        }
      }
      n = n + 6
    }
  }
  summ_raw = round(summ_raw, 1)
  
  write.csv(summ_raw, paste0(output_dir, 'summ_table_raw.csv'))
}

# summ_simp()
summ_simp = function(df, output_dir) {
  # df - 
  # output_dir - 
  
  summ_simp = df %>%
    group_by(simp) %>%
    summarize('min' = min(comf),
              '5_percent' = quantile(comf, probs = c(0.05), names = F),
              '1_quart' = quantile(comf, probs = c(0.25), names = F),
              'mean' = mean(comf),
              'median' = median(comf),
              '3_quart' = quantile(comf, probs = c(0.75), names = F),
              '95_percent' = quantile(comf, probs = c(0.95), names = F),
              'max' = max(comf),
              'mae' = custom_mae(comf),
              'rmse' = custom_rmse(comf)) %>%
    as.data.frame() %>%
    round(1)
  
  write.csv(summ_simp, paste0(output_dir, 'summ_table_simp.csv'))
}

# application ####
output_dir = '/home/rodox/git/master_ufsc/plot_table/'
data = process('/home/rodox/git/master_ufsc/result/')
summ_table(data$combo$afn$raw, output_dir)
summ_simp(data$combo$afn$diff_abs, output_dir)
box_plot_comf(df = data$combo$afn$raw, output_dir)
box_plot_diff_comf(df = data$combo$afn$diff_abs, rel = F, output_dir)
box_plot_diff_comf(df = data$combo$afn$diff_rel, rel = T, output_dir)
box_plot_diff_comf_stor(df = data$combo$afn$diff_abs, rel = F, output_dir)
box_plot_diff_comf_stor(df = data$combo$afn$diff_rel, rel = T, output_dir)
colours = c('wrap', 'storey', 'weather')
for (colour in colours) {
  scatter_plot_comf(data$combo$afn$red, output_dir, col = colour)
}
tb_cases = list(
  c('1', 'C10', 'Intermediário', 'Rio de Janeiro', 'SE', 'Sala'),
  c('2', 'SF', 'Térreo', 'São Paulo', 'NO', 'Sala'),
  c('3', 'C10', 'Intermediário', 'Curitiba', 'NO', 'Sala'),
  c('4', 'SF', 'Térreo', 'Rio de Janeiro', 'SE', 'Sala'),
  c('4', 'SF', 'Térreo', 'Rio de Janeiro', 'SO', 'Dorm. 1'),
  c('5', 'SF', 'Térreo', 'Rio de Janeiro', 'SO', 'Sala'),
  c('6', 'C10', 'Cobertura', 'Curitiba', 'NE', 'Sala'),
  c('6', 'SF', 'Térreo', 'Rio de Janeiro', 'SO', 'Sala'),
  c('7', 'SF', 'Térreo', 'Rio de Janeiro', 'SO', 'Dorm. 1')
)
for (case in tb_cases) {
  bar_plot_tb(data$combo$afn$raw, as.numeric(case[1]), case[2],
              case[3], case[4], case[5], case[6], output_dir)
}
for (case in tb_cases) {
  print(filter(data$combo$afn$diff_abs, simp == as.numeric(case[1]) & wrap == case[2] &
                 storey == case[3] & weather == case[4] & dwel == case[5] & room == case[6]))
  print('--------------')
  print('--------------')
  print('--------------')
}

