# load libraries ####
pkgs = c('data.table', 'dplyr', 'ggplot2', 'ggrepel', 'scales', 'stringr')
lapply(pkgs, library, character.only = TRUE)

# base function ####
BindHabOutputs = function(grid, df) {
  hab = df %>% filter(sim == grid['sim'], typo == grid['typo'], shell == grid['shell'],
                       level == grid['level'], position == grid['position'],
                       orient == grid['orient'], weather == grid['weather'])
  hab = data.frame('phft' = mean(hab$phft), 'ph_sup' = mean(hab$ph_sup),
                    'ph_inf' = mean(hab$ph_inf), 'top_max' = max(hab$top_max),
                    'top_min' = min(hab$top_min))
  return(hab)
}
# calculate difference between full building and simplified models
CalcDiff = function(df1, df0, rel, hab) {
  # df1: data frame with full simulation results
  # df0: data frame with single zone results
  # rel: 
  label = sapply(df0, function(x) is.character(x) | is.logical(x))
  df = df1[, !label] - df0[, !label]
  if (rel) {
    df = df/abs(df0[, !label])*100
  }
  df = cbind(df, df1[, label])
  if (hab == TRUE) {
    df$min = df1$min == df0$min
  }
  return(df)
}
# classificate habitations -> VERY INEFFICIENT FUNCTION!
CalcHab = function(df) {
  # df: 
  cols = colnames(df)[sapply(df, is.character)]
  cols = cols[!cols %in% c('room', 'storey')]
  grid = df[, cols] %>% sapply(unique) %>% expand.grid() %>% as.data.frame()
  df = grid %>% apply(1, BindHabOutputs, df) %>%
    bind_rows() %>% cbind(grid)
  df[, 6:12] = apply(df[, 6:12], 2, as.character)
  return(df)
}
#classificate habitation
ClassHab = function(df) {
  # df: 
  reals = unique(df$shell)
  reals = reals[reals != 'Referência']
  df = reals  %>% lapply(function(x, y) filter(y, shell == x), df) %>%
    lapply(IdentMin, filter(df, shell == 'Referência')) %>%
    bind_rows()
  return(df)
}
# estimate mae
EstMAE = function(var) {
  # var: 
  mae = sum(abs(var))/length(var)
  return(mae)
}
# estimate rmse
EstRMSE = function(var) {
  # var: 
  rmse = sqrt(sum(var^2)/length(var))
  return(rmse)
}
# identify cases with performance higher than the minimum
IdentMin = function(df1, df0) {
  # df: 
  df1$min = df1$phft > df0$phft & df1$top_max < df0$top_max + 1 &
    !(df1$weather == 'Curitiba' & df1$top_min < df0$top_min - 1)
  return(df1)
}
# differentiate
ProcessDiff = function(rel, df, hab = FALSE) {
  # rel: 
  # df: 
  simps = unique(df$sim)[-1]
  df = simps %>% lapply(function(x, y) filter(y, sim == x), df) %>%
    lapply(CalcDiff, filter(df, sim == '00'), rel, hab) %>%
    bind_rows()
  return(df)
}
# pre process data to plot
RnmCols = function(df) {
  # df: 
  df$sim = str_pad(df$sim, 2, side = 'left', pad = 0)
  df$typo = str_to_title(df$typo)
  df$shell = ifelse(df$shell == 'ref', 'Referência',
                    ifelse(df$shell == 'tm', 'Tijolo\nMaciço',
                           ifelse(df$shell == 'tv', 'Tijolo\nVazado', 'Steel\nFrame')))
  df$position = ifelse(df$position == 'corner', 'Canto', 'Meio')
  df$orient = str_to_upper(df$orient)
  df$room = ifelse(df$room == 'liv', 'Sala', 'Dormitório')
  df$weather = df$weather %>%
    str_replace_all('_', ' ') %>%
    str_to_title() %>%
    str_replace('(?<= )D(?=e )', 'd')
  df$storey = ifelse(df$level == 1, 'Térreo',
                     ifelse(df$level < max(df$level), 'Intermediário', 'Cobertura'))
  df$level = as.character(df$level)
  return(df)
}

# plot and table functions ####
# bar plot thermal balance
BarPlotClass = function(df, output_dir) {
  # df: 
  # output_dir: 
  df$min = ifelse(df$min == TRUE, 'Sim', 'Não')
  plot = ggplot(df, aes(x = sim, fill = min)) +
    geom_bar(stat = 'count', position = 'dodge', colour = 'black') +
    labs(x = 'Simplificação',
         y = 'Contagem',
         fill = 'Desempenho maior do que o mínimo?') +
    coord_flip() +
    theme(panel.background = element_rect(colour = 'black', fill = 'white', size = 0.5),
          panel.grid.major = element_line(size = 0.25, colour = 'black'),
          panel.grid.minor = element_line(size = 0.25, colour = 'black'),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 15),
          legend.position = 'bottom',
          axis.title.y = element_text(size=15),
          axis.title.x = element_text(size=15),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          strip.text.y = element_text(size = 16))
  plot_name = 'class_barp'
  WritePlot(plot, plot_name, output_dir)
}
# bar plot thermal balance
BarPlotDiffClass = function(df, output_dir) {
  # df: 
  # output_dir: 
  df = df %>% group_by(sim, weather) %>% summarize('count' = sum(min)/length(min)*100)
  plot = ggplot(df, aes(x = sim, y = count, fill = weather, label = sim)) +
    geom_bar(stat = 'identity', position = 'dodge', colour = 'black') +
    labs(x = 'Simplificação',
         y = 'Contagem (%)',
         fill = 'Clima:') +
    scale_y_continuous(breaks = seq(0, 100, 20)) +
    theme(panel.background = element_rect(colour = 'black', fill = 'white', size = 0.5),
          panel.grid.major = element_line(size = 0.25, colour = 'black'),
          panel.grid.minor = element_line(size = 0.25, colour = 'black'),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 15),
          legend.position = 'bottom',
          axis.title.y = element_text(size=15),
          axis.title.x = element_text(size=15),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          strip.text.y = element_text(size = 16))
  plot_name = 'class_diff_barp'
  WritePlot(plot, plot_name, output_dir)
}
# box plot the differences between full building and simplified models
BoxPlotDiffPHFT = function(df, rel, output_dir) {
  # df: data frame with compiled results
  # output_dir: output directory
  # rel: 'TRUE' (relative) or 'FALSE' (absolute)
  plot = ggplot(df, aes(x = sim, y = phft, group = sim)) +
      facet_grid(storey ~ weather) +
      geom_boxplot(outlier.shape = 4) +
      labs(x = 'Simplificação',
           y = ifelse(rel == FALSE, 'Diff. Abs. PHFT (%)', 'Diff. Rel. PHFT (%)')) +
      theme(legend.text = element_text(size = 11),
            legend.title = element_text(size = 12),
            legend.position = 'bottom',
            axis.title.x = element_text(size = 15),
            axis.title.y = element_text(size = 15),
            axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 14),
            strip.text.x = element_text(size = 17),
            strip.text.y = element_text(size = 17))
  plot_name = ifelse(rel == FALSE, 'phft_diff_abs_boxp', 'phft_diff_rel_boxp')
  WritePlot(plot, plot_name, output_dir)
}
# box plot phft for all simulations
BoxPlotPHFT = function(df, output_dir) {
  # df: data frame with compiled results
  # output_dir: output directory
  plot = ggplot(df, aes(x = sim, y = phft, group = sim)) +
      facet_grid(storey ~ weather) +
      geom_boxplot(outlier.shape = 4) +
      labs(x = 'Simplificação',
           y = 'PHFT (%)') +
      theme(legend.text = element_text(size = 11),
            legend.title = element_text(size = 12),
            legend.position = 'bottom',
            axis.title.x = element_text(size = 15),
            axis.title.y = element_text(size = 15),
            axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 14),
            strip.text.x = element_text(size = 17),
            strip.text.y = element_text(size = 17))
  WritePlot(plot, 'phft_simp_boxp', output_dir)
}
# scatter plot
ScatterPlotPHFT = function(df, output_dir, col) {
  # df: 
  # output_dir: 
  # col: 
  
  if (col == 'wrap') {
    cols = c('purple', 'yellow')
    leg = c('Envoltória:')
  } else {
    cols = c('red', 'blue', 'green')
    leg = ifelse(col == 'storey', 'Pavimento:', 'Clima:')
  }
  df = data$combo$afn$red
  df_simp = filter(df, simp != 0)[, c('simp', 'comf', 'room', 'wrap', 'storey', 'weather')]
  colnames(df_simp)[2] = 'comf_simp'
  df_base = filter(df, simp == 0)[, c('simp', 'comf')]
  df = cbind('comf_base' = df_base[, 2], df_simp)
  df$simp = paste('Simp.', as.character(df$simp))
  df$room = ifelse(grepl('Dorm', df$room), 'Dorm.', 'Sala')
  df$wrap = ifelse(df$wrap == 'C10', 'Concreto (10 cm)',
                   ifelse(df$wrap == 'TV', 'Tijolo Vazado', 'Steel Frame'))
  
  plot = ggplot(data = df, aes(x = comf_base, y = comf_simp)) +
      facet_wrap(~ simp) +
      geom_point(aes(colour = df[, col], shape = room)) +
      geom_abline(intercept = 0, slope = 1, colour = 'black', linetype = 'dashed') +
      labs(x = 'Redução do PHFT da simulação (%)',
           y = 'Redução do PHFT das simplificações (%)',
           colour = leg,
           shape = 'Ambiente:') +
      scale_shape_manual(values = c(4, 19)) +
      scale_colour_manual(values = cols) +
      theme(legend.text = element_text(size = 11),
            legend.title = element_text(size = 12),
            legend.position = 'bottom',
            axis.title.x = element_text(size = 15),
            axis.title.y = element_text(size = 15),
            axis.text.x = element_text(size = 14),
            axis.text.y = element_text(size = 14),
            strip.text.x = element_text(size = 17),
            strip.text.y = element_text(size = 17))
  # write plot
  plot_name = paste0('red_phft_simp_scat_', col)
}
# summarize results
SummResults = function(df, output_dir) {
  # df: 
  # output_dir: 
  df$sim = as.factor(df$sim)
  summ = df %>%
    group_by(sim) %>%
    summarize('min' = min(phft),
              '5_percent' = quantile(phft, probs = c(0.05), names = F),
              '1_quart' = quantile(phft, probs = c(0.25), names = F),
              'mean' = mean(phft),
              'median' = median(phft),
              '3_quart' = quantile(phft, probs = c(0.75), names = F),
              '95_percent' = quantile(phft, probs = c(0.95), names = F),
              'max' = max(phft),
              'mae' = EstMAE(phft),
              'rmse' = EstRMSE(phft))
  summ[, -1] = round(summ[, -1], 1)
  write.csv(summ, paste0(output_dir, 'summ_table_simp.csv'), row.names = FALSE)
}
# define characteristics to save the plot
WritePlot = function(plot, plot_name, output_dir) {
  # plot: plot variable
  # plot_name: file name (without extension)
  # output_dir: output directory
  png(filename = paste0(output_dir, plot_name, '.png'),
      width = 33.8, height = 19, units = 'cm', res = 500)
  plot(plot)
  dev.off()
}
# main function ####
DisplayResult = function(input_dir, output_dir) {
  # input_dir: 
  # output_dir: 
  # load data
  inputs_paths = dir(input_dir, '\\.csv', full.names = TRUE)
  # process data
  df_raw = inputs_paths %>%
    lapply(function(x) as.data.frame(fread(x))) %>%
    bind_rows()
  df_raw = RnmCols(df_raw)
  rels = c(FALSE, TRUE)
  dfs_diff_list = lapply(rels, ProcessDiff, df_raw)
  df_hab = df_raw %>% CalcHab() %>% ClassHab()
  df_hab_diff = ProcessDiff(FALSE, df_hab, TRUE)
  # plot
  BoxPlotPHFT(df_raw, output_dir)
  mapply(BoxPlotDiffPHFT, dfs_diff_list, rels, output_dir)
  SummResults(dfs_diff_list[[1]], output_dir)
  BarPlotClass(df_hab, output_dir)
  BarPlotDiffClass(df_hab_diff, output_dir)
}

# application ####
DisplayResult('~/git/master_ufsc/result/', '~/git/master_ufsc/plot_table/')
