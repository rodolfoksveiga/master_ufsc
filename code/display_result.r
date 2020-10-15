# load libraries ####
pkgs = c('dplyr', 'forcats', 'ggplot2', 'ggthemr',
         'jsonlite', 'purrr', 'RColorBrewer', 'reshape2')
lapply(pkgs, library, character.only = TRUE)

# base function ####
# calculate difference between full building and simplified models
CalcDiff = function(df1, df0, rel, hab) {
  label = sapply(df0, IsLabel)
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
  Test = function(index, df) {
    df = df[index:(index + 2), -grep('room', colnames(df))]
    label = sapply(df, IsLabel)
    df = data.frame('top_max' = max(df$top_max), 'top_min' = min(df$top_min),
                    'ph_sup' = mean(df$ph_sup), 'ph_inf' = mean(df$ph_inf),
                    'phft' = mean(df$phft), df[1, label])
    label = sapply(df, IsLabel)
    df[!label] = round(df[!label], 1)
    return(df)
  }
  df = seq(1, nrow(df), 3) %>% lapply(Test, df) %>% bind_rows()
  return(df)
}

#classificate habitation
ClassHab = function(df) {
  reals = unique(df$shell)
  reals = reals[reals != 'Referência']
  df = reals  %>% lapply(function(x, y) filter(y, shell == x), df) %>%
    lapply(IdentMin, filter(df, shell == 'Referência')) %>%
    bind_rows()
  return(df)
}
# estimate mae
EstMAE = function(var) {
  mae = sum(abs(var))/length(var)
  return(mae)
}
# estimate rmse
EstRMSE = function(var) {
  rmse = sqrt(sum(var^2)/length(var))
  return(rmse)
}
# identify cases with performance higher than the minimum
IdentMin = function(df1, df0) {
  df1$min = df1$phft > df0$phft & df1$top_max < df0$top_max + 1 &
    !(df1$weather == 'Curitiba' & df1$top_min < df0$top_min - 1)
  return(df1)
}
# is label?
IsLabel = function(col) is.character(col) | is.logical(col)
# differentiate
ProcessDiff = function(rel, df, hab = FALSE) {
  simps = unique(df$sim)[-1]
  df = simps %>% lapply(function(x, y) filter(y, sim == x), df) %>%
    lapply(CalcDiff, filter(df, sim == '00'), rel, hab) %>%
    bind_rows()
  return(df)
}
# pre process data to plot
RnmValues = function(df) {
  df$sim = str_pad(df$sim, 2, side = 'left', pad = 0)
  df$typo = str_to_title(df$typo)
  df$shell = ifelse(df$shell == 'ref', 'Referência',
                    ifelse(df$shell == 'tm', 'Tijolo\nMaciço',
                           ifelse(df$shell == 'tv', 'Tijolo\nVazado', 'Steel\nFrame')))
  df$position = ifelse(df$position == 'corner', 'Canto', 'Meio')
  df$orient = str_to_upper(df$orient)
  df$index = ifelse(grepl('dorm', df$room), str_extract(df$room, '(?<=dorm)[12]'), NA)
  df$room = ifelse(df$room == 'liv', 'Sala', 'Dormitorio')
  df$weather = df$weather %>%
    str_replace_all('_', ' ') %>%
    str_to_title() %>%
    str_replace('(?<= )D(?=e )', 'd')
  df$storey = ifelse(df$level == 1, 'Terreo',
                     ifelse(df$level < max(df$level), 'Intermediario', 'Cobertura'))
  df$level = as.character(df$level)
  return(df)
}

# plot and table functions ####
# bar plot classifications count
BarPlotClass = function(df, output_dir) {
  df$min = ifelse(df$min == TRUE, 'Sim', 'Não')
  plot = ggplot(df, aes(x = sim, fill = min)) +
    geom_bar(stat = 'count', position = 'dodge', colour = 'black') +
    labs(x = 'Simplificação',
         y = 'Contagem',
         fill = 'Desempenho maior do que o mínimo?') +
    coord_flip() +
    scale_fill_brewer(palette = 'Dark2') +
    theme(axis.title = element_text(size = 16, face = 'bold'),
          axis.text.x = element_text(size = 14, colour = 'black'),
          axis.text.y = element_text(size = 14, colour = 'black'),
          axis.line = element_line(colour = 'grey'),
          panel.background = element_rect(fill = 'cornsilk'),
          panel.grid = element_line(linetype = 'dotted', size = 0.75, colour = 'lightgrey'),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 15),
          legend.position = 'bottom')
  WritePlot(plot, 'class_barplot', output_dir)
}
# bar plot classification differences count
BarPlotDiffClass = function(df, output_dir) {
  df = df %>% group_by(sim, weather) %>% summarize('count' = sum(min)/length(min)*100)
  plot = ggplot(df, aes(x = sim, y = count, fill = weather, label = sim)) +
    geom_bar(stat = 'identity', position = 'dodge', colour = 'black') +
    labs(x = 'Simplificação',
         y = 'Contagem (%)',
         fill = 'Clima:') +
    scale_y_continuous(breaks = seq(0, 100, 20)) +
    scale_fill_brewer(palette = 'Dark2') +
    theme(axis.title = element_text(size = 16, face = 'bold'),
          axis.text.x = element_text(size = 14, colour = 'black'),
          axis.text.y = element_text(size = 14, colour = 'black'),
          axis.line = element_line(colour = 'grey'),
          panel.background = element_rect(fill = 'cornsilk'),
          panel.grid = element_line(linetype = 'dotted', size = 0.75, colour = 'lightgrey'),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 15),
          legend.position = 'bottom')
  WritePlot(plot, 'class_diff_barplot', output_dir)
}
# bar plot thermal balance
BarPlotTB = function(df, si, tp, sh, st, po, or, ro, id, we, output_dir) {
  df = filter(df, (sim == '00' | sim == si) & typo == tp & shell == sh & storey == st &
                position == po & orient == or & room == ro & (index == id | is.na(id)) &
                weather == we)
  df$sim = paste0(ifelse(df$sim == '00', 'Modelo Base', paste0('Simplificação n° ', df$sim)))
  plot_title = paste(tp, '-', sh, '-', po, or, '-', st, '-', we)
  variables = c('il_hg', 'floor_hg', 'roof_hg', 'walls_int_hg',
                'walls_ext_hg', 'windows_hg', 'doors_hg', 'afn_hg')
  df = melt(df, id.vars = 'sim', measure.vars = variables)
  variables = c('Cargas Internas', 'Piso', 'Cobertura', 'Parede Internas',
                'Parede Externas', 'Janelas', 'Portas', 'Ventilação Natural')
  png(filename = paste0(output_dir, plot_name), width = 33.8, height = 19, units = 'cm', res = 500)
  plot = ggplot(data = df, aes(x = variable, y = value, fill = variable)) +
      facet_grid(. ~ sim) +
      geom_bar(stat = 'identity', position = 'dodge') +
      labs(title = plot_title,
           subtitle = paste(ro, ifelse(is.na(id), '', id)),
           x = NULL,
           y = 'Carga Térmica (kWh/m²)') +
      scale_fill_discrete(name = 'Troca de calor:',
                          labels = variables) +
      theme(axis.title = element_text(size = 16, face = 'bold'),
            axis.text.x = element_text(size = 14, colour = 'black'),
            axis.text.y = element_text(size = 14, colour = 'black'),
            axis.line = element_line(colour = 'grey'),
            panel.background = element_rect(fill = 'cornsilk'),
            panel.grid = element_line(linetype = 'dotted', size = 0.75, colour = 'lightgrey'),
            legend.text = element_text(size = 14),
            legend.title = element_text(size = 15),
            legend.position = 'bottom')
  plot_name = tolower(paste0('tb_', si, '_', tp, '_', sh, '_', str_sub(str_to_lower(st), 1, 3),
                             '_', str_sub(str_to_lower(po), 1, 1), str_to_lower(or), '_',
                             str_sub(str_to_lower(ro), 1, 4), '_', gsub(' ', '_', we), '.png'))
  WritePlot(plot, plot_name, output_dir)
}
# bar plot sobol effects
BarPlotSA = function(result_path, problem_path, output_dir) {
  vars = problem_path %>%
    read_json() %>%
    pluck('names') %>%
    unlist()
  ggthemr('pale', layout = 'scientific')
  plot = result_path %>%
    RJSONIO::fromJSON() %>%
    keep(names(.) %in% c('S1', 'ST')) %>%
    as.data.frame() %>%
    mutate(Variable = as.factor(vars)) %>%
    arrange(ST) %>%
    mutate(Variable = fct_inorder(Variable),
           S1 = ifelse(S1 < 0, 0, S1)) %>%
    melt() %>%
    ggplot() +
    geom_bar(aes(x = Variable, y = value, fill = variable),
             stat = 'identity', position = 'dodge', colour = 'black') +
    geom_text(aes(x = Variable, y = value, label = round(value, 3)), size = 3,
              position = position_dodge2(width = 1), hjust = -0.15) +
    geom_hline(yintercept = 0.01, linetype = 'dashed', colour = 'black') +
    labs(x = 'Input variable', y = 'Sensitivity index value (adim.)') +
    coord_flip() +
    scale_fill_manual(name = 'Legend:', values = brewer.pal(2, 'Paired'),
                      labels = c('1st order', 'Total effect')) +
    theme(axis.title = element_text(size = 16, face = 'bold'),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14, angle = 30),
          legend.title = element_text(size = 15, face = 'bold'),
          legend.text = element_text(size = 14),
          legend.position = 'bottom')
  WritePlot(plot, 'sobol_barplot', output_dir)
  ggthemr_reset()
}
# box plot phft differences
BoxPlotDiffPHFT = function(df, rel, output_dir) {
  # df: data frame with compiled results
  # output_dir: output directory
  # rel: 'TRUE' (relative) or 'FALSE' (absolute)
  plot = ggplot(df, aes(x = sim, y = phft, group = sim)) +
    facet_grid(storey ~ weather) +
    geom_boxplot(outlier.shape = 4) +
    labs(x = 'Simplificação',
         y = ifelse(rel == FALSE, 'Diff. Abs. PHFT (%)', 'Diff. Rel. PHFT (%)')) +
    scale_fill_brewer(palette = 'Dark2') +
    theme(legend.text = element_text(size = 11),
          legend.title = element_text(size = 12),
          legend.position = 'bottom',
          axis.title.x = element_text(size = 15),
          axis.title.y = element_text(size = 15),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          strip.text.x = element_text(size = 17),
          strip.text.y = element_text(size = 17))
  plot_name = ifelse(rel == FALSE, 'phft_diff_abs_boxplot', 'phft_diff_rel_boxplot')
  WritePlot(plot, plot_name, output_dir)
}
# box plot phft
BoxPlotPHFT = function(df, output_dir) {
  # df: data frame with compiled results
  # output_dir: output directory
  plot = ggplot(df, aes(x = sim, y = phft, group = sim)) +
    facet_grid(storey ~ weather) +
    geom_boxplot(outlier.shape = 4) +
    labs(x = 'Simplificação',
         y = 'PHFT (%)') +
    scale_fill_brewer(palette = 'Dark2') +
    theme(legend.text = element_text(size = 11),
          legend.title = element_text(size = 12),
          legend.position = 'bottom',
          axis.title.x = element_text(size = 15),
          axis.title.y = element_text(size = 15),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          strip.text.x = element_text(size = 17),
          strip.text.y = element_text(size = 17))
  WritePlot(plot, 'phft_simp_boxplot', output_dir)
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
DisplayMain1 = function(input_path, output_dir) {
  # load and process data
  df = input_path %>%
    fread() %>%
    as.data.frame() %>%
    bind_rows() %>%
    RnmValues()
  df = df %>% CalcHab() %>% ClassHab()
  rels = c(FALSE, TRUE)
  diff_dfs_list = lapply(rels, ProcessDiff, df, TRUE)
  # plot
  BoxPlotPHFT(df, output_dir)
  mapply(BoxPlotDiffPHFT, diff_dfs_list, rels, output_dir)
  SummResults(diff_dfs_list[[1]], output_dir)
  BarPlotClass(df, output_dir)
  BarPlotDiffClass(diff_dfs_list[[1]], output_dir)
}

DisplayMain1('./result/data_linear.csv', './plot_table/')
BarPlotSA('./result/sobol_analysis.json', './result/sobol_problem.json', './plot_table/')
