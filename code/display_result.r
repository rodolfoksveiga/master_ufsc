## load libraries
pkgs = c('caret', 'data.table', 'dplyr', 'forcats', 'ggplot2',
         'jsonlite', 'purrr', 'RColorBrewer', 'reshape2', 'stringr')
lapply(pkgs, library, character.only = TRUE)

## base functions
# calculate difference between full building and simplified models
CalcDiff = function(df1, df0, rel, hab) {
  label = sapply(df0, IsLabel)
  df = df1[, !label] - df0[, !label]
  if (rel) {
    df = df/abs(df0[, !label])*100
  }
  df = cbind(df, df1[, label])
  return(df)
}
# classify habitations
  # VERY INEFFICIENT FUNCTION!
  # SHOULD USE GROUP_BY AND SUMMARIZE!
CalcHab = function(df) {
  Test = function(index, df) {
    df = df[index:(index + 2), -grep('room', colnames(df))]
    label = sapply(df, IsLabel)
    df = data.frame('top_max' = max(df$top_max), 'top_min' = min(df$top_min),
                    'ph_sup' = mean(df$ph_sup), 'ph_inf' = mean(df$ph_inf),
                    'phft' = mean(df$phft), df[1, label[-length(label)]])
    label = sapply(df, IsLabel)
    df[!label] = round(df[!label], 1)
    return(df)
  }
  df = seq(1, nrow(df), 3) %>% lapply(Test, df) %>% bind_rows()
  return(df)
}
# calculate thermal balance
CalcTB = function(input_path, walls, storey, dwel, area, bwcs) {
  # define constants
  sources = c('Zone Total Internal Convective Heating Energy',
              'AFN Zone Infiltration Sensible Heat Gain Energy',
              'AFN Zone Infiltration Sensible Heat Loss Energy',
              rep('Surface Inside Face Convection Heat Gain Energy', 8))
  names(sources) = c('int', 'afn_gain', 'afn_loss', 'floor', 'roof',
                     'window', 'door', rep('wall', 4))
  spec = c('floor', 'roof', 'window', 'door')
  rooms = c('liv', 'dorm1', 'dorm2', bwcs)
  sim = str_extract(input_path, '(?<=_)\\d(?=_)')
  # edit inputs
  walls = lapply(walls, function(x) paste0(x[['label']], '_wall', x[['index']]))
  storey = paste0('f', storey)
  targs = c(rep(list(rooms), 7), walls)
  names(targs)[1:7] = names(sources)[1:7]
  # load data frame
  df = as.data.frame(fread(input_path))
  # define column names
  cols = df %>% colnames() %>% str_remove('_(INT|EXT)')
  # define inner function to calculate heat flow
  CalcHF = function(targ, type) {
    targ = targ %>% str_flatten('|')
    surf = ifelse(type %in% spec, paste0('_', type), '')
    pattern = paste0(storey, '_', dwel, '_(', targ, ')', surf, '.*') %>%
      str_to_upper() %>% paste0(':', sources[[type]])
    index =  cols %>% str_which(pattern)
    mult = ifelse(type %in% c(spec, 'wall'), -1, 1)
    heat_flow = df %>% select(all_of(index)) %>% colSums() %>% sum() %>% prod(mult)
    return(heat_flow)
  }
  # calculate heat flow
  heat_flow = mapply(CalcHF, targs, names(sources))/3600000/area
  heat_flow['afn'] = heat_flow['afn_gain'] - heat_flow['afn_loss']
  heat_flow = heat_flow[!str_detect(names(heat_flow), 'afn_(gain|loss)')]
  heat_flow = heat_flow %>% as.data.frame() %>% rename(load = '.') %>%
    tibble::rownames_to_column('source') %>% cbind(sim = sim)
  return(heat_flow)
}
# create dummy variables
CreateDummies = function(data) {
  dummy_model = dummyVars(targ ~ ., data = data)
  dummy_data = data.frame(predict(dummy_model, newdata = data))
  dummy_data$targ = data$targ
  return(dummy_data)
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
# is label?
IsLabel = function(col) is.character(col) | is.logical(col)
# differentiate
ProcessDiff = function(rel, df, hab = TRUE) {
  simps = unique(df$sim)[-1]
  df = simps %>% lapply(function(x, y) filter(y, sim == x), df) %>%
    lapply(CalcDiff, filter(df, sim == '0'), rel, hab) %>%
    bind_rows()
  return(df)
}
# select features according to sobol analysis
SelectFeats = function(data, sa_path, threshold) {
  unvar = sa_path %>%
    RJSONIO::fromJSON() %>%
    keep(names(.) %in% c('S1', 'ST')) %>%
    as.data.frame() %>%
    mutate(var = colnames(data)[-length(data)]) %>%
    filter(ST <= threshold) %>%
    pull(var)
  data = data %>%
    select(-all_of(unvar))
  return(data)
}
# pre process data to plot
RnmValues = function(df) {
  df$sim = as.character(df$sim)
  df$typo = str_to_title(df$typo)
  df$shell = ifelse(df$shell == 'ref', 'Referência',
                    ifelse(df$shell == 'tm', 'Tijolo\nMaciço',
                           ifelse(df$shell == 'tv', 'Tijolo\nVazado', 'Steel\nFrame')))
  df$position = ifelse(df$position == 'corner', 'Canto', 'Meio')
  df$orient = str_to_upper(df$orient)
  df$index = ifelse(grepl('dorm', df$room), str_extract(df$room, '(?<=dorm)[12]'), NA)
  df$room = ifelse(df$room == 'liv', 'Sala', 'Dormitório')
  df$weather = df$weather %>%
    str_replace_all('_', ' ') %>%
    str_to_title() %>%
    str_replace('(?<= )D(?=e )', 'd')
  df$weather = ifelse(df$weather == 'Rio de Janeiro', 'Rio de\nJaneiro',
                      ifelse(df$weather == 'Sao Paulo', 'São\nPaulo', df$weather))
  df = filter(df, storey %in% c(1, 3, 5))
  df$storey = ifelse(df$storey == 1, 'Térreo',
                     ifelse(df$storey == 3, 'Intermediário', 'Cobertura'))
  return(df)
}

## plot and table functions
# summarize results
SummResults = function(df, output_dir) {
  df %>%
    mutate(sim = as.factor(sim)) %>%
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
              'rmse' = EstRMSE(phft)) %>%
    mutate_at(names(.)[-1], round, 1) %>%
    write.csv(paste0(output_dir, 'simp_summ_table.csv'), row.names = FALSE)
}
# plot density of phft differences
PlotDiffPHFTDist = function(df, output_dir) {
  plot = ggplot(df, aes(x = phft, colour = sim, fill = sim)) +
    geom_density(alpha = 0.1) +
    geom_vline(xintercept = 0, colour = 'black', linetype = 'dashed') +
    labs(x = 'ΔPHFT (%)', y = 'Probabilidade',
         fill = 'Simplificação: ', colour = 'Simplificação: ') +
    theme(legend.text = element_text(size = 12),
          legend.title = element_text(size = 13),
          legend.position = 'bottom',
          axis.title.x = element_text(size = 15),
          axis.title.y = element_text(size = 15),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14))
  WritePlot(plot, 'simp_diff_phft_dist', output_dir)
}
# plot phft in simplification 2
PlotPHFTS2 = function(input_path, output_dir) {
  plot = input_path %>%
    read.csv() %>%
    filter(sim %in% c(0, 2), storey %in% c(1, 3, 5)) %>%
    mutate(orient = ifelse(str_detect(orient, '^n'), 'N', 'S')) %>%
    group_by(sim, orient, storey, weather) %>%
    summarize(mean_phft = mean(phft)) %>%
    mutate(sim = ifelse(sim == 0, 'Modelo inicial', 'S2'),
           weather = str_replace_all(weather, '_', ' ') %>%
                        str_to_title() %>%
                        str_replace('De ', 'de\n') %>%
                        str_replace('Sao ', 'São\n'),
           storey = ifelse(storey == 1, 'Térreo',
                           ifelse(storey == 3, 'Intermediário',
                                  'Cobertura'))) %>%
    ggplot(aes(x = orient, y = mean_phft, shape = storey)) +
    geom_point(size = 1.5) +
    facet_grid(sim ~ weather, scale = 'free_y') +
    labs(x = 'Orientação da UH',
         y = 'PHFT médio (%)',
         shape = 'Pavimento:') +
    theme(legend.text = element_text(size = 11),
          legend.title = element_text(size = 12),
          legend.position = 'right',
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          axis.text.x = element_text(size = 13),
          axis.text.y = element_text(size = 13),
          strip.text.x = element_text(size = 15),
          strip.text.y = element_text(size = 15))
  WritePlot(plot, 'simp2_phft', output_dir)
}
# plot heat flow in the initial model
PlotHeatFlowS0 = function(input_path, output_dir) {
  plot = input_path %>%
    read.csv() %>%
    filter(sim == 0) %>%
    mutate(room = ifelse(str_detect(room, 'dorm'), 'Dormitório', 'Sala')) %>%
    melt(id.vars = 'room', measure.vars = c('il_hg', 'afn_hg')) %>%
    mutate(variable = ifelse(variable == 'il_hg', 'Cargas\ninternas',
                             'Ventilação\nNatural')) %>%
    group_by(room, variable) %>%
    summarize(mean_cgt = abs(mean(value))) %>%
    ggplot(aes(x = variable, y = mean_cgt, fill = variable), show) +
    geom_bar(stat = 'identity', position = 'dodge',
             colour = 'black', size = 0.25) +
    facet_grid(. ~ room) +
    labs(y = 'Carga térmica (kWh/m².ano)',
         fill = 'Direção do\nfluxo de calor:') +
    scale_fill_manual(values = brewer.pal(2, 'Set1'),
                      labels = c('Entrando\nna UH', 'Saindo\nda UH')) +
    theme(legend.text = element_text(size = 10),
          legend.title = element_text(size = 12),
          legend.position = 'right',
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 14),
          axis.text.x = element_text(size = 13),
          axis.text.y = element_text(size = 13),
          strip.text.x = element_text(size = 15),
          strip.text.y = element_text(size = 15))
  WritePlot(plot, 'simp0_cgt', output_dir, 18, 7)
}
# box plot phft
PlotPHFT = function(df, output_dir) {
  plot = ggplot(df, aes(x = sim, y = phft)) +
    facet_grid(storey ~ weather) +
    geom_boxplot(outlier.shape = NA) +
    geom_jitter(aes(colour = shell), size = 0.5, alpha = 0.75) +
    labs(x = 'Simplificação',
         y = 'PHFT (%)',
         colour = 'Envoltória: ') +
    theme(legend.text = element_text(size = 11),
          legend.title = element_text(size = 12),
          legend.position = 'bottom',
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          axis.text.x = element_text(size = 13),
          axis.text.y = element_text(size = 13),
          strip.text.x = element_text(size = 16),
          strip.text.y = element_text(size = 16))
  WritePlot(plot, 'simp_phft_boxplot', output_dir, 18, 27)
}
# box plot phft differences
PlotDiffPHFT = function(df, rel, output_dir) {
  plot = ggplot(df, aes(x = sim, y = phft)) +
    facet_grid(storey ~ weather) +
    geom_boxplot(outlier.shape = NA) +
    geom_jitter(aes(colour = shell), size = 0.5, alpha = 0.75) +
    geom_hline(yintercept = 0, colour = 'black', linetype = 'dashed') +
    labs(x = 'Simplificação',
         y = ifelse(rel == FALSE, 'ΔPHFT (%)', 'ΔPHFT Relativa (%)'),
         colour = 'Envoltória: ') +
    theme(legend.text = element_text(size = 11),
          legend.title = element_text(size = 12),
          legend.position = 'bottom',
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          axis.text.x = element_text(size = 13),
          axis.text.y = element_text(size = 13),
          strip.text.x = element_text(size = 16),
          strip.text.y = element_text(size = 16))
  plot_name = paste0('simp_diff_phft_', ifelse(rel == FALSE, 'abs', 'rel'), '_boxplot')
  WritePlot(plot, plot_name, output_dir, 18, 27)
}
# define extreme cases
DefExtremes = function(df, output_dir) {
  df %>%
    group_by(sim) %>%
    slice(c(which.min(phft), which.max(phft))) %>%
    mutate_if(is.numeric, round, 2) %>%
    write.csv(paste0(output_dir, 'simp_extremes.csv'), row.names = FALSE)
}
# bar plot thermal balance
PlotTB = function(input_paths, tag, walls, storey, dwel, area, bwcs, output_dir) {
  sources = c('Cargas Internas', 'Piso', 'Cobertura', 'Janelas', 'Portas', 'Fachada Sul',
              'Fachada Leste', 'Fachada Norte', 'Fachada Oeste', 'Ventilação Natural')
  df = lapply(input_paths, CalcTB, walls, storey, dwel, area, bwcs) %>%
    bind_rows() %>%
    mutate(source = factor(source, unique(source), sources),
           sim = ifelse(sim == 0, 'Modelo inicial', paste0('Simplificação n° ', sim)))
  # plot
  plot = ggplot(data = df, aes(x = source, y = load, fill = source)) +
    facet_grid(. ~ sim) +
    geom_bar(stat = 'identity', position = 'dodge', colour = 'black', size = 0.25) +
    labs(x = NULL,
         y = 'Carga térmica (kWh/m².ano)',
         fill = 'Troca de calor: ') +
    theme(legend.text = element_text(size = 10),
          legend.title = element_text(size = 12),
          legend.position = 'right',
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          axis.text.x = element_blank(),
          axis.text.y = element_text(size = 13),
          strip.text.x = element_text(size = 15),
          strip.text.y = element_text(size = 15),
          axis.ticks = element_blank())
  plot_name = paste0('simp_tb_', tag)
  WritePlot(plot, plot_name, output_dir)
}
# histograms of the datasets inputs
PlotInputHists = function(input_path, output_dir, sample) {
  df = read.csv(input_path)
  set.seed(100)
  train_part = createDataPartition(df$targ, p = 0.8, list = FALSE)
  df$targ = NULL
  mult = ifelse(sample == 'train', 1, -1)
  psych::multi.hist(df[mult*train_part, ], density = FALSE, freq = TRUE,
                    mar = c(2, 2, 1, 1))
}
# histograms of the datasets outputs
PlotTargDist = function(input_path, output_dir) {
  df = read.csv(input_path)
  set.seed(100)
  train_part = createDataPartition(df$targ, p = 0.8, list = FALSE)
  df$label[train_part] = 'Treino'
  df$label[-train_part] = 'Teste'
  plot = ggplot(df) +
    geom_density(aes(x = targ, colour = label, fill = label), alpha = 0.1) +
    labs(x = 'PHFT (%)',
         y = 'Probabilidade',
         colour = 'Amostra: ',
         fill = 'Amostra: ') +
    theme(legend.text = element_text(size = 12),
          legend.title = element_text(size = 13),
          legend.position = 'bottom',
          axis.title.x = element_text(size = 15),
          axis.title.y = element_text(size = 15),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14))
  WritePlot(plot, 'targ_dist', output_dir)
}
# box plot database distribution according to dbt thresholds
PlotInterDist = function(input_path, output_dir) {
  df = read.csv(input_path)
  df$int = with(df, ifelse(dbt < 25, 1, ifelse(dbt >= 25 & dbt < 27, 2, 3)))
  df$int = factor(df$int, c(1, 2, 3))
  inters = c('TBSm < 25°C', '25°C ≤ TBSm < 27°C', 'TBSm ≥ 27°C')
  plot = ggplot(df) +
    geom_boxplot(aes(y = targ, x = int, fill = int)) +
    coord_flip() +
    labs(x = 'Intervalo',
         y = 'PHFT (%)') +
    scale_fill_discrete(name = 'Intervalo: ', labels = inters) +
    theme(legend.text = element_text(size = 12),
          legend.title = element_text(size = 13),
          legend.position = 'bottom',
          axis.title.x = element_text(size = 15),
          axis.title.y = element_text(size = 15),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14),
          strip.text = element_text(size = 15))
  WritePlot(plot, 'int_boxplot', output_dir)
}
# bar plot sobol effects
PlotSA = function(result_path, problem_path, output_dir) {
  vars = problem_path %>%
    read_json() %>%
    pluck('names') %>%
    unlist()
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
             stat = 'identity', position = 'dodge', colour = 'black', size = 0.1) +
    geom_text(aes(x = Variable, y = value, label = round(value, 3)), size = 2.5,
              position = position_dodge2(width = 1), hjust = -0.075) +
    geom_hline(yintercept = c(0.01, 0.02), linetype = 'dashed', colour = 'black') +
    labs(x = 'Parâmetro de entrada', y = 'Índice de sensibilidade') +
    coord_flip() +
    scale_fill_manual(name = 'Índice: ', values = brewer.pal(2, 'Paired'),
                      labels = c('Efeito de primeira ordem', 'Efeito total')) +
    theme(legend.title = element_text(size = 14),
          legend.text = element_text(size = 13),
          legend.position = 'bottom',
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          axis.text.x = element_text(size = 12),
          axis.text.y = element_text(size = 11, angle = 30))
  WritePlot(plot, 'sobol_barplot', output_dir, height = 20)
}
# plot distribution of quantitative variables after pre-processing
PlotQuantsDist = function(input_path, output_dir) {
  quant_vars = c('area', 'ratio', 'height', 'azimuth', 'abs_wall',
                 'abs_roof', 'wwr_liv', 'wwr_dorm', 'u_window', 'shgc',
                 'open_factor', 'balcony', 'dbt', 'targ')
  set.seed(100)
  data = input_path %>%
    read.csv() %>%
    select(all_of(quant_vars)) %>%
    slice(createDataPartition(targ, p = 0.8, list = FALSE))
  pp_model = preProcess(data, method = c('center', 'scale'))
  data = predict(pp_model, data)
  psych::multi.hist(data[, -ncol(data)], density = FALSE, freq = TRUE)
}
# plot train accuracy
PlotTrainAccuracy = function(input_path, output_dir) {
  plot = input_path %>%
    read.csv() %>%
    select(-'rsquared') %>%
    melt(id.vars = c('model', 'nvar')) %>%
    mutate_at(c('model', 'variable'), str_to_upper) %>%
    ggplot(aes(x = as.factor(nvar), y = value, fill = model)) +
    geom_boxplot(show.legend = FALSE) +
    facet_grid(model ~ variable, scales = 'free_x') +
    coord_flip() +
    labs(x = 'N° de variáveis consideradas',
         y = 'Valor do índice estatístico (%)') +
    theme(axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          axis.text.x = element_text(size = 13),
          axis.text.y = element_text(size = 13),
          strip.text = element_text(size = 14))
  WritePlot(plot, 'perf_train', output_dir, height = 15)
}
# plot test accuracy
PlotTestAccuracy = function(input_path, output_dir) {
  plot = input_path %>%
    read.csv() %>%
    mutate(model = str_to_upper(model)) %>%
    melt(id.vars = c('model', 'nvar')) %>%
    mutate(variable = str_to_upper(variable)) %>%
    ggplot(aes(x = model, y = value, fill = as.factor(nvar))) +
    geom_bar(stat = 'identity', position = 'dodge', colour = 'black') +
    facet_grid(. ~ variable, scales = 'free_y') +
    labs(y = 'Valor do índice estatístico (%)',
         fill = 'N° de\nvariáveis:') +
    theme(legend.title = element_text(size = 14),
          legend.text = element_text(size = 13),
          axis.title.x = element_blank(),
          axis.title.y = element_text(size = 15),
          axis.text.x = element_text(size = 14, angle = 30),
          axis.text.y = element_text(size = 14),
          strip.text = element_text(size = 15))
  WritePlot(plot, 'perf_test', output_dir)
}
# plot performance of the final model
PlotFinalPerf = function(data_path, model_path, sa_path, threshold, output_dir) {
  data = read.csv(data_path)
  qual_vars = c('seed', 'storey', 'shell_wall',
                'shell_roof', 'blind', 'facade', 'mirror')
  quant_vars = colnames(data)[-length(data)]
  quant_vars = quant_vars[!quant_vars %in% qual_vars]
  set.seed(100)
  data = data %>%
    mutate_at(qual_vars, factor) %>%
    SelectFeats(sa_path, threshold) %>%
    CreateDummies() %>%
    slice(-createDataPartition(targ, p = 0.8, list = FALSE))
  model = readRDS(model_path)
  plot = data.frame(sim = data$targ,
                    pred = predict(model, newdata = data)) %>%
    ggplot(aes(x = sim, y = pred)) +
    geom_point(size = 0.2, alpha = 0.5) +
    geom_abline(slope = 1, colour = 'red', size = 0.75) +
    labs(x = 'PHFT Simulado (%)',
         y = 'PHFT Predito (%)') +
    scale_x_continuous(limits = c(0, 100)) +
    scale_y_continuous(limits = c(0, 100)) +
    theme(legend.text = element_text(size = 12),
          legend.title = element_text(size = 13),
          legend.position = 'bottom',
          axis.title.x = element_text(size = 15),
          axis.title.y = element_text(size = 15),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14))
  WritePlot(plot, 'perf_final', output_dir)
}

# define characteristics to save the plot
WritePlot = function(plot, plot_name, output_dir, width = 18, height = 10) {
  # plot: plot variable
  # plot_name: file name (without extension)
  # output_dir: output directory
  png(filename = paste0(output_dir, plot_name, '.png'),
      width, height, units = 'cm', res = 500)
  plot(plot)
  dev.off()
}

## main functions
# display statistics of the simplifications
DisplayStats = function(input_path, output_dir) {
  # load and process data
  df = input_path %>% fread() %>% as.data.frame() %>% RnmValues()
  df = df %>% CalcHab()
  diff_dfs_list = lapply(c(FALSE, TRUE), ProcessDiff, df)
  # plot
  SummResults(diff_dfs_list[[1]], output_dir)
  PlotDiffPHFTDist(diff_dfs_list[[1]], output_dir)
  PlotPHFT(df, output_dir)
  mapply(PlotDiffPHFT, diff_dfs_list, c(FALSE, TRUE), output_dir)
  DefExtremes(diff_dfs_list[[1]], output_dir)
}
# display thermal balance of the simplifications
DisplayTB = function(output_dir) {
  # define variables to calculate thermal balances
  input_paths = list(
    caso1 = c('~/rolante/master/sorriso_0_h_tm_f1_cne.csv',
              '~/rolante/master/sorriso_1_h_tm_f1_cne.csv'),
    caso2 = c('~/rolante/master/curitiba_0_l_tm_f1_mse.csv',
              '~/rolante/master/curitiba_2_l_tm_f1_mse.csv'),
    caso3 = c('~/rolante/master/teresina_0_l_tm_f3_mnw.csv',
              '~/rolante/master/teresina_4_l_tm_f3_mnw.csv'),
    caso4 = c('~/rolante/master/rio_de_janeiro_0_l_sf_f1_mse.csv',
              '~/rolante/master/rio_de_janeiro_4_l_sf_f1_mse.csv')
  )
  walls = list(
    caso1 = list(wall1 = list(label = 'liv',
                              index = 1),
                 wall2 = list(label = c('liv', 'bwc', 'dorm1'),
                              index = c(21, 2, 2)),
                 wall3 = list(label = c('dorm1', 'dorm2', 'liv'),
                              index = c(3, 3, 34)),
                 wall4 = list(label = c('liv', 'dorm2'),
                              index = c(4, 4))),
    caso2 = list(wall1 = list(label = c('liv', 'dorm2', 'dorm1'),
                              index = c(11, 1, 12)),
                 wall2 = list(label = 'dorm1',
                              index = 21),
                 wall3 = list(label = c(rep('dorm1', 3), rep('bwc1', 2),
                                        'liv', 'bwc2', 'bwc2'),
                              index = c(31, 22, 32, 3, 4, 31, 2, 3)),
                 wall4 = list(label = c('bwc2', 'liv'),
                              index = c(4, 4))),
    caso3 = list(wall1 = list(label = c(rep('liv', 3), rep('bwc', 2), 'dorm1'),
                              index = c(11, 41, 12, 1, 2, 12)),
                 wall2 = list(label = 'dorm1',
                              index = 2),
                 wall3 = list(label = c('dorm1', 'dorm2', 'liv'),
                              index = c(31, 3, 32)),
                 wall4 = list(label = rep('liv', 2),
                              index = c(41, 42))),
    caso4 = list(wall1 = list(label = c('liv', 'dorm2', 'dorm1'),
                              index = c(11, 1, 12)),
                 wall2 = list(label = 'dorm1',
                              index = 21),
                 wall3 = list(label = c(rep('dorm1', 3), rep('bwc1', 2),
                                        'liv', 'bwc2', 'bwc2'),
                              index = c(31, 22, 32, 3, 4, 31, 2, 3)),
                 wall4 = list(label = c('bwc2', 'liv'),
                              index = c(4, 4)))
  )
  area = c(caso1 = 44.24, caso2 = 41.24, caso3 = 38.65, caso4 = 41.24)
  storey = c(caso1 = 1, caso2 = 1, caso3 = 3, caso4 = 1)
  dwel = c(caso1 = 'cne', caso2 = 'mse', caso3 = 'mnw', caso4 = 'mse')
  bwcs = list(caso1 = 'bwc', caso2 = paste0('bwc', 1:2),
              caso3 = 'bwc', caso4 = paste0('bwc', 1:2))
  mapply(PlotTB, input_paths, names(input_paths), walls, storey, dwel, area, bwcs, output_dir)
}