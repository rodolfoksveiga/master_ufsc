# load libraries and global environment ####
invisible({
  pkgs = c('caret', 'doParallel', 'dplyr', 'hydroGOF', 'Metrics', 'parallel')
  lapply(pkgs, library, character.only = T)
  inmet = read.csv('./seed/inmet_list.csv', stringsAsFactors = FALSE)
})

# base functions ####
# rename analysis index r squared
RenameRsq = function(x) sub('Rsquared', 'R²', x)
# define qualitative variables as factors
DefFactors = function(df, vars) {
  df[, vars] = lapply(df[, vars], factor)
  return(df)
}

# machine learning model functions ####
# create dummy variables
CreateDummies = function(data) {
  dummy_model = dummyVars(targ ~ ., data = data)
  dummy_data = data.frame(predict(dummy_model, newdata = data))
  dummy_data$targ = data$targ
  return(dummy_data)
}
# split data into train and test sets
SplitData = function(train, dummy_data, train_prop, seed = 100) {
  # reproduce
  set.seed(seed)
  train_part = createDataPartition(dummy_data$targ, p = train_prop, list = F)
  if(train) {
    data = dummy_data[train_part, ]
  } else {
    data = dummy_data[-train_part, ]
  }
  return(data)
}
# pre-process data
PPData = function(data, pp_model) {
  data = predict(pp_model, data)
  return(data)
}
# fit
FitModel = function(train_tech, samp_tech, nfolds, nreps,
                    train_data, eval = 'RMSE', seed = 200) {
  # reproduce results
  set.seed(seed)
  fit_ctrl = trainControl(samp_tech, nfolds, nreps, returnData = FALSE, verboseIter = TRUE)
  fit = train(targ ~ ., train_data, trControl = fit_ctrl,
              tuneLength = 20, method = train_tech, metric = eval)
  return(fit)
}
# test
TestModel = function(sim, obs) {
  test = data.frame('RMSE' = rmse(obs, sim),
                    'Rsquared' = cor(obs, sim)^2,
                    'MAE' = mae(obs, sim))
  return(test)
}

# stats and plot functions ####
# stats comparison between models
CompModels = function(models) {
  comp = resamples(models)
  comp$metrics[3] = 'R²'
  names(comp$values)[-1] = names(comp$values)[-1] %>%
    RenameRsq() %>%
    toupper()
  comp$models = toupper(comp$models)
  return(comp)
}
# plot comparison between models
PlotComp = function(models_comp, suffix, output_dir) {
  plot = bwplot(models_comp, scales = list(x = list(relation = 'free'),
                                           y = list(relation = 'free')))
  SavePlot(plot, paste0('models_comp_', suffix), output_dir)
}
# plot training process
PlotFit = function(model, train_tech, suffix, output_dir) {
  k = ifelse(nrow(model$modelInfo$parameters) == 1, 1.5, 2)
  plot = plot(model, asp = 1/k,
              main = paste0(model$modelInfo$label,
                            '\nOtimização dos Hiperparâmetros'),
              xlab = paste('Valor do Hiperparâmetro -',
                           model$modelInfo$parameters$label[1]),
              ylab = 'RMSE de validação (%)')
  SavePlot(plot, paste0('fit_', train_tech, '_', suffix), output_dir)
}
# plot predicted x real
PlotPerf = function(train_tech, pred, targ, suffix, output_dir) {
  df = data.frame(pred, targ)
  plot = ggplot(df, aes(x = targ, y = pred)) +
    geom_point(size = 0.5) +
    geom_abline(slope = 1, colour = 'red', size = 1) +
    labs(title = 'Performance do modelo', subtitle = toupper(train_tech),
         x = 'PHFT Simulado (%)', y = 'PHFT Predito (%)') +
    theme(plot.title = element_text(size = 19, hjust = 0.5),
          plot.subtitle = element_text(size = 18, face = 'bold', hjust = 0.5),
          axis.title.x = element_text(size = 15),
          axis.title.y = element_text(size = 15),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14))
  SavePlot(plot, paste0('perf_', train_tech, '_', suffix), output_dir)
}
# plot variable importances
PlotVarImp = function(model, train_tech, suffix, output_dir) {
  df = varImp(model)[[1]]
  df = data.frame(var = rownames(df), imp = df$Overall)
  plot = ggplot(df, aes(x = reorder(var, imp), y = imp)) +
    coord_flip() +
    geom_bar(stat = 'identity') +
    labs(title = paste0('Análise simplificada da influência das variáveis (',
                        toupper(train_tech), ')'),
         x = 'Importância da variável', y = 'Variável') +
    theme(plot.title = element_text(size = 19, hjust = 0.5),
          plot.subtitle = element_text(size = 18, face = 'bold', hjust = 0.5),
          axis.title.x = element_text(size = 15),
          axis.title.y = element_text(size = 15),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 11))
  SavePlot(plot, paste0('var_imp_', train_tech, '_', suffix), output_dir)
}
# save plot
SavePlot = function(plot, plot_name, output_dir, lx = 33.8, ly = 19) {
  png(filename = paste0(output_dir, plot_name, '.png'),
      width = lx, height = ly, units = 'cm', res = 500)
  plot(plot)
  dev.off()
}
# create a summary table
GenAccuracyTable = function(models, pred, targ, suffix, output_dir) {
  table = SummAccuracy %>%
    mapply(models, names(models), pred, MoreArgs = list(targ), SIMPLIFY = FALSE) %>%
    bind_rows() %>% slice(2:n())
  write.csv(table, paste0(output_dir, 'summ_table_', suffix, '.csv'), row.names = FALSE)
}
# summarise fitting results
SummAccuracy = function(model, train_tech, pred, targ) {
  best_tune = model$bestTune
  index = mapply(function(x, y, z) which(z[[4]][y] == x), best_tune,
                 names(best_tune), SIMPLIFY = FALSE, MoreArgs = list(model))
  index = Reduce(intersect, index)
  table = data.frame(Model = toupper(train_tech), Sample = c('Train', 'Test'))
  train = model[[4]][index, c('RMSE', 'Rsquared', 'MAE')]
  test = data.frame('RMSE' = rmse(targ, pred),
                    'Rsquared' = cor(targ, pred)^2,
                    'MAE' = mae(targ, pred))
  table = cbind(table, rbind(train, test, make.row.names = FALSE))
  colnames(table)[4] = 'R²'
  return(table)
}

GenMLModels = function(data_path, weather_var, nfolds, nreps, save_models,
                       save_results, models_dir, plots_dir, cores_left, inmet) {
  # load data
  raw_data = read.csv(data_path)
  str(raw_data)
  # define qualitative and quantitative variables
  qual_vars = c('seed', 'storey', 'shell_wall', 'shell_roof', 'blind', 'facade')
  quant_vars = colnames(raw_data[-c(1, length(raw_data))])
  quant_vars = quant_vars[!quant_vars %in% qual_vars]
  # edit sample
  raw_data$epw = inmet[raw_data$epw, weather_var]
  # create dummy variables
  raw_data = DefFactors(raw_data, qual_vars)
  dummy_data = CreateDummies(raw_data)
  # split data into train and test sets
  dummy_data = lapply(list('train' = TRUE, 'test' = FALSE), SplitData, dummy_data, 0.8)
  # pre-process data
  pp_model = preProcess(dummy_data$train[, quant_vars], method = c('center', 'scale'))
  dummy_data = lapply(dummy_data, PPData, pp_model)
  # train
  models_list = list(lm = 'lm', gbrt = 'blackboost', qrf = 'qrf',
                     svm = 'svmRadial', brnn = 'brnn')
  models = mclapply(models_list, FitModel, 'cv', nfolds, nreps, dummy_data$train,
                    mc.cores = detectCores() - cores_left)
  # test
  predictions = models %>%
    lapply(predict, newdata = dummy_data$test) %>%
    as.data.frame()
  # plots and results
  # stats comparison between models
  suffix = paste0(weather_var, '_', nfolds, 'folds_',
                  ifelse(is.na(nreps), 0, nreps), 'reps')
  models_comp = CompModels(models)
  models_summ = summary(models_comp)
  if (save_results) {
    # plot comparison between models
    PlotComp(models_comp, suffix, plots_dir)
    # plot training process
    mapply(PlotFit, models[-1], names(models[-1]), suffix, MoreArgs = list(plots_dir))
    # plot model performance
    mapply(PlotPerf, names(models), predictions, MoreArgs = list(dummy_data$test$targ,
                                                                 suffix, plots_dir))
    # plot variables importance
    mapply(PlotVarImp, models, names(models), MoreArgs = list(suffix, plots_dir))
    # generate accuracy table
    GenAccuracyTable(models, predictions, dummy_data$test$targ, suffix, plots_dir)
  }
  if (save_models) {
    save(models, file = paste0(models_dir, 'models_', suffix, '.rds'))
  } else {
    return(models)
  }
}

# application ####
GenMLModels('./result/sample2.csv', 'tbsm', 20, NA, TRUE, TRUE,
            './result/', './plot_table/', 0, inmet)