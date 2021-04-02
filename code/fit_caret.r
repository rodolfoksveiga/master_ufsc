## load libraries and global environment
invisible({
  pkgs = c('caret', 'doParallel', 'data.table', 'dplyr',
           'ggplot2', 'hydroGOF', 'Metrics', 'parallel', 'purrr')
  # required packages to fit models
  # pkgs = c(pkgs, 'brnn', 'kernlab', 'plyr', 'randomForest', 'xgboost')
  lapply(pkgs, library, character.only = TRUE)
})

## functions
# rename analysis index r squared
RenameRsq = function(x) sub('Rsquared', 'R²', x)
# select features according to sobol analysis
SelectFeats = function(data, sa_path, threshold) {
  # define variables to be removed (low sensitivity)
  unvar = sa_path %>% # define sensitivity analysis path
    RJSONIO::fromJSON() %>% # load data (json is loaded as a list)
    keep(names(.) %in% c('S1', 'ST')) %>% # keep only the total index
    as.data.frame() %>% # transform list into a dataframe
    mutate(var = colnames(data)[-length(data)]) %>% # create a column with variable names
    filter(ST <= threshold) %>% # filter variables with sensitivity index lower than threshold
    pull(var) # select the column with the names of the variables left
  data = select(data, -all_of(unvar)) # remove variables with low sensitivity
  return(data)
}
# create dummy variables
CreateDummies = function(data) {
  # create a dummy model, considering "targ" as output
  dummy_model = dummyVars(targ ~ ., data = data)
  # apply the model to the dataset
  dummy_data = data.frame(predict(dummy_model, newdata = data))
  # attach the output to the dummy data
  dummy_data$targ = data$targ
  return(dummy_data)
}
# split data into train and test sets
SplitData = function(train, data, train_prop, seed = 100) {
  # assure reproducibility
  set.seed(seed)
  # define a logical vector with train indexes
  train_part = createDataPartition(data$targ, p = train_prop, list = FALSE)
  # select the train/test partitions
  if(train) { # train partition
    data = data[train_part, ]
  } else { # test partition
    data = data[-train_part, ]
  }
  return(data)
}
# fit machine learning model
FitModel = function(train_tech, tune_grid, nfolds, train_data,
                    cores_left, eval = 'RMSE', seed = 200) {
  # define number of cores
  cores = detectCores() - cores_left
  # start parallel processing
  registerDoParallel(cores)
  # define training properties
  fit_ctrl = trainControl('cv', nfolds, search = 'grid',
                          returnData = FALSE, verboseIter = TRUE)
  # assure reproducibility
  set.seed(seed)
  # train model
  fit = train(targ ~ ., train_data, trControl = fit_ctrl, tuneLength = 2,
              method = train_tech, metric = eval, preProcess = 'BoxCox')
  # stop parallel processing
  registerDoSEQ()
  # clean memory
  gc()
  return(fit)
}
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
  plot = bwplot(models_comp, main = 'Models Training Accuracy',
                scales = list(x = list(relation = 'free'), y = list(relation = 'free')))
  SavePlot(plot, paste0('models_comp_', suffix), output_dir)
}
# plot training process
PlotFit = function(model, train_tech, suffix, output_dir) {
  k = ifelse(nrow(model$modelInfo$parameters) == 1, 1.5, 2)
  plot = plot(model, asp = 1/k,
              main = paste0(model$modelInfo$label, '\nHyperparameters Optimization'),
              xlab = paste('Hyperparameter Value -', model$modelInfo$parameters$label[1]),
              ylab = 'Validation RMSE (%)')
  SavePlot(plot, paste0('fit_', train_tech, '_', suffix), output_dir)
}
# plot predicted against real
PlotPerf = function(train_tech, pred, targ, suffix, output_dir) {
  df = data.frame(pred, targ)
  plot = ggplot(df, aes(x = targ, y = pred)) +
    geom_point(size = 0.5) +
    geom_abline(slope = 1, colour = 'red', size = 1) +
    labs(title = 'Model Testing Accuracy', subtitle = toupper(train_tech),
         x = 'Simulated Energy Consumption (kWh/m².year)',
         y = 'Predicted Energy Consumption (kWh/m².year)') +
    theme(plot.title = element_text(size = 19, hjust = 0.5),
          plot.subtitle = element_text(size = 18, face = 'bold', hjust = 0.5),
          axis.title.x = element_text(size = 15),
          axis.title.y = element_text(size = 15),
          axis.text.x = element_text(size = 14),
          axis.text.y = element_text(size = 14))
  SavePlot(plot, paste0('perf_', train_tech, '_', suffix), output_dir)
}
# plot variable importances
PlotVarImp = function(train_tech, prediction, predictors, suffix, output_dir) {
  df = predictors %>%
    select(-targ) %>%
    filterVarImp(prediction) %>%
    setDT(keep.rownames = TRUE)
  plot = ggplot(df, aes(x = reorder(rn, Overall), y = Overall)) +
    coord_flip() +
    geom_bar(stat = 'identity') +
    labs(title = paste0('Simplified analysis of the variables influences\n',
                        toupper(train_tech)),
         x = 'Variable', y = 'T-Value') +
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
# summarize fitting results
SummAccuracy = function(model, train_tech, pred, targ) {
  best_tune = model$bestTune
  index = mapply(function(x, y, z) which(z[[4]][y] == x), best_tune,
                 names(best_tune), SIMPLIFY = FALSE, MoreArgs = list(model))
  index = Reduce(intersect, index)
  table = data.frame(Model = toupper(train_tech), Sample = c('Train', 'Test'))
  train = model[[4]][index, c('MAE', 'RMSE', 'Rsquared')]
  test = data.frame('MAE' = mae(targ, pred),
                    'RMSE' = rmse(targ, pred),
                    'Rsquared' = cor(targ, pred)^2)
  table = cbind(table, rbind(train, test, make.row.names = FALSE))
  colnames(table)[5] = 'R²'
  table[1:2] = sapply(table[1:2], as.character)
  return(table)
}
# create a summary table
GenAccuracyTable = function(models, pred, targ, suffix, output_dir) {
  SummAccuracy %>%
    mapply(models, names(models), pred, MoreArgs = list(targ), SIMPLIFY = FALSE) %>%
    bind_rows() %>% slice(2:n()) %>%
    write.csv(paste0(output_dir, 'summ_table_', suffix, '.csv'), row.names = FALSE)
}
# write results from hyperparameters search
WriteSearchResults = function(model, tag, output_dir) {
  output_path = paste0(output_dir, 'hps_', tag, '_', suffix, '.csv')
  write.csv(model$results, output_path, row.names = FALSE)
}

## variables
# path to the dataset
data_path = 'result/sample.csv'
# path to the sensitivity analysis output (json)
sa_path = 'result/sobol_analysis.json'
# sensitivity analysis threshold to select features
sa_threshold = 0.02
# number of folders used on cross-validation
nfolds = 2
# hyperparameters grid
tune_grid = list(
  lm = NULL,
  brnn = expand.grid(.neurons = seq(25, 45, 2)),
  svm = expand.grid(.sigma = 0.01,
                    .C = 2^(1:8)),
  rf = expand.grid(.mtry = seq(5, 35, 5)),
  xgbt = expand.grid(.nrounds = seq(200, 600, 100),
                     .max_depth = c(6, 8, 10),
                     .eta = c(0.3, 0.4),
                     .gamma = 0,
                     .colsample_bytree = c(0.6, 0.8),
                     .min_child_weight = 1,
                     .subsample = 1)
)
# directory to write plots and tables
pt_dir = 'plot_table/'
# directory to write machine learning models
models_dir = 'result/'
# number of cores not to use
cores_left = 0

## main code
# load data
raw_data = read.csv(data_path)
# define qualitative and quantitative variables
qual_vars = c('seed', 'storey', 'shell_wall',
              'shell_roof', 'blind', 'facade', 'mirror')
# define quantitative variables (exclude the output variable, named as "targ")
quant_vars = colnames(raw_data[-length(raw_data)])
quant_vars = quant_vars[!quant_vars %in% qual_vars]
# transform qualitative variables in factors
raw_data[, qual_vars] = lapply(raw_data[, qual_vars], factor)
# select features according to sensitivity analysis
raw_data = SelectFeats(raw_data, sa_path, sa_threshold)
# create a dummy dataset with dummy variables
dummy_data = CreateDummies(raw_data)
# split data into train and test sets
raw_data = lapply(list('train' = TRUE, 'test' = FALSE), SplitData, raw_data, 0.005)
dummy_data = lapply(list('train' = TRUE, 'test' = FALSE), SplitData, dummy_data, 0.005)
# train models
models_list = list(lm = 'lm', ann = 'brnn')
models = mapply(FitModel, models_list, list(NULL), SIMPLIFY = FALSE,
                MoreArgs = list(nfolds, dummy_data$train, cores_left))
# define suffix tag
suffix = paste0('nvar', ncol(raw_data$train))
# define a table with training performances
models_comp = CompModels(models)
models_summ = summary(models_comp)
# plot comparison between models
PlotComp(models_comp, suffix, pt_dir)
# test models
predictions = models %>%
  lapply(predict, newdata = dummy_data$test) %>%
  as.data.frame()
# generate accuracy table
GenAccuracyTable(models, predictions, dummy_data$test$targ, suffix, pt_dir)
# plot model performance
mapply(PlotPerf, names(models), predictions,
       MoreArgs = list(dummy_data$test$targ, suffix, pt_dir))
# write models
saveRDS(models, file = paste0(models_dir, 'models_', suffix, '.rds'))