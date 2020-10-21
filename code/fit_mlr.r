# notes ####
  # # simple train
  # model = train(learner = , task = regr, measures = list(mae, rmse))
  # # tune model
  # # resampling train
  # model = resample(learner = , task = regr,
  #                  resampling = cv, measures = list(mae, rmse))
  # # tune train
  # model = tuneParams(learner = svm_learner, task = regr, resampling = cv,
  #                    measures = list(mae, rmse, rsq), par.set = svm_hps,
  #                    control = random_search)
  # # nested cross-validation
  # cv_in = makeResampleDesc(method = 'Holdout', split = 4/5)
  # cv_out = makeResampleDesc(method = 'CV', iters = 4)
  # nnet_wrapper = makeTuneWrapper(learner = nnet_learner, resampling = cv_in,
  #                                par.set = nnet_hps, control = random_search,
  #                                measures = list(mae, rmse, rsq))
  # model = resample(learner = nnet_wrapper, task = regr, resampling = cv_out,
  #                  measures = list(mae, rmse, rsq), extract = getTuneResult)
  # # svm
  # svm_hps = makeParamSet(makeDiscreteParam(id = 'kernel', values = c('polynomial', 'radial')),
  #                        makeIntegerParam(id = 'degree', lower = 1, upper = 5,
  #                                         requires = quote(kernel == 'polynomial')),
  #                        makeNumericParam(id = 'cost', lower = 5, upper = 20),
  #                        makeNumericParam(id = 'gamma', lower = 0, upper = 2),
  #                        makeNumericParam(id = 'nu', lower = -2, upper = 2))
  # svm_learner = makeLearner(id = 'svm', cl = 'regr.svm')
  # # brnn
  # brnn_hps = makeParamSet(
  #   makeIntegerParam(id = 'neurons', lower = 5, upper = 20)
  # )
  # brnn_learner = makeLearner(id = 'brnn', cl = 'regr.brnn')

# load libraries and global environment ####
invisible({
  pkgs = c('dplyr', 'mlr', 'parallel', 'parallelMap')
  lapply(pkgs, library, character.only = TRUE)
  inmet = read.csv('./source/inmet_list.csv', stringsAsFactors = FALSE)
})

# variables ####
data_path = './result/sample.csv'
weather_var = 'tbsm'
cores_left = 0

# base functions ####
# split data into train and test sets
SplitData = function(is_train, data, train_prop, seed = 100) {
  # reproduce
  set.seed(seed)
  index = caret::createDataPartition(data$targ, p = train_prop, list = FALSE)
  if(is_train) {
    data = data[index, ]
  } else {
    data = data[-index, ]
  }
  return(data)
}
# pre-process data
PPData = function(data, target, method) {
  data %>%
    createDummyFeatures(target = target) %>%
    normalizeFeatures(target = target, method = method)
}

# main code ####
# load data
raw_data = read.csv(data_path, stringsAsFactors = FALSE)
str(raw_data)
# define qualitative and quantitative variables
qual_vars = c('seed', 'storey', 'shell_wall', 'shell_roof', 'blind', 'facade')
quant_vars = colnames(raw_data[-length(raw_data)])
quant_vars = quant_vars[!quant_vars %in% qual_vars]
# add weather variable values
raw_data$epw = inmet[raw_data$epw, weather_var]
# create dummy variables
raw_data[, qual_vars] = lapply(raw_data[, qual_vars], factor)
# split data
raw_data = lapply(list(train = TRUE, test = FALSE), SplitData, raw_data, 1/100)
# pre-process features
dummy_data = lapply(raw_data, PPData, 'targ', 'standardize')
# define regression task
regr = makeRegrTask('phft', data = dummy_data$train, target = 'targ')
# define hyperparameters
nnet_hps = makeParamSet(makeDiscreteParam(id = 'maxit', values = 1000),
                        makeDiscreteParam(id = 'MaxNWts', values = 10000),
                        makeDiscreteParam(id = 'size', values = c(20)),
                        # makeDiscreteParam(id = 'rang', values = c(0.7, 1, 1.5)),
                        makeDiscreteParam(id = 'decay', values = c(0.1, 1, 10)))
# define search type
grid_search = makeTuneControlGrid()
# define learner
nnet_learner = makeLearner(id = 'nnet', cl = 'regr.nnet')
# define resampling technique
cv = makeResampleDesc(method = 'CV', iters = 4)
# start multi cores parallelization
cores = detectCores() - cores_left
parallelStartMulticore(cores)
# tune model
model = tuneParams(learner = nnet_learner, task = regr, resampling = cv,
                   measures = list(mae, rmse, rsq), par.set = nnet_hps,
                   control = grid_search)
# stop multi cores parallelization
parallelStop()
# optimization models
opt_df = model %>%
  getTuneResultOptPath() %>%
  arrange(mae.test.mean)
View(opt_df)
