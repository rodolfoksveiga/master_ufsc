# load library ####
invisible({
  pkgs = c('h2o', 'parallel', 'stringr')
  lapply(pkgs, library, character.only = TRUE)
})

# variables ####
cores_left = 2

# initialize the h2o ####
h2o.init(nthreads = detectCores() - cores_left)

# main code ####
# load data
data = h2o.importFile('./result/sample_year.csv')

# transform qualitative variables
qual_vars = c('seed', 'storey', 'shell_wall', 'shell_roof',
              'blind', 'facade', 'mirror')
data[qual_vars] = h2o.asfactor(data[qual_vars])

# split data
# small
pre_splits = h2o.splitFrame(
  data,
  ratios = c(0.2, 0.1),
  destination_frames = paste0('pre_', c('train', 'test', 'leave'))
)
# large
splits = h2o.splitFrame(
  data,
  ratios = 0.8,
  destination_frames = c('train', 'test')
)

# auto-fit model
# algorithms = c('GLM', 'DRF', 'GBM', 'XGBoost')
models = h2o.automl(
  y = 'targ',
  training_frame = 'pre_train',
  nfolds = 5,
  include_algos = 'DRF',
  stopping_metric = 'MAE',
  sort_metric = 'MAE',
  max_models = 8,
  seed = 100,
  project_name = 'master',
  verbosity = 'warn'
)

# sort out best 5 models
leaderboard = as.data.frame(models@leaderboard)
index = str_which(leaderboard$model_id, 'AllModels|BestOfFamily', negate = TRUE)[1:5]
model_ids = leaderboard$model_id[index]

ExtractHPs = function(model_id) {
  model_id = model_ids[1]
  
  techs = str_flatten(c('GLM', 'DRF', 'GBM', 'XGBoost'), collapse = '|')
  tech = str_extract(model_id, techs)
  
  hps_list = list('GLM' = NA, 'DRF' = )
  hps = 
  
  model = h2o.getModel(model_id)
  model@allparameters[]
}

# save models
h2o.saveModel(
  best_model,
  path = './result/h2o_best_model',
  force = TRUE
)

# clean cluster
h2o.removeAll()

# # 60% training / 20% validation / 20% testing
# # split data
# data = h2o.splitFrame(
#   data,
#   ratios = c(0.6, 0.2),
#   destination_frames = c('train', 'valid', 'test')
# )
# # fit model
# model1 = h2o.deeplearning(
#   model_id = 'ml1', # model's identifation
#   training_frame = 'train', # training data
#   validation_frame = 'valid', # validation data
#   x = features, # features
#   y = target, # target
#   activation = 'Rectifier', # activation function (ReLU)
#   hidden = c(10, 10, 10), # number of neurons per hidden layer
#   epochs = 100, # number of epochs (cycles)
#   stopping_metric = 'MAE',
#   stopping_tolerance = 0.1, 
#   seed = 100, # seed to guarantee reproducibility
#   verbose = FALSE
# )
# 
# # 80% training with 5 folds cross-validation / 20% testing
# 
# # # add cross-validation column to the training set
# # folds = h2o.kfold_column(data[[1]], nfolds = 5)
# # names(folds) = 'fold'
# # data[[1]] = h2o.cbind(data[[1]], folds)
# 
# model2 = h2o.deeplearning(
#   model_id = 'ml2', # model's identifation
#   x = features, # features
#   y = target, # target
#   training_frame = 'train', # training data
#   nfolds = 5, # number of cross-validation folds
#   fold_assignment = 'Auto', # rely on a variable distribution
#   # fold_column = 'fold', # stratify folds according to the target
#   activation = 'Rectifier', # activation function (ReLU)
#   hidden = c(10, 10, 10), # number of neurons per hidden layer
#   epochs = 100, # number of epochs (cycles)
#   stopping_metric = 'MAE',
#   stopping_tolerance = 0.1, 
#   seed = 100, # seed to guarantee reproducibility
#   verbose = FALSE
# )
# # clean all busy clusters
# h2o.removeAll()
# 
# # evaluation
# perf = list(
#   train = h2o.performance(model, train = TRUE),
#   valid = h2o.performance(model, valid = TRUE),
#   test = h2o.performance(model, newdata = data[[3]])
# )
# h2o.performance(model, train = TRUE)
# h2o.performance(model, valid = TRUE)

# # write model
# h2o.saveModel(
#   model1,
#   path = './result/h2o_model1',
#   force = TRUE
# )

