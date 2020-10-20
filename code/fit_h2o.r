# load library
library(h2o)

# initialize the package with some configurations
h2o.init(nthreads = 3)
# clean all busy clusters
h2o.removeAll()

# load data
data = h2o.importFile('./result/sample_year.csv')

# transform qualitative variables
qual_vars = c('seed', 'storey', 'shell_wall', 'shell_roof',
              'blind', 'facade', 'mirror')
data[qual_vars] = h2o.asfactor(data[qual_vars])

# split data
splits = h2o.splitFrame(
  data,
  ratios = 0.8,
  destination_frames = c('train', 'test')
)
train = splits[[1]]
test = splits[[2]]

# # load pre-trained model
# model = h2o.loadModel('./result/h2o_best_model/XGBoost_3_AutoML_20201020_174322')

# auto-fit model
model = h2o.automl(
  y = 'targ',
  training_frame = data[[1]],
  nfolds = 2,
  include_algos = 'XGBoost',
  stopping_metric = 'MAE',
  sort_metric = 'MAE',
  max_models = 5,
  seed = 100,
  project_name = 'master',
  verbosity = 'warn'
)

# sort out best model
leaderboard = as.data.frame(model@leaderboard)
index = str_which(leaderboard$model_id, 'AllModels|BestOfFamily', negate = TRUE)[1]
model_id = leaderboard$model_id[index]
best_model = h2o.getModel(model_id)

# save models
h2o.saveModel(
  best_model,
  path = './result/h2o_best_model',
  force = TRUE
)

# evaluate test set
h2o.performance(model, newdata = test)

# clean all busy clusters
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

