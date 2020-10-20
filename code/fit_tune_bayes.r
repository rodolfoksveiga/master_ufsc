# load libraries
library(tidymodels)
# define input variables
data_path = './result/sample_year.csv'
qual_vars = c('seed', 'storey', 'shell_wall', 'shell_roof', 'blind', 'facade')
# load data
data = data_path %>%
  read.csv() %>%
  mutate_at(qual_vars, as.factor)
# split data
set.seed(100)
data_split = data %>%
  initial_split(prop = 0.8, strata = targ)
data_train = training(data_split)
data_test = testing(data_split)
# create cross-validation folds
set.seed(100)
data_folds = vfold_cv(data_train, v = 5, strata = targ)
# write a pre-process recipe
model_recipe = data_train %>%
  recipe(targ ~ .) %>%
  step_BoxCox(all_predictors(), -all_nominal()) %>%
  step_dummy(all_nominal())
# models
model_mlp = mlp(hidden_units = tune(), activation = 'relu', epochs = 50) %>%
  set_mode('regression') %>%
  set_engine('keras')
# model_svm = svm_rbf(cost = tune(), rbf_sigma = tune()) %>%
#   set_mode('regression') %>%
#   set_engine('kernlab')
# workflows
model_workflow = workflow() %>%
  add_model(model_mlp) %>%
  add_recipe(model_recipe)
# parameters
model_set = parameters(model_workflow)
# define search
model_search = model_workflow %>%
  tune_bayes(
    resamples = data_folds,
    param_info = model_set,
    initial = 5,
    iter = 10,
    metric = metric_set(rmse),
    control = control_bayes(no_improve = 5, verbose = TRUE)
  )
