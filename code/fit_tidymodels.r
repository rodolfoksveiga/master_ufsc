# load libraries ####
library(tidymodels)

# variables ####
data_path = './result/sample_year.csv'
qual_vars = c('seed', 'storey', 'shell_wall', 'shell_roof',
              'blind', 'facade', 'mirror')

# main code ####
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
  step_dummy(all_nominal()) %>%
  prep()
# perform pre-process on training set (dataset used in recipe())
data_train = juice(model_recipe)
# perform pre-process on testing test (any new dataset)
data_test = model_recipe %>%
  bake(data_test)
# train models
model_mlp = mlp(hidden_units = tune(), activation = 'relu') %>%
  set_mode('regression') %>%
  set_engine('keras') %>%
  tune_grid(model_recipe, resamples = data_folds, grid = 5)
# predict testing sample
pred = model_mlp %>%
  predict(data_test)
# evaluate model
pred %>%
  bind_cols(data_test) %>%
  metrics(truth = targ, estimate = .pred)
# show best from resample
show_best(model_lm, metric = 'rmse')

