# load libraries and global environment ####
invisible({
  pkgs = c('dplyr', 'mlr')
  # required packages to fit models
  # pkgs = 'nnet'
  lapply(pkgs, library, character.only = TRUE)
  inmet = read.csv('./source/inmet_list.csv', stringsAsFactors = FALSE)
})

# variables ####
data_path = './result/sample.csv'
weather_var = 'cdh'

# main code ####
# load data
raw_data = read.csv(data_path)
str(raw_data)
# define qualitative and quantitative variables
qual_vars = c('seed', 'storey', 'shell_wall', 'shell_roof', 'blind', 'facade')
quant_vars = colnames(raw_data[-length(raw_data)])
quant_vars = quant_vars[!quant_vars %in% qual_vars]
# add weather variable values
raw_data$epw = inmet[raw_data$epw, weather_var]
# pre-process features
raw_data[, qual_vars] = lapply(raw_data[, qual_vars], factor)
dummy_data = raw_data %>%
  createDummyFeatures(target = 'targ') %>%
  normalizeFeatures(target = 'targ', method = 'standardize')
# setup
task = makeRegrTask('phft', data = dummy_data, target = 'targ')
hyperparameters1 = list('size' = 5, 'maxit' = 1000)
hyperparameters2 = list('neurons' = 5, 'verbose' = TRUE)
learner1 = makeLearner('regr.nnet', par.vals = hyperparameters1)
learner2 = makeLearner('regr.brnn', par.vals = hyperparameters2)
# cross-validation
resample = makeResampleDesc(method = 'CV', iters = 5)
model = resample(learner2, task, resample, measures = list(mae, rmse))

# brnn
# Aggregated Result: mae.test.mean=9.8248810,rmse.test.rmse=12.2513900