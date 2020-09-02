# load libraries ####
invisible({
  pkgs = c('caret', 'dplyr', 'keras')
  lapply(pkgs, library, character.only = TRUE)  
  inmet = read.csv('./source/inmet_list.csv', stringsAsFactors = FALSE)
})

# variables ####
weather_var = 'tbsm'

# load data ####
raw_data = read.csv('./result/sample.csv')
str(raw_data)

# pre-process ####
# edit sample
raw_data$epw = inmet[raw_data$epw, weather_var]
# define qualitative and quantitative variables
qual_vars = c('seed', 'storey', 'shell_wall', 'shell_roof', 'blind', 'facade')
quant_vars = colnames(raw_data[-length(raw_data)])
quant_vars = quant_vars[!quant_vars %in% qual_vars]
# create dummy variables
raw_data[, qual_vars] = lapply(raw_data[, qual_vars], factor)
dummy_model = dummyVars(targ ~ ., data = raw_data)
dummy_data = data.frame(predict(dummy_model, newdata = raw_data))
dummy_data$targ = raw_data$targ
# split data into train and test sets
index = createDataPartition(dummy_data$targ, p = 0.8, list = FALSE)
train_data = dummy_data[index, ]
test_data = dummy_data[-index, ]
# pre-process model
pp_model = preProcess(train_data[, quant_vars], method = 'BoxCox')
train_data = predict(pp_model, train_data)
test_data = predict(pp_model, test_data)
# separate labels and targets
train_labels = as.matrix(select(train_data, -targ))
train_targets = as.matrix(select(train_data, targ))
test_labels = as.matrix(select(test_data, -targ))
test_targets = as.matrix(select(test_data, targ))

# build model ####
# define kera's model class
# add layers to the model
set.seed(100)
model = keras_model_sequential() %>%
  layer_dense(units = 16, activation = 'relu', input_shape = ncol(train_labels)) %>%
  layer_dense(units = 8, activation = 'relu') %>%
  layer_dense(units = 8, activation = 'relu') %>%
  layer_dense(units = 4, activation = 'relu') %>%
  layer_dense(units = 4, activation = 'relu') %>%
  layer_dense(units = 1) %>%
  compile(loss = 'mse',
          optimizer = optimizer_adam(lr = 0.002),
          metrics = list('mae'))
# print model info
summary(model)
# fit model
history = model %>%
  fit(train_labels, train_targets, batch_size = 516,
      epochs = 1000, validation_split = 0.2, verbose = 2,
      callbacks = callback_early_stopping(monitor = 'val_loss', patience = 20))
# evaluate model
test = model %>%
  evaluate(test_labels, test_targets)
