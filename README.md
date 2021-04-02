# Predicting the thermal performance of naturally ventilated apartments

This project is the outcome of my master thesis, carried out from 2019 to 2021, in the Federal University of Santa Catarina (UFSC).

The master thesis can be accessed, in Portuguese, at the official repository from [UFSC](http://repositorio.ufsc.br/). The text describes in details the motivations and objectives of the research, all the bibliographic references used throughout the methodology, the methodology, the results, and the conclusions.

A shorter version of the thesis was submitted, in English, to the [Building Simulation Conference 2021](https://bs2021.org/) in the form of a research paper. As soon as it is published, I'll provide you the link for download.

## Objective

The Brazilian standard for the thermal performance of residential buildings (NBR 15.575) offers a simulation-based method. However, building energy simulations (BES) are complex, time-consuming, and require qualified professionals. Data-driven models are an alternative to BES, since they significantly reduce the number of input parameters and can achieve high accuracy. Therefore, the objective of this project was to develop a predictive model to estimate the thermal performance of naturally ventilated apartments.

### Partnership

The project was financed by the Brazilian National Council for Scientific and Technological Development (CNPq).

### Methods

* Descriptive statistics
* Data visualization
* Data processing
* Machine learning

### Dependencies

* [R 4.0.4](https://cran.r-project.org/src/base/R-4/)
    * brnn 0.8
    * caret 6.0-86
    * data.table 1.14.0
    * doParallel 1.0.16
    * dplyr 1.0.5
    * forcats 0.5.1
    * ggplot2 3.3.3
    * hydroGOF 0.4-0
    * jsonlite 1.7.2
    * kernlab 0.9-29
    * Metrics 0.1.4
    * parallel 4.0.4
    * plyr 1.8.6
    * purrr 0.3.4
    * RColorBrewer 1.1-2
    * reshape2 1.4.4
    * reticulate 1.18
    * RJSONIO 1.3-1.4
    * stringr 1.4.0
    * tibble 3.1.0
    * xgboost 1.3.2.1
* [Python 3.9.2](https://www.python.org/downloads/release/python-392/)
    * numpy 1.20.1
    * pandas 1.2.3
    * SALib 1.3.12

## Project description

The project follows the fluxogram bellow.
<br>
<br>
![fluxogram](/home/rodox/Downloads/fluxogram.png)
<br>
<br>
All the programming codes to run this project are located in the folder **[code](https://github.com/rodolfoksveiga/master_ufsc/tree/master/code)**.

The *[main2.r](https://github.com/rodolfoksveiga/master_ufsc/blob/master/code/main2.r)* code samples and tidies the dataset and performs sensitivity analysis.

After trying many frameworks to develop the predictive model, such as Tidymodels and Keras, [Caret](https://topepo.github.io/caret/) was the chosen one, since it's simple and could achieve the desired model. The *[fit_caret.r](https://github.com/rodolfoksveiga/master_ufsc/blob/master/code/fit_caret.r) code pre-processes the dataset and optimizes the predictive model.

The *[display_results.r](https://github.com/rodolfoksveiga/master_ufsc/blob/master/code/display_result.r)* plots most of the graphs displayed in this **README** file. The graphs are all located in the folder **[plot_table](https://github.com/rodolfoksveiga/master_ufsc/tree/master/plot_table)**.

### Database sampling

To sample and tidy the database, *[main2.r](https://github.com/rodolfoksveiga/master_ufsc/blob/master/code/main2.r)* performs the following steps:

**1.** Load libraries and data, source external code files, and define global variables.

```R
## libraries and global environment
# define libraries to load
pkgs = c('data.table', 'dplyr', 'jsonlite', 'reticulate',
       'parallel', 'purrr', 'stringr', 'tibble')
# load libraries
lapply(pkgs, library, character.only = TRUE)
# define pipenv as the python virtual environment
venv = system('pipenv --venv', inter = TRUE)
use_virtualenv(venv, required = TRUE)
py_config()
# define external codes source
codes = c('build_model', 'calc_target', 'make_slices', 'run_ep_sim', 'tidy_sample')
codes = paste0('./code/', codes, '.r')
# source codes
lapply(codes, source)
# load external data
occup = read.csv('./source/occup.csv')
inmet = read.csv('./source/inmet_list.csv')
geometry = read_json('./source/geometry.json')[[2]]
construction = read_json('./source/construction.json')
fill = read_json('./source/fill.json')
setup = read_json('./source/setup.json')

## variables
# path to save the initial dataset
saltelli_path = './result/saltelli_sample.csv'
# directory of the seed simulation files
seeds_dir = './seed/'
# directory to save the energyplus simulation models
models_dir = '~/Documents/master/model/'
# directory of the weather files
epws_dir = '~/Documents/weather/'
# directory to save the simulations outputs
output_dir = '~/Documents/master/output/'
# directory to save the results
result_dir = '~/Documents/master/result/'
# path to save the tidy sample
sample_path = './result/sample.csv'
# number of cores not to use
cores_left = 0
```

**2.** Execute *[saltelli_sample.py](https://github.com/rodolfoksveiga/master_ufsc/blob/master/code/saltelli_sample.py)*, which generates a numeric dataset using the Saltelli's Sequence. Saltelli's Sequence is an extension of the so-called Sobol's Sequence. The [Sensitivity Analysis Library in Python](https://salib.readthedocs.io/en/latest/) (SALib) was used to achieve sample the dataset.

```R
## main code
# generate sample
py_run_file('./code/saltelli_sample.py')
```

The dataset contains 17 variables and 126,000 instances. The variables ranges were defined according to the table bellow. The table also describes the meaning of each variable.

| Variable | Meaning | Range |
|:-:|:-:|:-:|
| seed | Typology | 1 ~ 5 |
| storey | Floor | 1 ~ 4 |
| area | Total living room and bedroom net floor area (m²) | 50 ~ 150 |
| ratio | Width per length ratio | 0.5 ~ 2 |
| height | Ceiling height (m) | 2.5 ~ 3.5 |
| azimuth | Azimuth angle of the building (°) | 0 ~ 360 |
| shell_wall | Walls materials | 1 ~ 7 |
| abs_wall | Solar absorptance of external walls | 0.2 ~ 0.8 |
| shell_roof | Roof and ceiling materials | 1 ~ 5 |
| abs_roof | Solar absorptance of the roof | 0.2 ~ 0.8 |
| wwr_liv | Window to wall ratio on the living room (%) | 0.2 ~ 0.8 |
| wwr_dorm | Window to wall ratio on the dormitories (%) | 0.2 ~ 0.8 |
| u_window | Thermal transmittance of the windows (W/m².K) | 2.8 ~ 5.7 |
| shgc | Solar heat gain coefficient of the glasses | 0.22 ~ 0.87 |
| open_factor | Window opening factor | 0.4 ~ 0.9 |
| blind | Blind | 0 ~ 2 |
| balcony | Balcony depth (m²) | -1.5 ~ 2.5 |
| facade | Position of the window on the bedroom | 1 ~ 3 |
| mirror | Building floor plan mirroring | 0 ~ 2 |
| dbt | Mean drybulb temperature (°C) | 10.83 ~ 28.24 |

Ps.: for extra information about the dataset, I suggest reading either the short research paper or the complete thesis text, referenced at beginning of this **README** file.

**3.** Perform the function `TidySample`, from *[tidy_sample.r](https://github.com/rodolfoksveiga/master_ufsc/blob/master/code/tidy_sample.r)*, which tidies the dataset, so that the values become meaningful.

```R
# read and tidy up sample
sample = TidySample(saltelli_path, seeds_dir, models_dir, epws_dir, inmet)
```

The table bellow describes the shape of the tidy dataset.

| Variable | Meaning | Data Type | Possible Values |
|:-:|:-:|:-:|:-:|
| seed | Typology | Factor | 1; 2; 3; 4 |
| storey | Floor | Factor | 1; 2; 3 |
| area | Total living room and bedroom net floor area (m²) | Numeric | 50 ~ 150 |
| ratio | Width per length ratio | Numeric | 0.5 ~ 2.0 |
| height | Ceiling height (m) | Numeric | 2.5 ~ 3.5 |
| azimuth | Azimuth angle of the building (°) | Numeric | 0 ~ 360 |
| shell_wall | Walls materials | Factor | 1; 2; 3; 4; 5; 6 |
| abs_wall | Solar absorptance of external walls | Numeric | 0.2 ~ 0.8 |
| shell_roof | Roof and ceiling materials | Factor | 1; 2; 3; 4 |
| abs_roof | Solar absorptance of the roof | Numeric | 0.2 ~ 0.8 |
| wwr_liv | Window to wall ratio on the living room (%) | Numeric | 0.2 ~ 0.8 |
| wwr_dorm | Window to wall ratio on the dormitories (%) | Numeric | 0.2 ~ 0.8 |
| u_window | Thermal transmittance of the windows (W/m².K) | Numeric | 2.8 ~ 5.7 |
| shgc | Solar heat gain coefficient of the glasses | Numeric | 0.22 ~ 0.87 |
| open_factor | Window opening factor | Numeric | 0.4 ~ 1 |
| blind | Blind | Factor | 0; 1 |
| balcony | Balcony depth (m²) | Numeric | 0; 0.5 ~ 2.5 |
| facade | Position of the window on the bedroom | Factor | 1; 2 |
| mirror | Building floor plan mirroring | Factor | 0; 1 |
| dbt | Mean drybulb temperature (°C) | Numeric | 10.83 ~ 28.24 |

It's important to note that `TidySample` removes repeated rows from the dataset. These repeated rows appears after `TidySample` rounds the values of the factor variables. Therefore, the tidy dataset contains 109,276 rows.

**4.** Apply the function `BuildModel`, from *[build_model.r](https://github.com/rodolfoksveiga/master_ufsc/blob/master/code/build_model.r)*, for each row of the tidy dataset. This function takes as arguments each of the variables in the dataset, except for the variable *dbt*, and builds EnergyPlus simulation files according to these variables values. Thus, after the execution of this function, 109,276 simulation files are generated.

The EnergyPlus simulation files have *epJSON* extension, which corresponds to *JSON* files.

```R
## main code
# define number of cores to use
cores = detectCores() - cores_left
# build simulation files
with(sample, mcmapply(BuildModel, seed_path, area, ratio, height, azimuth, shell_wall,
                      abs_wall, shell_roof, abs_roof, wwr_liv, wwr_dorm, u_window, shgc,
                      open_factor, blind, balcony, mirror, model_path, 'op_temp',
                      MoreArgs = list(construction, fill, setup, geometry),
                      mc.cores = cores))
```

**5.** Perform the function `MakeSlices`, from *[make_slices.r](https://github.com/rodolfoksveiga/master_ufsc/blob/master/code/make_slices.r)*, which defines a list of dataset slices. The number of slices is defined according to the value of the global variable `cores_left` and the number of rows of the tidy dataset.

After that, each dataset slice is processed by the function `ProcessSlices`. Firstly, this function runs the EnergyPlus simulations in parallel, thorough the function `ProcessEPSim`, from *[run_ep_sim.r](https://github.com/rodolfoksveiga/master_ufsc/blob/master/code/run_ep_sim.r)*. Then, it runs the function `ApplyCalcTarget`, from *[calc_target.r](https://github.com/rodolfoksveiga/master_ufsc/blob/master/code/calc_target.r)*, which calculates the targets and attach them to the correspondent dataset slice. Finally, it writes the dataset slice with the targets as a *csv* file, remove useless simulation outputs and compile the simulation errors in a single a file.

```R
## functions
# process slices of simulations
ProcessSlices = function(sample, n, size) {
# run simulations in parallel
ProcessEPSims(sample, output_dir, cores_left)
# calculate targets and add them to the sample
sample = ApplyCalcTarget(sample, output_dir, occup, inmet)
# define case
case = str_pad(n, str_length(size), 'left', 0)
# write sample file
WriteSlice(sample, result_dir, case)
# remove useless simulation outputs
RmCSVs(output_dir)
# rename simulation error files
MvErrs(output_dir, result_dir, case)
}

## main code
# define number of slices
size = nrow(sample) %/% cores
# define a vector to apply MakeSlices()
n = 1:size
# make a list with slices of the sample
slices = MakeSlices(sample, n, size, cores)
# apply ProcessSlices() on each element of the slices list
mapply(ProcessSlices, slices, n, size)
```

The target represents the thermal performance of the apartment, which is described by an index named PHFT. PHFT stands for the percentage of occupied hours within an operative temperature range. The index was defined in the NBR 15.575 and its fundaments were published in [this](https://www.researchgate.net/publication/345742006_Proposta_de_metodo_de_avaliacao_do_desempenho_termico_de_residencias_NBR_15575) research paper, available in Portuguese.

The following formula calculates the PHFT:

**PHFT = ( Nh<sub>FT</sub> / Nh<sub>Occup</sub> ) \* 100**

**Nh<sub>FT</sub>** is the number of hours throughout the year when the apartment is occupied and their operative temperature are within a specific range. **Nh<sub>Occup</sub>** is the total number of hours throughout the year when the apartment is occupied.

The table bellow shows the operative temperature ranges.

| Range ID | Mean drybulb temperature (mDBT) | Indoor operative temperature (T<sub>O</sub>) range |
|:-:|:-:|:-:|
| 1 | mDBT < 25°C | 18°C < T<sub>O</sub> < 26°C |
| 2 | 25°C <= mDBT < 27°C | T<sub>O</sub> < 28°C |
| 3 | mDBT >= 27°C | T<sub>O</sub> < 30°C |

The mean drybulb temperature of each EnergyPlus weather file (*epw*) considered in this project was defined in the file *[inmet_list.csv](https://github.com/rodolfoksveiga/master_ufsc/blob/master/source/inmet_list.csv)*.

**6.** Pile up the dataset slices into a single dataset and write it as a *csv* file. Also concatenate the simulation error files of each slice into a single error file.

```R
## main code
# pile up results
WriteSample('sample.csv', sample_path, result_dir)
# concatenate errors
lapply(c('summary', 'description'), HandleSlices, result_dir)
```

The final dataset contain 109,276 rows and 21 columns, corresponding to the variables: `seed`, `storey`, `area`, `ratio`, `height`, `azimuth`, `shell_wall`, `abs_wall`, `shell_roof`, `abs_roof`, `wwr_liv`, `wwr_dorm`, `u_window`, `shgc`, `open_factor`, `blind`, `balcony`, `facade`, `mirror`, `dbt`, `targ` (PHFT).

### Sobol's Sensitivity Analysis

To perform the sensitivity analysis, *[main2.r](https://github.com/rodolfoksveiga/master_ufsc/blob/master/code/main2.r)* executes the code chunk below.

First, it runs the function `JoinSamples`, from *[tidy_sample.r](https://github.com/rodolfoksveiga/master_ufsc/blob/master/code/tidy_sample.r)*, which join the tidy dataset (with targets) with the original numeric dataset (without targets). After that, it executes *[sobol_analysis.py](https://github.com/rodolfoksveiga/master_ufsc/blob/master/code/sobol_analysis.py)*, which perfmorms the Sobol's Sensitivity Analysis over the joined dataset. SALib was used to compute the sensitivity analysis.

```R
## main code
# join samples
JoinSamples(saltelli_path, sample_path)
# perform sensitivity analysis
py_run_file('./code/saltelli_sample.py')
```

### Metamodel training and testing

The development of the predictive model lies on the optimization of the following 5 machine learning techniques:
* [Multiple Linear Regression](https://en.wikipedia.org/wiki/Linear_regression) (MLR)
* [Artificial Neural Networks](https://en.wikipedia.org/wiki/Artificial_neural_network) (ANN)
* [Support-Vector Machines](https://en.wikipedia.org/wiki/Support-vector_machine) (SVM)
* [Random Forest](https://en.wikipedia.org/wiki/Random_forest) (RF)
* [Extreme Gradient Boosted Trees](https://en.wikipedia.org/wiki/XGBoost) (XGBoost)

To train and test the model, *[fit_caret.r](https://github.com/rodolfoksveiga/master_ufsc/blob/master/code/main2.r)* performs the following steps:

**1.** Load libraries and data, source external code files, and define global variables.

```R
## load libraries and global environment
pkgs = c('caret', 'doParallel', 'data.table', 'dplyr',
        'ggplot2', 'hydroGOF', 'Metrics', 'parallel', 'purrr')
# required packages to fit models
# pkgs = c(pkgs, 'brnn', 'kernlab', 'plyr', 'xgboost')
lapply(pkgs, library, character.only = TRUE)

## variables
# path to the dataset
data_path = 'result/sample.csv'
# number of folders used on cross-validation
# path to the sensitivity analysis output (json)
sa_path = 'result/sobol_analysis.json'
# sensitivity analysis threshold to select features
sa_threshold = 0.02
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
pt_dir = '~/Documents/master/'
# directory to write machine learning models
models_dir = '~/Documents/master/'
# number of cores not to use
cores_left = 0
```

**2.** Load dataset and transform qualitative variables in factors, so that it's possible to transform these variables into dummy variables.

```R
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
```

**3.**  Perform the function `SelectFeats`, which removes from the dataset every variable presenting sensitivity index lower than the threshold defined by the global variable `sa_threshold`. This function takes as input the output of the code *[saltelli_sample.py](https://github.com/rodolfoksveiga/master_ufsc/blob/master/code/saltelli_sample.py)*.

```R
## functions
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

## main code
# select features according to sensitivity analysis
raw_data = SelectFeats(raw_data, sa_path, threshold)
```

**4.** Transform qualitative variables into dummy variables, through the function `CreateDummies`.

```R
## functions
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

## main code
# create a dummy dataset with dummy variables
dummy_data = CreateDummies(raw_data)
```

**5.** Execute the function `SplitData` to split the dataset into a list containing two datasets: training (80%) and testing (20%). In this step, both datasets (raw and dummy) were splitted, because the raw dataset will be used later to perform the predictions and calculate the accuracy metrics.

```R
## functions
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

## main code
# split data into train and test sets
raw_data = lapply(list('train' = TRUE, 'test' = FALSE), SplitData, raw_data, 0.01)
dummy_data = lapply(list('train' = TRUE, 'test' = FALSE), SplitData, dummy_data, 0.01)
```

**6.** Apply the function `FitModel` over each model and its respective hyperparameters grid, defined in the global variable `tune_grid` (training dataset). First, the function defines some training properties, such as the sampling technique (cross-validation) and the number of folders adopted in the cross-validation. After that, the model is trained considering the pre-defined grid.

During the execution of the function `train`, from Caret, a Box-Cox transformation is performed in order to pre-process the dataset. Still during the training, every model is evaluated according to the metric RMSE.

All the processing is done in parallel and reproducibility is assured.

```R
## functions
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
  fit = train(targ ~ ., train_data, trControl = fit_ctrl, tuneGrid = tune_grid,
              method = train_tech, metric = eval, preProcess = 'BoxCox')
  # stop parallel processing
  registerDoSEQ()
  # clean memory
  gc()
  return(fit)
}

## main code
# train models
models_list = list(lm = 'lm', ann = 'brnn', svm = 'svmRadial', rf = 'rf', xgbt = 'xgbTree')
models = mapply(FitModel, models_list, tune_grid, SIMPLIFY = FALSE,
                MoreArgs = list('cv', nfolds, dummy_data$train, cores_left))
```

**7.** Evaluate the best model selected from each technique using the metrics MAE, RMSE and R² (testing dataset), write the summary as a *csv* file with training and testing performances, and write the models as a *rds* files.

```R
## main code
# test models
predictions = models %>%
  lapply(predict, newdata = dummy_data$test) %>%
  as.data.frame()
# define suffix tag
suffix = paste0('nvar', ncol(raw_data$train))
# generate accuracy table
GenAccuracyTable(models, predictions, dummy_data$test$targ, suffix, pt_dir)
# write models
saveRDS(models, file = paste0(models_dir, 'models_', suffix, '.rds'))
```

![perf_test](https://github.com/rodolfoksveiga/master_ufsc/blob/master/plot_table/perf_test.png)

### Application

To facilitate the use of the developed predictive model, the most accurate machine learning model was deployed as an application at this [web page]().

## Contact

For further information about the project, please, contact me on rodolfoksveiga@gmail.com.
