# Predicting the thermal performance of multifamily buildings

This project is the outcome of my master thesis, carried out from 2019 to 2021, in the Federal University of Santa Catarina (UFSC).
<br>
The master thesis can be accessed, in Portuguese, at the official repository from [UFSC](http://repositorio.ufsc.br/). The text describes in details all the bibliographic references, the methodology, and the results.
<br>
A shorter version of the thesis was submitted, in English, to the [Building Simulation Conference 2021](https://bs2021.org/) in the form of an research paper. As soon as it gets approved, I'll provide the link for download.

## Objective

The Brazilian standard for the thermal performance of residential buildings offers a simulation-based methodology. However, building energy simulations (BES) are complex, time-consuming, and recquire qualified professionals. Data-driven models are an alternative to BES, since they significantly reduce the number of inputs parameters and can achieve high accuracy. Therefore, the objective of this project is to develop an accurate predictive model to estimate the thermal performance of multifamily buildings.

### Partnership

The project was financed by the National Council for Scientific and Technological Development (CNPq).

### Methods

* Descriptive statistics
* Data visualization
* Data processing
* Machine learning

### Technologies

* R
 * Tidyverse, Caret, Shiny
* Python
 * NumPy, Pandas, SALib

### Dependencies



## Project description

The project follows the fluxogram bellow.
<br>
<br>
![fluxogram](/home/rodox/Downloads/fluxogram.png)
<br>
<br>
The main code to build the dataset, process it, and optimize the predictive models is located at *[./code/main2.r](https://github.com/rodolfoksveiga/master_ufsc/blob/master/code/main2.r)*. As the file extension suggests, the code was developed using the [R](https://www.r-project.org/) language.

### Database design and sampling

To design and the database, *main2.r* performs the following steps:

**1.** Solves the dependencies, i.e. loads libraries and data, sources external code files, and defines global variables.

```R
## libraries and global environment
# define libraries to be loaded
pkgs = c('data.table', 'dplyr', 'jsonlite', 'reticulate',
       'parallel', 'purrr', 'stringr', 'tibble')
# load libraries
lapply(pkgs, library, character.only = TRUE)
# define external codes to be sourced
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
# directory to save the simulations's outputs
output_dir = '~/Documents/master/output/'
# directory to save the results
result_dir = '~/Documents/master/result/'
# path to save the tidy sample
sample_path = './result/sample.csv'
# number of cores to not use
cores_left = 0
```

**2.** Executes the Python code (location: *[./code/saltelli_sample.py](https://github.com/rodolfoksveiga/master_ufsc/blob/master/code/saltelli_sample.py)*), which generates a numeric dataset through the [Sensitivity Analysis Library in Python](https://salib.readthedocs.io/en/latest/) (SALib).

```R
# generate sample
py_run_file('./code/saltelli_sample.py')
```

The dataset contains 17 variables and 126000 instances, and the variables ranges were defined according to the table bellow. The table also describes the meaning of each variable.

| Variable | Meaning | Range |
|:-|:-:|:-:|
| seed | Typology | 1 ~ 5 |
| storey | Floor | 1 ~ 4 |
| area | Total living room and beroom net floor area (m²) | 50 ~ 150 |
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

Ps.: for extra information related the dataset, I suggest reading either the short research paper or the thesis text, referenced at beginning of this **README**.

**3.** Performs the function `TidySample` (location: *[./code/tidy_sample.r](https://github.com/rodolfoksveiga/master_ufsc/blob/master/code/tidy_sample.r)*), which tidies the data, so that the values become meaningful.

```R
# read and tidy up sample
sample = TidySample(saltelli_path, seeds_dir, models_dir, epws_dir, inmet)
```

After tidying, the types and possible values assume the values onde the table bellow.

| Variable | Meaning | Data Type | Possible Values |
|:-|:-:|:-:|:-:|
| seed | Typology | Factor | 1; 2; 3; 4 |
| storey | Floor | Factor | 1; 2; 3 |
| area | Total living room and beroom net floor area (m²) | Numeric | 50 ~ 150 |
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

It's important to note that `TidySample` removes repeated the rows from the dataset. These repeated rows appears after `TidySample` rounds the values of the integer (or factor) variables. Therefore, the tidy dataset contains 109276 rows.

**4.** Applies the function `BuildModel` (location: *[.code/build_model.r](https://github.com/rodolfoksveiga/master_ufsc/blob/master/code/build_model.r)*), for each row of the tidy dataset. This function takes as arguments each of the variables in the dataset and builds EnergyPlus simulation files (*JSON*) according to the variables values. Thus, after the execution of this function, 109276 simulation files are generated.

```R
# build cases
cores = detectCores() - cores_left
with(sample, mcmapply(BuildModel, seed_path, area, ratio, height, azimuth, shell_wall,
                      abs_wall, shell_roof, abs_roof, wwr_liv, wwr_dorm, u_window, shgc,
                      open_factor, blind, balcony, mirror, model_path, 'op_temp',
                      MoreArgs = list(construction, fill, setup, geometry),
                      mc.cores = cores))
```

**5.** Performs the function `MakeSlices` (location: *[./code/make_slices.r](https://github.com/rodolfoksveiga/master_ufsc/blob/master/code/make_slices.r)*), which defines a list of slices of the dataset. The number of slices is defined according to the value of the global variable `cores_left` and the number of rows of the tidy dataset.

After that, each slice is processed by the function `ProcessSlices`. Firstly, this function runs the EnergyPlus simulations in parallel, thorough the function `ProcessEPSim` (location: *[./code/run_ep_sim.r](https://github.com/rodolfoksveiga/master_ufsc/blob/master/code/run_ep_sim.r)*). Then, it calculates the targets and attach them to the slice of dataset, through the function `ApplyCalcTarget` (location: *[./code/calc_target.r](https://github.com/rodolfoksveiga/master_ufsc/blob/master/code/calc_target.r)*). Finally, it writes the slice of sample as a *csv* file, remove useless files and compile the errors in a single a file.


```R
# base function ####
# process slices of simulations
ProcessSlices = function(sample, n, size) {
    # run simulations in parallel
    ProcessEPSims(sample, output_dir, cores_left)
    # calculate targets and add them to the sample
    samples = ApplyCalcTarget(sample, output_dir, occup, inmet)
    # define case
    case = str_pad(n, str_length(size), 'left', 0)
    # write sample file
    lapply(samples, WriteSlice, result_dir)
    # remove simulation files
    RmCSVs(output_dir)
    # rename error files
    MvErrs(output_dir, result_dir, case)
}

# main code ####
# define number of slices according to the number of cores
size = nrow(sample) %/% cores
# define a vector to apply MakeSlices()
n = 1:size
# make a list with slices of the sample
slices = MakeSlices(sample, n, size, cores)
# apply ProcessSlices() on each element of the slices list
mapply(ProcessSlices, slices, n, size)
```

**6.** 

```R
# pile up results
patterns = paste0(periods, '.*\\.csv')
sample_paths = paste0(dirname(sample_path), '/sample_', periods, '.csv')
mapply(WriteSample, patterns, sample_paths, result_dir)
lapply(c('summary', 'description'), HandleSlices, result_dir)
# join samples
JoinSamples(saltelli_path, sample_paths[1])
# perform sensitivity analysis
py_run_file('./code/saltelli_sample.py')
```

**7.**

```R
# define list with hyperparameters
hps = list(lm = NULL, rf = NULL, svm = expand.grid(.sigma = 0.01, .C = 2^(1:8)))
# apply GenMLModels on the hyperparameters list
lapply(c(0, 0.01, 0.02), GenMLModels, sa_path = './result/sobol_analysis.json',
       data_path = './result/sample_year.csv', nfolds = 2, tune_length = 8, tune_grid = hps,
       save_stats = TRUE, save_plots = TRUE, save_models = TRUE, results_dir = './result/',
       plots_dir = './plot_table/', cores_left = 0)
```


### Data pre-processing


### Metamodel train and testing

The development of the predictive model lies on the optimization of the following 5 machine learning techniques:
* [Multiple Linear Regression](https://en.wikipedia.org/wiki/Linear_regression) (MLR)
* [Artificial Neural Networks](https://en.wikipedia.org/wiki/Artificial_neural_network) (ANN)
* [Support-Vector Machines](https://en.wikipedia.org/wiki/Support-vector_machine) (SVM)
* [Random Forest](https://en.wikipedia.org/wiki/Random_forest) (RF)
* [Extreme Gradient Boosted Trees](https://en.wikipedia.org/wiki/XGBoost) (XGBoost)

All the codes needed to optimize 


### Application

To facilitate the use of the developed predictive model, the most accurate machine learning model was deployed as an application at this [web page]().

## Contact

For further information about the project, please, contact me on rodolfoksveiga@gmail.com.
