from SALib.sample import saltelli
from SALib.analyze import sobol
from SALib.test_functions import Ishigami
import numpy as np
import pandas as pd

# define the input number, its names and limits
problem = {
    'num_vars': 15,
    'names': ['lx', 'ly', 'lz', 'alt', 'azi', 'room', 'storey', 'wrap', 'abs_wall',
              'abs_roof', 'shgc', 'u_window', 'vf', 'app', 'wwr_1'],
    'bounds': [[2, 20],
               [2, 20],
               [2, 5.5],
               [-10, 60],
               [0, 360],
               [0, 2],
               [0, 3],
               [0, 3],
               [0.2, 0.9],
               [0.2, 0.9],
               [0.1, 1],
               [2.5, 5.8],
               [0.4, 1],
               [0, 6],
               [0.15, 0.85]]
}

# create a qasi-random sample following saltelli's methodology
param_values = saltelli.sample(problem, 20)
df = pd.DataFrame(param_values, columns = problem['names'])
df.to_csv('/home/rodox/00.git/00.master_ufsc/05.sample/saltelli_sample_11.csv')

# define the input number, its names and limits
problem = {
    'num_vars': 16,
    'names': ['lx', 'ly', 'lz', 'alt', 'azi', 'room', 'storey', 'wrap', 'abs_wall',
              'abs_roof', 'shgc', 'u_window', 'vf', 'app', 'wwr_1', 'op'],
    'bounds': [[2, 20],
               [2, 20],
               [2, 5.5],
               [-10, 60],
               [0, 360],
               [0, 2],
               [0, 3],
               [0, 3],
               [0.2, 0.9],
               [0.2, 0.9],
               [0.1, 1],
               [2.5, 5.8],
               [0.4, 1],
               [0, 3],
               [0.15, 0.85],
               [0, 2]]
}

# create a qasi-random sample following saltelli's methodology
param_values = saltelli.sample(problem, 5)
df = pd.DataFrame(param_values, columns = problem['names'])
df.to_csv('/home/rodox/00.git/00.master_ufsc/05.sample/saltelli_sample_21.csv')

# define the input number, its names and limits
problem = {
    'num_vars': 16,
    'names': ['lx', 'ly', 'lz', 'alt', 'azi', 'room', 'storey', 'wrap', 'abs_wall',
              'abs_roof', 'shgc', 'u_window', 'vf', 'app', 'wwr_1', 'wwr_2'],
    'bounds': [[2, 20],
               [2, 20],
               [2, 5.5],
               [-10, 60],
               [0, 360],
               [0, 2],
               [0, 3],
               [0, 3],
               [0.2, 0.9],
               [0.2, 0.9],
               [0.1, 1],
               [2.5, 5.8],
               [0.4, 1],
               [0, 3],
               [0.15, 0.85],
               [0.15, 0.85]]
}

# create a qasi-random sample following saltelli's methodology
param_values = saltelli.sample(problem, 5)
df = pd.DataFrame(param_values, columns = problem['names'])
df.to_csv('/home/rodox/00.git/00.master_ufsc/05.sample/saltelli_sample_22.csv')

# define the input number, its names and limits
problem = {
    'num_vars': 15,
    'names': ['lx', 'ly', 'lz', 'alt', 'azi', 'room', 'storey', 'wrap', 'abs_wall',
              'abs_roof', 'shgc', 'u_window', 'vf', 'app', 'wwr_1'],
    'bounds': [[2, 20],
               [2, 20],
               [2, 5.5],
               [-10, 60],
               [0, 360],
               [0, 2],
               [0, 3],
               [0, 3],
               [0.2, 0.9],
               [0.2, 0.9],
               [0.1, 1],
               [2.5, 5.8],
               [0.4, 1],
               [0, 3],
               [0.15, 0.85]]
}

# create a qasi-random sample following saltelli's methodology
param_values = saltelli.sample(problem, 3)
df = pd.DataFrame(param_values, columns = problem['names'])
df.to_csv('/home/rodox/00.git/00.master_ufsc/05.sample/saltelli_sample_31.csv')

# define the input number, its names and limits
problem = {
    'num_vars': 16,
    'names': ['lx', 'ly', 'lz', 'alt', 'azi', 'room', 'storey', 'wrap', 'abs_wall',
              'abs_roof', 'shgc', 'u_window', 'vf', 'app', 'wwr_1', 'wwr_2'],
    'bounds': [[2, 20],
               [2, 20],
               [2, 5.5],
               [-10, 60],
               [0, 360],
               [0, 2],
               [0, 3],
               [0, 3],
               [0.2, 0.9],
               [0.2, 0.9],
               [0.1, 1],
               [2.5, 5.8],
               [0.4, 1],
               [0, 3],
               [0.15, 0.85],
               [0.15, 0.85]]
}

# create a qasi-random sample following saltelli's methodology
param_values = saltelli.sample(problem, 3)
df = pd.DataFrame(param_values, columns = problem['names'])
df.to_csv('/home/rodox/00.git/00.master_ufsc/05.sample/saltelli_sample_32.csv')

# define the input number, its names and limits
problem = {
    'num_vars': 15,
    'names': ['lx', 'ly', 'lz', 'alt', 'azi', 'room', 'storey', 'wrap', 'abs_wall',
              'abs_roof', 'shgc', 'u_window', 'vf', 'wwr_1', 'op'],
    'bounds': [[2, 20],
               [2, 20],
               [2, 5.5],
               [-10, 60],
               [0, 360],
               [0, 2],
               [0, 3],
               [0, 3],
               [0.2, 0.9],
               [0.2, 0.9],
               [0.1, 1],
               [2.5, 5.8],
               [0.4, 1],
               [0.15, 0.85],
               [0, 3]]
}

# create a qasi-random sample following saltelli's methodology
param_values = saltelli.sample(problem, 3)
df = pd.DataFrame(param_values, columns = problem['names'])
df.to_csv('/home/rodox/00.git/00.master_ufsc/05.sample/saltelli_sample_41.csv')

# define the input number, its names and limits
problem = {
    'num_vars': 16,
    'names': ['lx', 'ly', 'lz', 'alt', 'azi', 'room', 'storey', 'wrap', 'abs_wall',
              'abs_roof', 'shgc', 'u_window', 'vf', 'wwr_1', 'wwr_2', 'op'],
    'bounds': [[2, 20],
               [2, 20],
               [2, 5.5],
               [-10, 60],
               [0, 360],
               [0, 2],
               [0, 3],
               [0, 3],
               [0.2, 0.9],
               [0.2, 0.9],
               [0.1, 1],
               [2.5, 5.8],
               [0.4, 1],
               [0.15, 0.85],
               [0.15, 0.85],
               [0, 3]]
}

# create a qasi-random sample following saltelli's methodology
param_values = saltelli.sample(problem, 3)
df = pd.DataFrame(param_values, columns = problem['names'])
df.to_csv('/home/rodox/00.git/00.master_ufsc/05.sample/saltelli_sample_42.csv')


