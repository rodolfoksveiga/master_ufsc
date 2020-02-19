from SALib.sample import saltelli
from SALib.analyze import sobol
from SALib.test_functions import Ishigami
import numpy as np
import pandas as pd

# define the input number, its names and limits
problem = {
    'num_vars': 15,
    'names': ['lx', 'ly', 'lz', 'alt', 'azi', 'room', 'storey', 'bound', 'wrap',
              'abs_wall', 'abs_roof', 'shgc', 'u_window', 'vf', 'weather'],
    'bounds': [[2, 20],
               [2, 20],
               [2, 5.5],
               [-10, 60],
               [0, 360],
               [0, 2],
               [0, 3],
               [0, 7],
               [0, 3],
               [0.2, 0.9],
               [0.2, 0.9],
               [0.1, 1],
               [2.5, 5.8],
               [0.4, 1],
               [0, 3]],
}

# create a qasi-random sample following saltelli's methodology
param_values = saltelli.sample(problem, 200)
print(param_values)
df = pd.DataFrame(param_values, columns = problem['names'])
df.to_csv('/home/rodox/00.git/00.master_ufsc/saltelli_sample_cob.csv')
