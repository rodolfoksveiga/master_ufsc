import json
import pandas as pd
import numpy as np
from SALib.sample import saltelli

# define range
def DefRange(bound, gap):
  mult = (bound[1] - bound[0])*gap
  inter = [bound[0] - mult, bound[1] + mult]
  return(inter)

# generate sample
def GenSample(names, bounds, gap, quals, size, output_dir, save_problem = True):
    consts = np.repeat(gap, len(names))
    consts[quals] = 0
    bounds = [DefRange(bound, const) for bound, const in zip(bounds, consts)]
    problem = {'num_vars': len(names), 'names': names, 'bounds': bounds}
    param_values = saltelli.sample(problem, size)
    df = pd.DataFrame(param_values, columns = problem['names'])
    df.to_csv(output_dir + 'saltelli_sample.csv', index = False)
    if save_problem:
      problem_path = output_dir + 'sobol_problem.json'
      with open(problem_path, 'w') as file:
        json.dump(problem, file, indent = 4)

# main code
names = ['seed', 'area', 'ratio', 'height', 'azimuth', 'shell_wall', 'abs_wall',
         'shell_roof', 'abs_roof', 'wwr_liv', 'wwr_dorm', 'u_window', 'shgc',
         'open_factor', 'blind', 'balcony', 'facade', 'epw']
bounds = [[1, 5], [30, 150], [0.5, 2], [2.5, 3.5], [0, 360], [1, 8], [0.2, 0.8],
          [1, 5], [0.2, 0.8], [0.15, 0.85], [0.15, 0.85], [2.8, 5.7], [0.22, 0.87],
          [0.4, 0.9], [0, 2], [-2, 2], [0, 2], [1, 412]]
quant_vars = [0, 5, 7, 14, 16, 17]
output_dir = '/home/rodox/git/master/result/'
GenSample(names, bounds, 0.02, quant_vars, 500, output_dir)

