from SALib.sample import saltelli
import pandas as pd
import numpy as np

# define range
def DefRange(bound, const):
  mult = (bound[1] - bound[0])*const
  inter = [bound[0] - mult, bound[1] + mult]
  return(inter)

# generate sample
def GenSample(names, bounds, const, quals, size, prop):
    sizes = [size*prop, size*(1 - prop)]
    sizes = [int(size) for size in sizes]
    cats = ['train', 'test']
    consts = np.repeat(const, len(names))
    consts[quals] = 0
    bounds = [DefRange(bound, const) for bound, const in zip(bounds, consts)]
    problem = {
            'num_vars': len(names),
            'names': names,
            'bounds': bounds
        }
    for size, cat in zip(sizes, cats):
        param_values = saltelli.sample(problem, size)
        df = pd.DataFrame(param_values, columns = problem['names'])
        df.to_csv('~/git/master/model/sample_' + cat + '.csv', index = False)

# main code
names = ['seed_path', 'area', 'ratio', 'height', 'azimuth', 'shell_wall', 'shell_roof', 'abs_wall',
         'abs_roof', 'wwr_liv', 'wwr_dorm', 'u_window', 'shgc', 'open_factor', 'weather']
bounds = [[1, 4], [30, 150], [0.5, 2], [2.5, 3.5], [0, 360], [1, 8], [1, 5], [0.2, 0.8], [0.2, 0.8],
          [0.15, 0.85], [0.15, 0.85], [2.8, 5.7], [0.22, 0.87], [0.4, 0.9], [1, 412]]
GenSample(names, bounds, 0.02, [0, 5, 6, 14], 200, 0.8)
