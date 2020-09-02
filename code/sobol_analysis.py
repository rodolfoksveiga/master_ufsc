import json
import math
import numpy as np
import pandas as pd
import random as rd
from SALib.analyze import sobol

problem_path = '/home/rodox/git/master/result/sobol_problem.json'
sample_path = '/home/rodox/git/master/result/sample.csv'
saltelli_path = '/home/rodox/git/master/result/saltelli_sample.csv'

with open(problem_path) as file:
  problem = json.load(file)

# load sample with targets
sample = pd.read_csv(sample_path)
# load satelli sample
saltelli = pd.read_csv(saltelli_path)
# extract targets
targ = np.asarray(sample['targ'])
targ = np.asarray(saltelli['targ'])

# check if it's possible to calculate sensitivity analysis
# problem['num_vars'] = problem['num_vars'] + 1
dimension = problem['num_vars']

rest = targ.size % (2*dimension + 2)
if rest == 0:
  print('Correct number of records.')
else:
  n = [rd.randint(0, len(targ)) for _ in range(rest)]
  targ = np.append(targ, targ[n])
  print('Incorrect number of records in model output file!')

solution = sobol.analyze(problem, targ, calc_second_order = True, print_to_console = True)


# for key in sa:
# 	sa[key] = list(sa[key])
# for key in ['S2','S2_conf']:
# 	for line in range (len(sa[key])):
# 		sa[key][line] = list(sa[key][line])
# with open ('sa_c_sp_cob.json','w') as file:
# 	json.dump(sa, file)
