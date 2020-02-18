from SALib.sample import saltelli
from SALib.analyze import sobol
from SALib.test_functions import Ishigami
import numpy as np
import pandas as pd

#DEFINIR A QUANTIDADE E NOME DOS INPUTS E OS LIMITES DE VARIAÇÃO DOS MESMOS
problem = {
    'num_vars': 11,
    'names': ['veneziana','wwr','u_cob','ct_cob','abscob','u_par','ct_par','abspar','fs_vid','hjan','openfac'],
    'bounds': [[0,2],
               [0.1,0.9],
               [0.5,3.5],
               [0,3],
               [0.2,0.8],
               [0.5,3.5],
               [0,3],
               [0.2,0.8],
               [0.22,0.87],
               [0,1],
               [0.5,1]],
}

#CRIAR UM SAMPLE COM VALORES ALEATÓRIOS DE ACORDO COM SALTELLI
param_values = saltelli.sample(problem, 1000)
print(param_values)
df = pd.DataFrame(param_values,columns=problem['names'])
df.to_csv('saltelli_sample_cob.csv')