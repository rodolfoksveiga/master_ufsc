from SALib.sample import saltelli
from SALib.analyze import sobol
from SALib.test_functions import Ishigami
import numpy as np
import pandas as pd
from six.moves import xrange
import json

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

#IMPORTAR AS VARIÁVEIS ALEATÓRIAS CALCULADAS PELO MÉTODO DE SALTELLI PARA O DATA FRAME DE ENTRADA DO METAMODELO (LEMBRAR DE ARREDONDAR OS VALORES DE CT_COB E CT_PAR PARA 1, 2 E 3)
#ATRAVÉS DO METAMODELO, CALCULA-SE A CARGA TÉRMICA DE RESFRIAMENTO PARA A CONDIÇÃO REAL

#CRIAR UM VETOR (Y) COM O RESULTADO DO CÁLCULO DO METAMODELO COMO ARQUIVO .csv
#IMPORTAR O VETOR (Y) PARA O PYTHON


#SÃO PAULO
Y = pd.read_csv('y_c_sp_cob.csv')
Y = np.asarray(Y)
Y_new = [x for x in range(len(Y))]
for x in xrange(len(Y)):
	Y_new[x] = Y[x][0]
Y = Y_new
Y = np.asarray(Y)
#REALIZAR A ANÁLISE DE SENSIBILIDADE DE SOBOL
sa = sobol.analyze(problem,Y,print_to_console=False)
#TRANSFORMAR O ARQUIVO EM .JSON
for key in sa:
	sa[key] = list(sa[key])
for key in ['S2','S2_conf']:
	for line in range (len(sa[key])):
		sa[key][line] = list(sa[key][line])
with open ('sa_c_sp_cob.json','w') as file:
	json.dump(sa,file)


#CUIABÁ
Y = pd.read_csv('y_c_mt_cob.csv')
Y = np.asarray(Y)
Y_new = [x for x in range(len(Y))]
for x in xrange(len(Y)):
	Y_new[x] = Y[x][0]
Y = Y_new
Y = np.asarray(Y)
#REALIZAR A ANÁLISE DE SENSIBILIDADE DE SOBOL
sa = sobol.analyze(problem,Y,print_to_console=False)
#TRANSFORMAR O ARQUIVO EM .JSON
for key in sa:
	sa[key] = list(sa[key])
for key in ['S2','S2_conf']:
	for line in range (len(sa[key])):
		sa[key][line] = list(sa[key][line])
with open ('sa_c_mt_cob.json','w') as file:
	json.dump(sa,file)


#SALVADOR
Y = pd.read_csv('y_c_ba_cob.csv')
Y = np.asarray(Y)
Y_new = [x for x in range(len(Y))]
for x in xrange(len(Y)):
	Y_new[x] = Y[x][0]
Y = Y_new
Y = np.asarray(Y)
#REALIZAR A ANÁLISE DE SENSIBILIDADE DE SOBOL
sa = sobol.analyze(problem,Y,print_to_console=False)
#TRANSFORMAR O ARQUIVO EM .JSON
for key in sa:
	sa[key] = list(sa[key])
for key in ['S2','S2_conf']:
	for line in range (len(sa[key])):
		sa[key][line] = list(sa[key][line])
with open ('sa_c_ba_cob.json','w') as file:
	json.dump(sa,file)


#MANAUS
Y = pd.read_csv('y_c_am_cob.csv')
Y = np.asarray(Y)
Y_new = [x for x in range(len(Y))]
for x in xrange(len(Y)):
	Y_new[x] = Y[x][0]
Y = Y_new
Y = np.asarray(Y)
#REALIZAR A ANÁLISE DE SENSIBILIDADE DE SOBOL
sa = sobol.analyze(problem,Y,print_to_console=False)
#TRANSFORMAR O ARQUIVO EM .JSON
for key in sa:
	sa[key] = list(sa[key])
for key in ['S2','S2_conf']:
	for line in range (len(sa[key])):
		sa[key][line] = list(sa[key][line])
with open ('sa_c_am_cob.json','w') as file:
	json.dump(sa,file)
