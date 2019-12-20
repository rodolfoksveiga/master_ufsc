import os
import re 

def basic_sim_runner(sim_file, epw_file, output_dir = 'input_dir', convert = False):
	if output_dir == 'input_dir':
		output_dir = os.path.dirname(sim_file) + '/'
	print '\nModel file: ' + sim_file
	print 'Weather file: ' + epw_file
	print 'Output directory: ' + output_dir
	if convert == False:
		os.system('energyplus -r -x -w ' + epw_file + ' -p ' + output_dir + ' ' + sim_file)
	else:
		os.system('energyplus -r -x -c -w ' + epw_file + ' -p ' + output_dir + ' ' + sim_file)
	return;

# application
basic_sim_runner(sim_file = '/home/rodox/Desktop/test/EMS_SP_R02.idf',
				 epw_file = '/home/rodox/00.git/00.master_ufsc/05.source/00.epw/br_sao_paulo_sp_congonhas_ap_tmy_2003-2017.epw',
				 convert = True)
