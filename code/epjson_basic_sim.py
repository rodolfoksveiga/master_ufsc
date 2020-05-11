import os
import re 

def basic_sim_runner(sim_file, epw_file, output_dir = 'input_dir', convert = False):
	if output_dir == 'input_dir':
		output_dir = os.path.dirname(sim_file) + '/'
	print '\nModel file: ' + sim_file
	print 'Weather file: ' + epw_file
	print 'Output directory: ' + output_dir
	if convert == False:
		os.system('energyplus -r -w ' + epw_file + ' -p ' + output_dir + ' ' + sim_file)
	else:
		os.system('energyplus -r -c -w ' + epw_file + ' -p ' + output_dir + ' ' + sim_file)
	os.system('rm sqlite.err')
	return;

# application
basic_sim_runner(sim_file = '/home/rodox/Desktop/case_1087_afn.epJSON',
				 epw_file = '/home/rodox/00.git/00.master_ufsc/06.source/00.epw/br_rio_de_janeiro_rj_santos_dumont_ap_tmy_2003-2017.epw',
				 convert = False)

'''
# epw files
'/home/rodox/00.git/00.master_ufsc/06.source/00.epw/br_curitiba_pr_bacacheri_ap_tmy_2003-2017.epw'
'/home/rodox/00.git/00.master_ufsc/06.source/00.epw/br_sao_paulo_sp_congonhas_ap_tmy_2003-2017.epw'
'/home/rodox/00.git/00.master_ufsc/06.source/00.epw/br_rio_de_janeiro_santos_dumont_ap_tmy_2003-2017.epw'
'''