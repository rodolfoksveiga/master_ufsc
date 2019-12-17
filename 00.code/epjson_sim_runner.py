import os
import glob
import numpy as np
import pandas as pd

# epjson_sim_runner()
	# the function run simulations with energyplus using 'epjson' files
def epjson_sim_runner(epjson_dir, pattern, output_dir, epw_dir, weather_names):
	# epjson_dir - the directory path where the 'epjson' files are (you can put as many files as you
		# wish)
	# output_dir - the directory path where the outputs from the simulations will be
		# in the output directory will be only the errors (compiled in one file ('errors.txt')) and
			# the main 'csvs' files
		# the name of the output will have the following structure: name of the weather and the name
			# of the 'epjson' file (e.g. 'rio_de_janeiro_apto_oeste.csv')
	# epw_dir - the directory path where the 'epw' files are (you can put as many files as you wish)
	# weather_names - the names of the weathers (in alphabetic order and using underlines as spaces)
		# to be simulated (they must be in a numpy array)
		
	epjson_files = glob.glob(epjson_dir + pattern + '*.epJSON')
	list.sort(epjson_files)
	
	epjson_names = map(str,np.empty([len(epjson_files)]))
	for count,value in enumerate(epjson_files):
		epjson_names[count] = value
		epjson_names[count] = epjson_names[count].replace(epjson_dir, '')[:-7]

	epw_files = glob.glob(epw_dir + '*.epw')
	list.sort(epw_files)

	# run simulations
	sim_count = 0
	for count_1, value_1 in enumerate(epjson_files):
		for count_2, value_2 in enumerate(epw_files):
			sim_count += 1
			print '\nSimulation count: ', sim_count
			print 'Model file: ' + epjson_names[count_1] + '.epJSON'
			print 'Weather file: ' + value_2.replace(epw_dir, '')
			os.system('energyplus -r -w ' + value_2 + ' -d ' + output_dir + ' -p ' +
					  weather_names[count_2] + '_' + epjson_names[count_1] + '_ ' + value_1)

	# remove unuseful files from simulations
	os.system('rm ' + output_dir + '*tbl.csv ' + output_dir + '*.rvaudit ' + output_dir +
			  '*.audit ' + output_dir + '*.bnd ' + output_dir + '*.dxf ' + output_dir + '*.eio ' +
			  output_dir + '*.end ' + output_dir + '*.eso ' + output_dir + '*.mdd ' + output_dir +
			  '*.mtd ' + output_dir + '*.rdd ' + output_dir + '*.edd ' + output_dir + '*.shd ' +
			  output_dir + 'sqlite.err && rm sqlite.err')

	# bundle the 'err' files
	error_files = glob.glob(output_dir + '*.err')
	for value in error_files:
		with open(value) as input_file:
			with open(output_dir + pattern + '_errors.txt', 'a') as output_file:
				output_file.write(value + '\n')
				for line in input_file:
					output_file.write(line)
				output_file.write('\n\n')

	# remove errors files from simulations
	os.system('rm ' + output_dir + '*.err')

	# rename files without "_out"
	output_files = glob.glob(output_dir + '*.csv')
	for value in output_files:
		os.system('mv ' + value + ' ' + value[:-8] + value[-4:])	
	return;


# application
# sz
'''
epjson_sim_runner(epjson_dir = '/home/rodox/Dropbox/00.master_ufsc/00.single_zone/01.validation/00.sz/08.hvac_v04/02.steal_frame/00.model/',
				  output_dir = '/home/rodox/Dropbox/00.master_ufsc/00.single_zone/01.validation/00.sz/08.hvac_v04/02.steal_frame/01.result/',
				  epw_dir = '/home/rodox/Dropbox/00.master_ufsc/00.single_zone/03.source/00.epw/',
				  weather_names = np.array(['rio_de_janeiro', 'sao_paulo']))
'''
# multi
'''
epjson_sim_runner(epjson_dir = '/home/rodox/Dropbox/00.master_ufsc/00.single_zone/01.validation/01.multi/01.hvac_v01/02.steal_frame/00.model/',
				  output_dir = '/home/rodox/Dropbox/00.master_ufsc/00.single_zone/01.validation/01.multi/01.hvac_v01/02.steal_frame/01.result/',
				  epw_dir = '/home/rodox/Dropbox/00.master_ufsc/00.single_zone/03.source/00.epw/',
				  weather_names = np.array(['rio_de_janeiro', 'sao_paulo']))
'''
# simplifications

# v00
# sf
'''
epjson_sim_runner(epjson_dir = '/home/rodox/01.going_on/00.hive/00.hyp/00.hyp_v00/02.sf/',
				  pattern = 'hyp_sf_v00',
				  output_dir = '/home/rodox/01.going_on/00.hive/00.hyp/00.hyp_v00/02.sf/',
				  epw_dir = '/home/rodox/00.git/00.master_ufsc/05.source/00.epw/',
				  weather_names = np.array(['curitiba', 'rio_de_janeiro', 'sao_paulo']))
'''
# tv
'''
epjson_sim_runner(epjson_dir = '/home/rodox/01.going_on/00.hive/00.hyp/00/01.tv/',
				  pattern = 'hyp_tv_v00',
				  output_dir = '/home/rodox/01.going_on/00.hive/00.hyp/00/01.tv/',
				  epw_dir = '/home/rodox/00.git/00.master_ufsc/05.source/00.epw/',
				  weather_names = np.array(['curitiba', 'rio_de_janeiro', 'sao_paulo']))
'''
# c10
'''
epjson_sim_runner(epjson_dir = '/home/rodox/01.going_on/00.hive/00.hyp/00/00.c10/',
				  pattern = 'hyp_c10_v00',
				  output_dir = '/home/rodox/01.going_on/00.hive/00.hyp/00/00.c10/',
				  epw_dir = '/home/rodox/00.git/00.master_ufsc/05.source/00.epw/',
				  weather_names = np.array(['curitiba', 'rio_de_janeiro', 'sao_paulo']))
'''
# v01
# c10
'''
epjson_sim_runner(epjson_dir = '/home/rodox/01.going_on/00.hive/00.hyp/01.hyp_v01/00.c10/',
				  pattern = 'hyp_c10_v01',
				  output_dir = '/home/rodox/01.going_on/00.hive/00.hyp/01.hyp_v01/00.c10/',
				  epw_dir = '/home/rodox/00.git/00.master_ufsc/05.source/00.epw/',
				  weather_names = np.array(['curitiba','rio_de_janeiro', 'sao_paulo']))
'''
# tv
'''
epjson_sim_runner(epjson_dir = '/home/rodox/01.going_on/00.hive/00.hyp/01.hyp_v01/01.tv/',
				  pattern = 'hyp_tv_v01',
				  output_dir = '/home/rodox/01.going_on/00.hive/00.hyp/01.hyp_v01/01.tv/',
				  epw_dir = '/home/rodox/00.git/00.master_ufsc/05.source/00.epw/',
				  weather_names = np.array(['curitiba','rio_de_janeiro', 'sao_paulo']))
'''
# sf
'''
epjson_sim_runner(epjson_dir = '/home/rodox/01.going_on/00.hive/00.hyp/01.hyp_v01/02.sf/',
				  pattern = 'hyp_sf_v01',
				  output_dir = '/home/rodox/01.going_on/00.hive/00.hyp/01.hyp_v01/02.sf/',
				  epw_dir = '/home/rodox/00.git/00.master_ufsc/05.source/00.epw/',
				  weather_names = np.array(['curitiba','rio_de_janeiro', 'sao_paulo']))
'''
# v02
# c10
'''
epjson_sim_runner(epjson_dir = '/home/rodox/01.going_on/00.hive/00.hyp/02.hyp_v02/00.c10/',
				  pattern = 'hyp_c10_v02',
				  output_dir = '/home/rodox/01.going_on/00.hive/00.hyp/02.hyp_v02/00.c10/',
				  epw_dir = '/home/rodox/00.git/00.master_ufsc/05.source/00.epw/',
				  weather_names = np.array(['curitiba','rio_de_janeiro', 'sao_paulo']))
'''
# tv
'''
epjson_sim_runner(epjson_dir = '/home/rodox/01.going_on/00.hive/00.hyp/02.hyp_v02/01.tv/',
				  pattern = 'hyp_tv_v02',
				  output_dir = '/home/rodox/01.going_on/00.hive/00.hyp/02.hyp_v02/01.tv/',
				  epw_dir = '/home/rodox/00.git/00.master_ufsc/05.source/00.epw/',
				  weather_names = np.array(['curitiba','rio_de_janeiro', 'sao_paulo']))
'''
# sf
'''
epjson_sim_runner(epjson_dir = '/home/rodox/01.going_on/00.hive/00.hyp/02.hyp_v02/02.sf/',
				  pattern = 'hyp_sf_v02',
				  output_dir = '/home/rodox/01.going_on/00.hive/00.hyp/02.hyp_v02/02.sf/',
				  epw_dir = '/home/rodox/00.git/00.master_ufsc/05.source/00.epw/',
				  weather_names = np.array(['curitiba','rio_de_janeiro', 'sao_paulo']))
'''
# v03
# c10
'''
epjson_sim_runner(epjson_dir = '/home/rodox/00.git/00.master_ufsc/02.model/01.hvac/00.hyp/03/00.c10/',
				  pattern = 'hyp_c10_v03',
				  output_dir = '/home/rodox/01.going_on/00.hive/00.hyp/03/00.c10/',
				  epw_dir = '/home/rodox/00.git/00.master_ufsc/05.source/00.epw/',
				  weather_names = np.array(['curitiba','rio_de_janeiro', 'sao_paulo']))
'''
# tv
'''
epjson_sim_runner(epjson_dir = '/home/rodox/00.git/00.master_ufsc/02.model/01.hvac/00.hyp/03/01.tv/',
				  pattern = 'hyp_tv_v03',
				  output_dir = '/home/rodox/01.going_on/00.hive/00.hyp/03/01.tv/',
				  epw_dir = '/home/rodox/00.git/00.master_ufsc/05.source/00.epw/',
				  weather_names = np.array(['curitiba','rio_de_janeiro', 'sao_paulo']))
'''
# sf
'''
epjson_sim_runner(epjson_dir = '/home/rodox/00.git/00.master_ufsc/02.model/01.hvac/00.hyp/03/02.sf/',
				  pattern = 'hyp_sf_v03',
				  output_dir = '/home/rodox/01.going_on/00.hive/00.hyp/03/02.sf/',
				  epw_dir = '/home/rodox/00.git/00.master_ufsc/05.source/00.epw/',
				  weather_names = np.array(['curitiba','rio_de_janeiro', 'sao_paulo']))
'''
# v04
# c10

epjson_sim_runner(epjson_dir = '/home/rodox/00.git/00.master_ufsc/02.model/01.hvac/00.hyp/04/00.c10/',
				  pattern = 'hyp_c10_v04',
				  output_dir = '/home/rodox/01.going_on/00.hive/00.hyp/04/00.c10/',
				  epw_dir = '/home/rodox/00.git/00.master_ufsc/05.source/00.epw/',
				  weather_names = np.array(['curitiba','rio_de_janeiro', 'sao_paulo']))

# tv
'''
epjson_sim_runner(epjson_dir = '/home/rodox/00.git/00.master_ufsc/02.model/01.hvac/00.hyp/04/01.tv/',
				  pattern = 'hyp_tv_v04',
				  output_dir = '/home/rodox/01.going_on/00.hive/00.hyp/04/01.tv/',
				  epw_dir = '/home/rodox/00.git/00.master_ufsc/05.source/00.epw/',
				  weather_names = np.array(['curitiba','rio_de_janeiro', 'sao_paulo']))
'''
# sf
'''
epjson_sim_runner(epjson_dir = '/home/rodox/00.git/00.master_ufsc/02.model/01.hvac/00.hyp/04/02.sf/',
				  pattern = 'hyp_sf_v04',
				  output_dir = '/home/rodox/01.going_on/00.hive/00.hyp/04/02.sf/',
				  epw_dir = '/home/rodox/00.git/00.master_ufsc/05.source/00.epw/',
				  weather_names = np.array(['curitiba','rio_de_janeiro', 'sao_paulo']))
'''
