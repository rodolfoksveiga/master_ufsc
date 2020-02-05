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

	os.system('rm ' + output_dir + pattern + '_errors.txt')
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
# v00
'''
wraps = np.array(['c10', 'tv', 'sf'])
for count, value in enumerate(wraps):
	epjson_sim_runner(epjson_dir = '/home/rodox/00.git/00.master_ufsc/02.model/00.hyp/00/',
					  pattern = 'hyp_' + value + '_v00_hvac',
					  output_dir = '/home/rodox/Desktop/01.new/00.hyp/00/0' + str(count) + '.' +
								   value + '/',
					  epw_dir = '/home/rodox/00.git/00.master_ufsc/05.source/00.epw/',
					  weather_names = np.array(['curitiba', 'rio_de_janeiro', 'sao_paulo']))
'''
# v01
'''
wraps = np.array(['c10', 'tv', 'sf'])
for count, value in enumerate(wraps):
	epjson_sim_runner(epjson_dir = '/home/rodox/00.git/00.master_ufsc/02.model/00.hyp/01/',
					  pattern = 'hyp_' + value + '_v01_hvac',
					  output_dir = '/home/rodox/Desktop/01.new/00.hyp/01/0' + str(count) + '.' +
								   value + '/',
					  epw_dir = '/home/rodox/00.git/00.master_ufsc/05.source/00.epw/',
					  weather_names = np.array(['curitiba', 'rio_de_janeiro', 'sao_paulo']))
'''
# v02
'''
wraps = np.array(['c10', 'tv', 'sf'])
storeys = np.array(['floor', 'inter', 'roof'])
for count_1, value_1 in enumerate(wraps):
	for count_2, value_2 in enumerate(storeys):
		epjson_sim_runner(epjson_dir = '/home/rodox/00.git/00.master_ufsc/02.model/00.hyp/02/',
						  pattern = 'hyp_' + value_1 + '_v02_' + value_2 + '_afn',
						  output_dir = '/home/rodox/01.going_on/00.hive/00.hyp/02/0' + str(count_1) + '.' + value_1 + '/0' + str(count_2) + '.' + value_2 + '/00.afn/',
						  epw_dir = '/home/rodox/00.git/00.master_ufsc/05.source/00.epw/',
						  weather_names = np.array(['curitiba', 'rio_de_janeiro', 'sao_paulo']))
'''
# v03
'''
wraps = np.array(['c10', 'tv', 'sf'])
storeys = np.array(['floor', 'inter', 'roof'])
conds = np.array(['afn', 'hvac'])
for count_1, value_1 in enumerate(wraps):
	for count_2, value_2 in enumerate(storeys):
		for count_3, value_3 in enumerate(conds):
			epjson_sim_runner(epjson_dir = '/home/rodox/00.git/00.master_ufsc/02.model/00.hyp/03/',
							  pattern = 'hyp_' + value_1 + '_v03_' + value_2 + '_' + value_3,
							  output_dir = '/home/rodox/01.going_on/00.hive/00.hyp/03/0' +
										   str(count_1) + '.' + value_1 + '/0' + str(count_2) + '.'
										   + value_2 + '/0' + str(count_3) + '.' + value_3 + '/',
							  epw_dir = '/home/rodox/00.git/00.master_ufsc/05.source/00.epw/',
							  weather_names = np.array(['curitiba', 'rio_de_janeiro', 'sao_paulo']))
'''
# v04
'''
wraps = np.array(['c10', 'tv', 'sf'])
storeys = np.array(['floor', 'inter', 'roof'])
conds = np.array(['afn', 'hvac'])
for count_1, value_1 in enumerate(wraps):
	for count_2, value_2 in enumerate(storeys):
		for count_3, value_3 in enumerate(conds):
			epjson_sim_runner(epjson_dir = '/home/rodox/00.git/00.master_ufsc/02.model/00.hyp/04/',
							  pattern = 'hyp_' + value_1 + '_v04_' + value_2 + '_' + value_3,
							  output_dir = '/home/rodox/01.going_on/00.hive/00.hyp/04/0' +
										   str(count_1) + '.' + value_1 + '/0' + str(count_2) + '.'
										   + value_2 + '/0' + str(count_3) + '.' + value_3 + '/',
							  epw_dir = '/home/rodox/00.git/00.master_ufsc/05.source/00.epw/',
							  weather_names = np.array(['curitiba', 'rio_de_janeiro', 'sao_paulo']))
'''
# v05
'''
wraps = np.array(['c10', 'tv', 'sf'])
storeys = np.array(['floor', 'inter', 'roof'])
conds = np.array(['afn', 'hvac'])
for count_1, value_1 in enumerate(wraps):
	for count_2, value_2 in enumerate(storeys):
		for count_3, value_3 in enumerate(conds):
			epjson_sim_runner(epjson_dir = '/home/rodox/00.git/00.master_ufsc/02.model/00.hyp/05/',
							  pattern = 'hyp_' + value_1 + '_v05_' + value_2 + '_' + value_3,
							  output_dir = '/home/rodox/01.going_on/00.hive/00.hyp/05/0' +
										   str(count_1) + '.' + value_1 + '/0' + str(count_2) + '.'
										   + value_2 + '/0' + str(count_3) + '.' + value_3 + '/',
							  epw_dir = '/home/rodox/00.git/00.master_ufsc/05.source/00.epw/',
							  weather_names = np.array(['curitiba', 'rio_de_janeiro', 'sao_paulo']))
'''
# v06
'''
wraps = np.array(['c10', 'tv', 'sf'])
storeys = np.array(['floor', 'inter', 'roof'])
conds = np.array(['afn', 'hvac'])
for count_1, value_1 in enumerate(wraps):
	for count_2, value_2 in enumerate(storeys):
		for count_3, value_3 in enumerate(conds):
			epjson_sim_runner(epjson_dir = '/home/rodox/00.git/00.master_ufsc/02.model/00.hyp/06/',
							  pattern = 'hyp_' + value_1 + '_v98_' + value_2 + '_' + value_3,
							  output_dir = '/media/rodox/HD_EXTERNO/00.hive/00.hyp/06/0' +
										   str(count_1) + '.' + value_1 + '/0' + str(count_2) + '.'
										   + value_2 + '/0' + str(count_3) + '.' + value_3 + '/',
							  epw_dir = '/home/rodox/00.git/00.master_ufsc/05.source/00.epw/',
							  weather_names = np.array(['curitiba', 'rio_de_janeiro', 'sao_paulo']))
'''
# v07
'''
wraps = np.array(['c10', 'tv', 'sf'])
storeys = np.array(['floor', 'inter', 'roof'])
conds = np.array(['afn', 'hvac'])
for count_1, value_1 in enumerate(wraps):
	for count_2, value_2 in enumerate(storeys):
		for count_3, value_3 in enumerate(conds):
			epjson_sim_runner(epjson_dir = '/home/rodox/00.git/00.master_ufsc/02.model/00.hyp/07/',
							  pattern = 'hyp_' + value_1 + '_v99_' + value_2 + '_' + value_3,
							  output_dir = '/media/rodox/HD_EXTERNO/00.hive/00.hyp/07/0' +
										   str(count_1) + '.' + value_1 + '/0' + str(count_2) + '.'
										   + value_2 + '/0' + str(count_3) + '.' + value_3 + '/',
							  epw_dir = '/home/rodox/00.git/00.master_ufsc/05.source/00.epw/',
							  weather_names = np.array(['curitiba', 'rio_de_janeiro', 'sao_paulo']))
'''


wraps = np.array(['sf']) # count_1
storeys = np.array(['roof']) # count_2
conds = np.array(['afn']) # count_3
count_1 = 2
for value_1 in wraps:
	count_2 = 2
	for value_2 in storeys:
		count_3 = 0
		for value_3 in conds:
			epjson_sim_runner(epjson_dir = '/home/rodox/00.git/00.master_ufsc/02.model/00.hyp/98/',
							  pattern = 'hyp_' + value_1 + '_v98_' + value_2 + '_' + value_3,
							  output_dir = '/home/rodox/Desktop/98/0' +
										   str(count_1) + '.' + value_1 + '/0' + str(count_2) + '.'
										   + value_2 + '/0' + str(count_3) + '.' + value_3 + '/',
							  epw_dir = '/home/rodox/00.git/00.master_ufsc/05.source/00.epw/',
							  weather_names = np.array(['curitiba', 'rio_de_janeiro', 'sao_paulo']))
			count_3 = count_3 + 1
		count_2 = count_2 + 1
	count_1 = count_1 + 1
							
