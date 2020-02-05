# READ ME
'''
what? e um codigo de python que cria arquivos idf, base de simulacoes no energyplus, sequencial e
	automaticamente.
why? precisamos desenvolver uma ferramenta simples, de facil uso e rapida, que crie variacoes a
	partir de um idf base. assim, pode-se fazer analises complexas a partir de um numero
	consideravel de simulacoes e pode-se gerar a materia prima para o desenvolvimento de modelos de
	predicao.
where? no labeee, um ambiente full of programadores/simuladores que estao sedentos por analises de
	sensibilidade e predicoes acuradas.
who? a abelha rainha @marceloso desenvolveu o codigo e o disponibilizou a todas as abelhas
	operarias. logo, todos podem o utilizar e a produtividade coletiva aumenta consideravelmente,
	beneficiando o grupo, inclusive a abelha rainha.
how? o codigo reune arquivos de texto (tripas) contendo os diversos objetos necessarios para
	simulacoes no energyplus e os cola, um apos o outro, gerando um arquivo unico no formato '.idf'.
	esse arquivo pode ser aberto no ep-lunch e a simulacao podeser realizada.

passo a passo para utilizar o codigo:
	1. crie as tripas de idf (em formato '.txt') separadamente.
		as tripas necessarias para uma simulacao completa, seguindo as diretrizes do labeee, sao:
			a) system_and_rules.txt;
			b) zone.txt;
			c) ground.txt;
			d) material.txt';
			e) construction.txt;
			f) surface.txt;
			g) fenestration.txt;
			h) internal_gain.txt;
			i) schedule.txt;
			j) shading.txt;
			k) airflow.txt;
			l) hvac.txt;
			m) ems.txt;
			n) output.txt.
		obs.1: os objetos do item 'a' ao item 'e' sao estritamente necessarios para qualquer
			simulacao, os objetos seguintes pertencem a simulacoes avancadas e sao opcionais;
		obs.1: caso seja necessario realizar uma simulacao com um numero menor de objetos (sem
			sistema de condicionamento de ar, por exemplo), e importante lembrar que alteracoes em
			objetos de outras tripas podem ser necessarias;
		obs.2: a funcao nao limita o numero de tripas, sendo assim, novas tripas podem ser
			desenvolvidas e adicionadas ao argumento 'input_files[]' da funcao 'idf_bundler'.
	2. abaixo do codigo e dentro da funcao 'idf_bundler()', preencha o argumento 'input_files=[]'
		com as tripas criadas (entre aspas), separando-as por virgula;
	3. ainda dentro da funcao 'idf_bundler()', preencha o ultimo argumento com o nome do idf a ser
		criado (nao esqueca do '.idf' no fim do nome, para definir o tipo de arquivo como idf);
	4. dentro da funcao 'idf_bundler' separe os argumentos 'input_files[]' e 'output_names[]' por
		uma virgula.
	obs. final: nao altere o arquivo 'idf_bundler.py' e as tripas padroes disponibilizadas, copie e
		cole o codigo e as tripas e um novo diretorio e faca suas alteracoes la!
'''

# import libraries
import numpy as np

# idf_blunder()
	# the function bundles files with different idf objects to create one idf model
def idf_bundler(input_dir, object_files, output_dir, output_name):
    # input_dir - directory to pull the inputs
    # input_files - list of files with idf objects
    # output_dir - directory to push the output
    # output_name - the name of the resulted idf model
    print output_dir + output_name
    for i in range(len(object_files)):
        object_files[i] = open(input_dir + object_files[i], 'rb')
    with open(output_dir + output_name, 'wb') as input_file:
        for i in range(len(object_files)):
            for line in object_files[i]:
                input_file.write(line)

# application

floor = np.array(['inter'])
wrap = np.array(['tv'])

for a in floor:
	for b in wrap:
			if a == 'terreo':
				idf_bundler('/home/rodox/Dropbox/TEMP/00.single_zone/validation/seed/',
							['system_and_rules.txt', 'zone_terreo.txt', 'material.txt',
							'ground_terreo.txt', 'construction_' + b + '.txt', 'surface_terreo.txt',
							'fenestration.txt', 'internal_gain.txt', 'schedule_ems.txt',
							'airflow_ems.txt', 'hvac_ems.txt', 'output_ems.txt', 'ems.txt'],
							'/home/rodox/Dropbox/TEMP/00.single_zone/validation/idf/',
							'terreo_' + b + '.idf')
			else:
				idf_bundler('/home/rodox/Dropbox/TEMP/00.single_zone/validation/seed/',
							['system_and_rules.txt', 'zone_' + a + '.txt', 'material.txt',
							'construction_' + b + '.txt', 'surface_' + a + '.txt',
							'fenestration.txt', 'internal_gain.txt', 'schedule_ems.txt',
							'airflow_ems.txt', 'hvac_ems.txt', 'output_ems.txt', 'ems.txt'],
							'/home/rodox/Dropbox/TEMP/00.single_zone/validation/idf/',
							a + '_' + b + '.idf')
