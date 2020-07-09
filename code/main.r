# load libraries and global environment ####
pkgs = c('jsonlite', 'purrr', 'parallel', 'stringr')
lapply(pkgs, library, character.only = TRUE)
source('~/git/master/code/edit_seed.r')

sample = read.csv('./model/sample2.csv')


EditSeed(seed_path = './seed/seed1.json',
         area = 100,
         ratio = 0.8,
         height = 3,
         azimuth = 60,
         shell_wall = 'concreto_10cm',
         shell_roof = 'fibrocimento_concreto',
         abs_wall = 0.4,
         abs_roof = 0.7,
         wwr_liv = 0.4,
         wwr_dorm = 0.2,
         u_window = 5.7,
         shgc = 0.87,
         open_factor = 0.7,
         construction, fill, setup, geometry)