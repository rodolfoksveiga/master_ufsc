# load libraries
pkgs = c('jsonlite', 'purrr', 'parallel', 'stringr')
lapply(pkgs, library, character.only = TRUE)

# build_model.r
source('~/git/master/code/build_model.r')
grid = expand.grid('shell' = c('ref17', 'ref8', 'tm', 'tv', 'sf'),
                   'boundary' = c('surface', 'adiabatic'),
                   stringsAsFactors = FALSE)
geom_path = '/home/rodox/git/master_ufsc/seed/linear.json'
n_strs = 5
output_dir = '/home/rodox/git/master_ufsc/model/'
mcmapply(BuildModel, TRUE, geom_path, grid$shell,
         grid$boundary, 5, output_dir, mc.cores = detectCores())