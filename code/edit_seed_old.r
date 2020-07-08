# load libraries, functions and global environment ####
# load libraries
pkgs = c('jsonlite', 'purrr', 'parallel', 'stringr')
lapply(pkgs, library, character.only = TRUE)
source('~/git/master/code/build_model.r')
load('~/git/master/seed/geometry.rds')

# base functions ####
# move fenestration
MoveFenestration = function(vertices, ratio) {
  v1 = vertices[[1]]
  v3 = vertices[[3]]
  l = (v3 - v1)/2
  center = (v1 + l)*ratio
  c = ifelse(v3 > v1, 1, -1)
  vertices = sapply(rep(c(-1, 1), each = 2), function(x, y, z) y + x*z, center, l)
  return(vertices)
}
# scale one vertex of a surface
ScaleVertex = function(coord, ratio) coord*ratio
# scale surface
ScaleSurf = function(vertex, ratio, axis) {
  coords = paste0('vertex_', axis, '_coordinate')
  vertex[coords] = mapply(ScaleVertex, vertex[coords], ratio)
  return(vertex)
}
# scale door
ScaleDoor = function (axis, ratio, door) {
  coords = paste0('vertex_', 1:4, '_', axis, '_coordinate')
  if (ratio > 1) {
    vertices = MoveFenestration(door[coords], ratio)
  } else {
    vertices = sapply(door[coords], ScaleVertex, ratio)
  }
  return(vertices)
}
# scale window
ScaleWindow = function(axis, ratio, is_bwc, window) {
  # window = object
  # axis = 'x'
  # ratio = ratio[['x']]
  # is_bwc = grepl('bwc', object$building_surface_name)
  
  coords = paste0('vertex_', 1:4, '_', axis, '_coordinate')
  window = keep(window, names(window) %in% coords)
  if (is_bwc) {
    vertices = MoveFenestration(window[coords], ratio)
  } else {
    vertices = lapply(window, ScaleVertex, ratio)
  }
  return(vertices)
}
# scale an object
ScaleObject = function(object, type, ratio) {
  # object = seed$`FenestrationSurface:Detailed`$bwc1_window3
  # type = 'fen'
  
  axis = c('x', 'y')
  if (type == 'zone') {
    coords = paste0(axis, '_origin')
    object[coords] = mapply(ScaleVertex, object[coords], ratio)
  } else if (type == 'surf') {
    object$vertices = lapply(object$vertices, ScaleSurf, ratio, axis)
  } else {
    coords = paste0('vertex_', 1:4, '_', rep(axis, each = 4), '_coordinate')
    if (object$surface_type == 'Door') {
      object[coords] = mapply(ScaleDoor, axis, ratio, SIMPLIFY = TRUE,
                              MoreArgs = list(object[coords]))
    } else {
      is_bwc = grepl('bwc', object$building_surface_name)
      object[coords] = mapply(ScaleWindow, axis, ratio, is_bwc,
                              SIMPLIFY = TRUE, MoreArgs = list(object[coords]))
    }
  }
  return(object)
}
# scale group of objects (apply ScaleObject)
ScaleGroup = function(group, type, ratio) lapply(group, ScaleObject, type, ratio)

# main function ####
EditSeed = function(seed_path, area, ratio, azi, shell, wwr_liv, wwr_dorm,
                    u_window, open_fac, abs_wall, abs_roof, geometry) {
  seed_path = '~/git/master_ufsc/seed/seed1.epJSON'
  seed = read_json(seed_path)
  
  index = seed_path %>% str_extract('[0-9](?=\\.epJSON)') %>% as.numeric()
  adjust = 1/geometry[[index]]$ratio
  area_seed = geometry[[index]]$area %>% flatten_dbl() %>% sum()
  mult = sqrt(area/(area_seed*adjust))
  ratio = c('x' = mult, 'y' = ratio*mult)
  
  seed = mapply(ScaleGroup, seed, c('surf', 'fen', 'zone'),
                SIMPLIFY = FALSE, MoreArgs = list(ratio))
  model = BuildModel(shell = 'tv', boundary = 'surface', n_strs = 3, lsm = FALSE,
                     seed = seed, construction = construction, fill = fill, setup = setup)
  
  
}

# test
seed_path = '~/git/master/seed/seed1.json'
seed = read_json(seed_path)
area = 300
ratio = 1/4

index = seed_path %>% str_extract('[0-9](?=\\.json)') %>% as.numeric()
adjust = 1/geometry[[index]]$ratio
ratio = ratio*adjust
area_seed = geometry[[index]]$area %>% flatten_dbl() %>% sum()
multiplier = sqrt(area/(area_seed*ratio))
ratio = c('x' = multiplier, 'y' = ratio*multiplier)

seed = mapply(ScaleGroup, seed, c('surf', 'fen', 'zone'),
              SIMPLIFY = FALSE, MoreArgs = list(ratio))

model = BuildModel(shell = 'tv', boundary = 'surface', n_strs = 1, lsm = FALSE,
                   seed = seed, construction = construction, fill = fill, setup = setup)

write_json(model, '~/Documents/test/test.epJSON', pretty = T, auto_unbox = T)
