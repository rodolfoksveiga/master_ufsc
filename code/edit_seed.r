# load libraries, functions and global environment ####
# load libraries
pkgs = c('jsonlite', 'purrr', 'parallel', 'stringr')
lapply(pkgs, library, character.only = TRUE)
source('~/git/master_ufsc/code/build_model.r')
load('~/git/master_ufsc/seed/geometry.rds')

# main function ####
EditSeed = function(geom_path, area, ratio) {
  seed_path = '~/git/master_ufsc/seed/seed1.epJSON'
  model = read_json(seed_path)
  
  index = seed_path %>% str_extract('[0-9](?=\\.epJSON)') %>% as.numeric()
  ratio = ratio*(1/geometry[[index]]$ratio)
}

seed_path = '~/git/master_ufsc/seed/seed1.epJSON'
model = read_json(seed_path)
area = 45.49629*2
ratio = 1

index = seed_path %>% str_extract('[0-9](?=\\.epJSON)') %>% as.numeric()
adjust = 1/geometry[[index]]$ratio # turns the ratio equal to 1
area_seed = geometry[[index]]$area %>% flatten_dbl() %>% sum()
mult = sqrt(area/(area_seed*adjust))
ratio = c('x' = mult, 'y' = ratio*mult)

ScaleVertex = function(coord, ratio) coord*ratio

ScaleSurf = function(vertex, ratio, axis) {
  coords = paste0('vertex_', axis, '_coordinate')
  vertex[coords] = mapply(ScaleVertex, vertex[coords], ratio)
  return(vertex)
}

ScaleDoor = function (axis, ratio, door) {
  coords = paste0('vertex_', 1:4, '_', axis, '_coordinate')
  if (ratio > 1) {
    v1 = door[[coords[1]]]
    v3 = door[[coords[3]]]
    l = (v3 - v1)/2
    center = (v1 + l)*ratio
    c = ifelse(v3 > v1, 1, -1)
    vertices = sapply(rep(c(-1, 1), each = 2),
                      function(x, y, z) y + x*z, center, l)
  } else {
    vertices = sapply(door[coords], ScaleVertex, ratio)
  }
  return(vertices)
}

ScaleWindow = function(axis, ratio, window) {
  window = keep(window, grepl(paste0('_', axis, '_'), names(window)))
  vertices = lapply(window, ScaleVertex, ratio)
  return(vertices)
}

ScaleObject = function(object, type, ratio) {
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
      object[coords] = mapply(ScaleWindow, axis, ratio, SIMPLIFY = TRUE,
                              MoreArgs = list(object[coords]))
    }
  }
  return(object)
}

ScaleGroup = function(group, type, ratio) lapply(group, ScaleObject, type, ratio)

model = mapply(ScaleGroup, model, c('surf', 'fen', 'zone'),
               SIMPLIFY = FALSE, MoreArgs = list(ratio))

