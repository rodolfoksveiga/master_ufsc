# load global environment ####
source('~/git/master/code/build_model.r')
load('~/git/master/seed/geometry.rds')

# base functions ####
# move fenestration
MoveFenestration = function(vertices, ratio, axis, is_bwc = FALSE) {
  v1 = vertices[[1]]
  v3 = vertices[[3]]
  l = (v3 - v1)/2
  center = (v1 + l)*ratio
  c = ifelse(v3 > v1, 1, -1)
  if (axis != 'z') {
    loop = c(-1, -1, 1, 1)
    vertices = sapply(loop, function(x, y, z) y + x*z, center, l)
  } else {
    if (is_bwc) {
      loop = c(-1, 1, 1, -1)
      vertices = sapply(loop, function(x, y, z) y + x*z, center, l)
    }
  }
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
    vertices = MoveFenestration(door[coords], ratio, axis)
  } else {
    vertices = sapply(door[coords], ScaleVertex, ratio)
  }
  return(vertices)
}
# resize window
ResizeWindow = function(vertices, ratio, wwr, axis) {
  v1 = vertices[[1]]
  v3 = vertices[[3]]
  l = (v3 - v1)/2
  center = (v1 + l)*ratio
  l = l*ratio*wwr
  c = ifelse(v3 > v1, 1, -1)
  if (axis != 'z') {
    loop = c(-1, -1, 1, 1)
  } else {
    loop = c(-1, 1, 1, -1)
  }
  vertices = sapply(loop, function(x, y, z) y + x*z, center, l)
  return(vertices)
}
# scale window
ScaleWindow = function(axis, ratio, is_bwc, wwr, window) {
  coords = paste0('vertex_', 1:4, '_', axis, '_coordinate')
  window = keep(window, names(window) %in% coords)
  if (is_bwc) {
    vertices = MoveFenestration(window[coords], ratio, axis, is_bwc)
  } else {
    vertices = ResizeWindow(window[coords], ratio, wwr, axis)
  }
  return(vertices)
}
# scale an object
ScaleObject = function(object, type, ratio, wwr) {
  axis = c('x', 'y', 'z')
  if (type == 'zone') {
    coords = paste0(axis[1:2], '_origin')
    object[coords] = mapply(ScaleVertex, object[coords], ratio[1:2])
  } else if (type == 'surf') {
    object$vertices = lapply(object$vertices, ScaleSurf, ratio, axis)
  } else {
    coords = paste0('vertex_', 1:4, '_', rep(axis, each = 4), '_coordinate')
    if (object$surface_type == 'Door') {
      object[coords] = mapply(ScaleDoor, axis, ratio, SIMPLIFY = TRUE,
                              MoreArgs = list(object[coords]))
    } else {
      wwr = ifelse(grepl('liv', object$building_surface_name), wwr[['liv']], wwr[['dorm']])
      is_bwc = grepl('bwc', object$building_surface_name)
      object[coords] = mapply(ScaleWindow, axis, ratio, is_bwc, wwr,
                              SIMPLIFY = TRUE, MoreArgs = list(object[coords]))
    }
  }
  return(object)
}
# scale group of objects (apply ScaleObject)
ScaleGroup = function(group, type, ratio, wwr) lapply(group, ScaleObject, type, ratio, wwr)

# main function ####
BuildCase = function(seed_path, area, ratio, height, azimuth, shell_wall, shell_roof,
                     abs_wall, abs_roof, wwr_liv, wwr_dorm, u_window, shgc, open_factor,
                     model_path, construction, fill, setup, geometry) {
  # seed_path: seed file path
  # area: sum of the long occupancy rooms (living rooms and dormitories) [30 ~ 150]
  # ratio: ratio between the 'y' and the 'x' axis [0.25 ~ 4]
  # azimuth: azimuth angle related to south orientation [0 ~ 360]
  # shell_wall: wall construction materials composition
  # shell_roof: roof construction materials composition
  # abs_wall: solar absorptance of the walls [0.2 ~ 0.9]
  # abs_roof: solar absorptance of the roof [0.2 ~ 0.9]
  # wwr_liv: window to wall ratio in the living room (weighted average) [0.1 ~ 0.8]
  # wwr_dorm: window to wall ratio in the dormitories (weighted average) [0.1 ~ 0.8]
  # u_window: solar transmitance of the windows (mean) [2.8 ~ 5.7]
  # shgc: solar heat gain coefficient of the windows (mean) [0.22 ~ 0.87]
  # open_factor: open factor (weighted average) [0.4 ~ 1]
  # output_dir: output directory
  # construction, fill, setup and geometry: auxiliar files
  seed = read_json(seed_path)
  index = seed_path %>% str_extract('[0-9](?=\\.json)') %>% as.numeric()
  adjust = 1/geometry[[index]]$ratio
  ratio = ratio*adjust
  area_seed = geometry[[index]]$area %>% flatten_dbl() %>% sum()
  multiplier = sqrt(area/(area_seed*ratio))
  ratio = c('x' = multiplier, 'y' = ratio*multiplier, 'z' = height/2.6)
  wwr = sqrt(c('liv' = wwr_liv, 'dorm' = wwr_dorm)/0.17)
  seed = mapply(ScaleGroup, seed, c('zone', 'surf', 'fen'),
                SIMPLIFY = FALSE, MoreArgs = list(ratio, wwr))
  model = BuildModel(seed, shell_wall, shell_roof, 3, 'surface', construction, fill, setup, FALSE)
  model$Building[[1]]$north_axis = azimuth
  model$'WindowMaterial:SimpleGlazingSystem'$vidro$u_factor = u_window
  model$'WindowMaterial:SimpleGlazingSystem'$vidro$solar_heat_gain_coefficient = shgc
  pos = 'width_factor_for_opening_factor_2'
  model$'AirflowNetwork:MultiZone:Component:DetailedOpening'$window_opening[[pos]] = open_factor
  outside_layer_wall = model$'Construction'$'ext_wall'$'outside_layer'
  model$'Material'[[outside_layer_wall]]$'solar_absorptance' = abs_wall
  outside_layer_roof = model$'Construction'$'roof'$'outside_layer'
  model$'Material'[[outside_layer_roof]]$'solar_absorptance' = abs_roof
  write_json(model, model_path, pretty = TRUE, auto_unbox = TRUE)
}