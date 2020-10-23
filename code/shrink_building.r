# base functions ####
# define coordinates according to orientation
DefCoord = function(side) ifelse(side %in% c(1, 3), 'x', 'y')
# define start and end points to calculate the width
DefStartEnd = function(side) c(ifelse(IsSE(side), 3, 1), ifelse(IsSE(side), 1, 3))
# edit airflow network surfaces
EditAFNSurfs = function(group, zones) {
  if (!grepl('\\|', zones)) {
    zones = paste0(zones, '_window')
  }
  group = SelectObjects(group, zones)
  if (length(group) == 1) {
    orders = c('a', 'b')
    tags = paste0(names(group), orders)
    group = lapply(orders, RnmSplWindow, group[[1]])
    names(group) = tags
  }
  return(group)
}
# edit building surfaces
EditBuildSurfs = function(group, zones, lvls) {
  group = SelectObjects(group, zones)
  if (length(lvls) == 1) {
    group = lapply(group, RmBounds, zones)
  } else {
    walls = grepl('wall', names(group))
    group[walls] = lapply(group[walls], RmBounds, zones)
    hsurfs = grepl('floor|roof', names(group))
    group[hsurfs] = lapply(group[hsurfs], EditHBounds, lvls)
  }
  return(group)
}
# edit fenestrations
EditFens = function(group, zones) {
  group = SelectObjects(group, zones)
  group = lapply(group, RmFens, zones)
  group[sapply(group, is.null)] = NULL
  if (length(group) == 1) {
    tag = names(group)
    group = lapply(1:2, SplitWindow, group[[1]], tag)
    names(group) = paste0(tag, c('a', 'b'))
  }
  return(group)
}
# edit horizontal boundaries
EditHBounds = function(object, lvls) {
  floor = paste0('(?<=f)', min(lvls))
  roof = paste0('(?<=f)', max(lvls))
  if (str_detect(object$zone_name, floor) & object$surface_type == 'Floor') {
    object$construction_name = 'slab'
    object$outside_boundary_condition = 'OtherSideConditionsModel'
    object$outside_boundary_condition_object = 'ground_coupled_oscm'
  } else if (str_detect(object$zone_name, roof) & object$surface_type == 'Roof') {
    object$construction_name = 'roof'
    object$sun_exposure = 'SunExposed'
    object$wind_exposure = 'WindExposed'
    object$outside_boundary_condition = 'Outdoors'
    object$outside_boundary_condition_object = NULL
  }
  return(object)
}
# edit internal loads
EditIntLoads = function(groups, room) {
  groups = lapply(groups, RmIntLoads, room)
  index = !sapply(groups, is.null)
  groups = groups[index]
  return(groups)
}
# remove unseful zone list
EditZoneList = function(group, zones) {
  group = lapply(group, RmZonesFromList, zones)
  index = sapply(group, function(object) object$idf_max_extensible_fields != 0)
  group = group[index]
  return(group)
}
# is the window south or east?
IsSE = function(side) side %in% c(1, 2)
# label vertices
LabelVert = function(verts, coord) paste0('vertex_', verts, '_', coord, '_coordinate')
# pile habitations
PileHabs = function(tag, nstrs) {
  lvl = tag %>% str_sub(2, 2) %>% as.numeric()
  if (lvl == 1) {
    strs = c(1, 2)
  } else if (lvl < nstrs) {
    strs = c(-1, 1)
  } else {
    strs = c(-2, -1)
  }
  lvls = as.character(lvl + strs) %>%
    sapply(function(x, y) str_replace(y, '(?<=f)[0-9]', x), tag) %>%
    append(tag) %>% str_flatten(collapse = '|')
  return(lvls)
}
# remove boundary condition object
RmBounds = function(object, zones) {
  if (object$outside_boundary_condition == 'Surface') {
    if (!grepl(zones, object$outside_boundary_condition_object)) {
      object$outside_boundary_condition = 'Adiabatic'
      object$sun_exposure = 'NoSun'
      object$wind_exposure = 'NoWind'
      object$outside_boundary_condition_object = NULL
    }
  }
  return(object)
}
# remove fenestrations
RmFens = function(object, zones) {
  if (!is.null(object$outside_boundary_condition_object)) {
    if (grepl(zones, object$outside_boundary_condition_object)) {
      return(object)
    }
  } else {
    return(object)
  }
}
# remove internal loads
RmIntLoads = function(group, room) {
  index = !grepl(room, names(group))
  group[index] = NULL
  return(group)
}
# remove zones from zone list
RmZonesFromList = function(object, zones) {
  index = sapply(object$zones, function(object, zones) grepl(zones, object$zone_name), zones)
  object$zones = object$zones[index]
  object$idf_max_extensible_fields = length(object$zones)
  return(object)
}
# rename splitted window
RnmSplWindow = function(order, object) {
  object$surface_name = paste0(object$surface_name, order)
  return(object)
}
# select objects according to the zone
SelectObjects = function(group, zones) group[grep(zones, names(group))]
# split window in two
SplitWindow = function(order, window, tag) {
  side = tag %>% str_extract('(?<=window)\\D')
  coord = DefCoord(side)
  wv = LabelVert(DefStartEnd(side), coord)
  width = window[[wv[1]]] - window[[wv[2]]]
  if (order == 1) {
    verts = LabelVert(c(3, 4), coord)
    if (IsSE(side)) {
      window[verts] = window[[wv[2]]] + width/2
    } else {
      window[verts] = window[[wv[1]]] - width/2
    }
  } else {
    verts = LabelVert(c(1, 2), coord)
    if (IsSE(side)) {
      window[verts] = window[[wv[1]]] - width/2
    } else {
      window[verts] = window[[wv[2]]] + width/2
    }
  }
  return(window)
}

# main functions ####
# shrink building
ShrinkBuilding = function(seed_path, pattern, output_dir) {
  # load seed model
  model = read_json(seed_path)
  # find zones associated to the habitation
  tags = names(model$'Zone')
  zones = tags[grep(pattern, tags)]
  model$'Zone' = model$'Zone'[zones]
  zones = str_flatten(zones, collapse = '|')
  # building surfaces
  lvls = zones %>% str_extract_all('(?<=f)\\d') %>% pluck(1) %>% as.numeric() %>% unique()
  group = model$'BuildingSurface:Detailed'
  model$'BuildingSurface:Detailed' = EditBuildSurfs(group, zones, lvls)
  # fenestrations
  group = model$'FenestrationSurface:Detailed'
  model$'FenestrationSurface:Detailed' = EditFens(group, zones)
  # zone list
  model$'ZoneList' = EditZoneList(model$'ZoneList', zones)
  # internal loads
  if (!grepl('\\|', zones)) {
    ils = c('ElectricEquipment', 'Lights', 'People')
    room = ifelse(grepl('liv', zones), 'liv', 'dorm')
    model[ils] = EditIntLoads(model[ils], room)
    index = sapply(model, function(group) length(group) == 0)
    model[index] = NULL
  }
  # airflownetwork zone
  group = model$'AirflowNetwork:MultiZone:Zone'
  model$'AirflowNetwork:MultiZone:Zone' = SelectObjects(group, zones)
  # airflow network surfaces
  group = model$'AirflowNetwork:MultiZone:Surface'
  model$'AirflowNetwork:MultiZone:Surface' = EditAFNSurfs(group, zones)
  # ground domain
  if (!(length(lvls) > 1 | grepl('f1', pattern))) {
    items = c('Site:GroundDomain:Slab', 'SurfaceProperty:OtherSideConditionsModel',
              'Site:GroundTemperature:Undisturbed:FiniteDifference')
    model[items] = NULL
  }
  # write epJSON file
  c = str_count(zones, '\\|')
  if (c <= 4) {
    sim = ifelse(c == 0, '3', '2')
  } else {
    sim = '4'
    pattern = pattern %>% str_split('\\|') %>% pluck(1)
    pattern = pattern[3]
  }
  output_name = seed_path %>% basename() %>%
    str_remove('.epJSON') %>% str_replace('^0', sim)
  output_path = paste0(output_dir, output_name, '_', pattern, '.epJSON')
  write_json(model, output_path, pretty = TRUE, auto_unbox = TRUE)
}
# apply ShrinkBuilding()
ApplyShrinkBuild = function(typo, seed_dir, nstrs, output_dir, cores_left) {
  rooms = c('liv', 'dorm1', 'dorm2')
  if (typo == 'l') {
    habs = c('csw', 'msw', 'mse', 'cse', 'cne', 'mne', 'mnw', 'cnw')
  } else if (typo == 'h') {
    habs = c('csw', 'cse', 'cne', 'cnw')
  } else {
    stop('Typology not recognized!')
  }
  habs = paste0('f', rep(c(1:nstrs), each = length(habs)), '_', habs)
  piles = sapply(habs, PileHabs, nstrs)
  patterns = c(habs, paste0(habs, '_', rep(rooms, each = length(habs))), piles)
  seed_paths = dir(seed_dir, paste0('^0_', typo, '.*epJSON$'), full.names = TRUE)
  grid = expand.grid('seed_path' = seed_paths, 'pattern' = patterns,
                     stringsAsFactors = FALSE)
  mcmapply(ShrinkBuilding, grid$seed_path, grid$pattern, output_dir,
           mc.cores = detectCores() - cores_left)
}