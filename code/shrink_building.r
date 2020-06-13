# load libraries and global environment ####
# load libraries
pkgs = c('dplyr', 'jsonlite', 'parallel', 'stringr')
lapply(pkgs, library, character.only = TRUE)

# base functions ####
# define coordinates according to orientation
DefCoord = function(side) ifelse(side %in% c('s', 'n'), 'x', 'y')
# define start and end points to calculate the width
DefStartEnd = function(side) c(ifelse(IsSE(side), 3, 1), ifelse(IsSE(side), 1, 3))
# edit airflow network surfaces
EditAFNSurfs = function(group, zones) {
  orders = 1:2
  if (!grepl('\\|', zones)) {
    zones = paste0(zones, '_window')
  }
  group = SelectObjects(group, zones)
  if (length(group) == 1) {
    tags = paste0(names(group), orders)
    group = lapply(orders, RnmSplWindow, group[[1]])
    names(group) = tags
  }
  return(group)
}
# edit building surfaces
EditBuildSurfs = function(group, zones) {
  group = SelectObjects(group, zones)
  group = lapply(group, RmBounds, zones)
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
    names(group) = paste0(tag, 1:2)
  }
  return(group)
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
IsSE = function(side) side %in% c('s', 'e')
# label vertices
LabelVert = function(verts, coord) paste0('vertex_', verts, '_', coord, '_coordinate')
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
# remove groun domain
RmGroundDomain = function(model, pattern) {
  if (!grepl('f1', pattern)) {
    items = c('Site:GroundDomain:Slab', 'SurfaceProperty:OtherSideConditionsModel',
              'Site:GroundTemperature:Undisturbed:FiniteDifference')
    model[items] = NULL
  }
  return(model)
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
  pos = ifelse(grepl('\\D$', tag), -1, -2)
  side = str_sub(tag, pos, pos)
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

# main function ####
ShrinkBuilding = function(seed_path, pattern, output_dir) {
  # load seed model
  model = read_json(seed_path)
  # find zones associated to the dweling
  tags = names(model$'Zone')
  zones = tags[grep(pattern, tags)]
  model$'Zone' = model$'Zone'[zones]
  zones = str_flatten(zones, collapse = '|')
  # building surfaces
  group = model$'BuildingSurface:Detailed'
  model$'BuildingSurface:Detailed' = EditBuildSurfs(group, zones)
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
  model = RmGroundDomain(model, pattern)
  # write epJSON file
  sim = ifelse(grepl('\\|', zones), '02', '03')
  output_name = seed_path %>% basename() %>%
    str_remove('.epJSON') %>% str_replace('^..', sim)
  output_path = paste0(output_dir, output_name, '_', pattern, '.epJSON')
  write_json(model, output_path, pretty = T, auto_unbox = T)
  print(output_path)
}

# application ####
dwels = c('csw', 'msw', 'mse', 'cse', 'cne', 'mne', 'mnw', 'cnw')
n_strs = 5
dwels = paste0('f', rep(c(1:n_strs), each = length(dwels)), '_', dwels)
rooms = c('liv', 'dorm_1', 'dorm_2')
patterns = c(dwels, paste0(dwels, '_', rep(rooms, each = length(dwels))))
shells = c('ref', 'tm', 'tv', 'sf')
seed_dir = '/home/rodox/git/master_ufsc/model/'
seed_paths = paste0(seed_dir, '00_linear_', shells, '.epJSON')
grid = expand.grid('seed_path' = seed_paths, 'pattern' = patterns,
                   stringsAsFactors = FALSE)
output_dir = '/home/rodox/git/master_ufsc/model/'
mcmapply(ShrinkBuilding, grid$seed_path, grid$pattern, output_dir,
         mc.cores = detectCores())
