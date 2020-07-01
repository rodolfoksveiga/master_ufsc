# load libraries and global environment ####
# load libraries
pkgs = c('jsonlite', 'purrr', 'parallel', 'stringr')
lapply(pkgs, library, character.only = TRUE)
# load complementary data
load('~/git/master_ufsc/seed/snippets.rdata')

# base functions ####
# create a string with storeys' label according to the number of floors
  # bot - ground / mid - intermediary / top - roof storey
LabelStoreys = function(n) {
  # n: number of floors
  if (n == 1) { # if the building has only one storey
    storeys = 'single'
  } else { # if the building has multiple storeys
    storeys = c('bot', rep('mid', n - 2), 'top')
  }
  return(storeys)
}
# add a prefix to a string indicating the storey
  # output: f + index of storey + _ + string
  # example: f1_south_apartment_living / f5_north_apartment_dormitory_1_windows_west
RnmStorey = function(tags, storey, index) {
  if (storey != 'single') {
    tags = paste0('f', index, '_', tags)
  }
  return(tags)
}

# model edition functions ####
# airflow network zones
AddAFNZone = function(tag, fill) AddFields(fill$item, 'zone_name', tag)
# airflow network surfaces
AddAFNSurf = function(object, tag, storey, index, fill) {
  fields = c('surface_name', 'leakage_component_name',
             'idf_max_fields', 'ventilation_control_mode')
  # surface_name = surface name with storey prefix
  values = RnmStorey(tag, storey, index)
  if (object$surface_type == 'Window') { # if fenestration is a window
    # leakage_component_name = window_opening
    values = append(values, 'window_opening')
    if (grepl('bwc', tag)) { # if it's a bwc window
      # vetilation_control_mode = Constant
      values = append(values, list(7, 'Constant'))
    } else { # if it's a living room or dormitory
      fields = append(fields, c('venting_availability_schedule_name',
                                'ventilation_control_zone_temperature_setpoint_schedule_name'))
      sch = ifelse(grepl('liv', tag), 'sch_afn_liv', 'sch_afn_dorm')
      # venting_availability_schedule_name = Temperature
      # ventilation_control_zone_temperature_setpoint_schedule_name = sch_window_behav
      values = append(values, list(9, 'Temperature', 'sch_window_behav', sch))
    }
  } else if (object$surface_type == 'Door') { # if fenestration is a door
    # leakage_component_name = door_opening
    values = append(values, list('door_opening', 7))
    if (grepl('bwc|core', tag)) { # if it's a bwc door
      # vetilation_control_mode = NoVent
      values = append(values, 'NoVent')
    } else if (grepl('dorm', tag) &  # if it's a door between living room and dormitory
               !grepl('bwc|core', object$outside_boundary_condition_object)) {
      # vetilation_control_mode = Constant
      values = append(values, 'Constant')
    } else { # if the door is repeated
      # the airflow network surface is not considered twice, thus the value is removed (NULL)
      values = NULL
    }
  } else {  # if fenestration is not a door or a window
    stop ('Not recognized fenestration type!')
  }
  if (!is.null(values)) { # if it's not a repeated door
    # fill the fields with the values
    surf = AddFields(fill$item, fields, values)
    return(surf)
  }
}
# add fields and its values to an object
AddFields = function(object, fields, values) {
  tags = names(object)
  object = append(object, mapply(function(x, y) x = y, fields, values, SIMPLIFY = FALSE))
  names(object) = c(tags, fields)
  return(object)
}
# add internal loads
AddIntLoad = function(tag, fill, type) {
  fields = 'zone_or_zonelist_name'
  values = tag
  liv = grepl('liv', tag)
  if (type == 'light') {
    fields = append(fields, 'schedule_name')
    sch = ifelse(liv, 'sch_ilum_liv', 'sch_ilum_dorm')
    values = append(values, sch)
  } else if (type == 'people') {
    fields = append(fields, c('activity_level_schedule_name',
                              'number_of_people_schedule_name', 'number_of_people'))
    sch_act = ifelse(liv, 'sch_activ_liv', 'sch_activ_dorm')
    sch_np = sub('activ', 'occup', sch_act)
    np = ifelse(liv, 4, 2)
    values = list(values, sch_act, sch_np, np)
  }
  int_load = AddFields(fill$item, fields, values)
  return(int_load)
}
# create zone lists for living rooms and dormitories
AddZoneList = function(room, tags, fill) {
  tags = tags[grep(room, tags)]
  fields = c('idf_max_extensible_fields')
  values = c(length(tags))
  list = AddFields(fill$item, fields, values)
  list$zones = lapply(tags, function(x, y) append(y, list('zone_name' = x)), list$zones)
  return(list)
}
# apply AddAFNSurf for all fenestrations
ApplyAFNSurfs = function(storey, index, objects, tags) {
  surfs = mapply(AddAFNSurf, objects, tags, storey, index, SIMPLIFY = FALSE,
                 MoreArgs = list(fill$'AirflowNetwork:MultiZone:Surface'))
  surfs[sapply(surfs, is.null)] = NULL
  tags = RnmStorey(names(surfs), storey, index)
  tags = paste0('afn_', tags)
  names(surfs) = tags
  return(surfs)
}
# apply DefBuildSurf for all surfaces
ApplyBounds = function(storey, index, objects, tags, boundary) {
  surfs = mapply(DefBuildSurf, objects, tags, storey, index, boundary, SIMPLIFY = FALSE)
  tags = RnmStorey(names(surfs), storey, index)
  names(surfs) = tags
  return(surfs)
}
# apply DefFen for all fenestrations
ApplyFens = function(storey, index, objects, tags) {
  surfs = mapply(DefFen, objects, storey, index, SIMPLIFY = FALSE)
  tags = RnmStorey(tags, storey, index)
  names(surfs) = tags
  return(surfs)
}
# apply AddIntLoad for all lists of zones
ApplyIntLoads = function(type, fill, tags) {
  if (type == 'equip') {
    tags = tags[grep('liv', tags)]
  } else if (!type %in% c('light', 'people')) {
    stop('Not recognized load source!')
  }
  int_loads = lapply(tags, AddIntLoad, fill, type)
  names(int_loads) = paste0(type, '_', tags)
  return(int_loads)
}
# copy zones
CpZones = function(storey, index, height, objects, tags) {
  zones = lapply(objects, AddFields, 'z_origin', height)
  names(zones) = RnmStorey(tags, storey, index)
  return(zones)
}

# define building surface boundaries for floors and roofs
DefBuildSurf = function(object, tag, storey, index, boundary) {
  type = object$surface_type
  object$zone_name = RnmStorey(object$zone_name, storey, index)
  if (type == 'Floor' | type == 'Roof') {
    fields = c('construction_name')
    if (type == 'Floor') {
      fields = append(fields, 'outside_boundary_condition')
      if (storey %in% c('single', 'bot')) {
        fields = append(fields, 'outside_boundary_condition_object')
        values = c('slab', 'OtherSideConditionsModel', 'ground_coupled_oscm')
      } else if (storey %in% c('mid', 'top')) {
        if (boundary == 'surface') {
          fields = append(fields, 'outside_boundary_condition_object')
          values = c('floor', 'Surface', paste0('f', index - 1, '_', TagBoundSurf(tag)))
        } else if (boundary == 'adiabatic') {
          values = c('floor', 'Adiabatic')
        } else {
          stop('Not recognized boundary condition!')
        }
      } else {
        stop('Not recognized storey!')
      }
    } else {
      fields = append(fields, c('sun_exposure', 'wind_exposure', 'outside_boundary_condition'))
      if (storey %in% c('single', 'top')) {
        values = c('roof', 'SunExposed', 'WindExposed', 'Outdoors')
      } else if (storey %in% c('bot', 'mid')) {
        values = c('ceiling', 'NoSun', 'NoWind')
        if (boundary == 'surface') {
          fields = append(fields, 'outside_boundary_condition_object')
          values = append(values, c('Surface', paste0('f', index + 1, '_', TagBoundSurf(tag))))
        } else if (boundary == 'adiabatic') {
          values = append(values, 'Adiabatic')
        } else {
          stop('Not recognized boundary condition!')
        }
      } else {
        stop('Not recognized storey!')
      }
    }
    surf = AddFields(object, fields, values)
    return(surf)
  } else if (type == 'Wall') {
    bound = object$outside_boundary_condition_object
    if (!is.null(bound)) {
      object$outside_boundary_condition_object = RnmStorey(bound, storey, index)
    }
    return(object)
  } else {
    stop('Not recognized surface type!')
  }
}

# define fenestrations
DefFen = function(object, storey, index) {
  object$building_surface_name = RnmStorey(object$building_surface_name, storey, index)
  if (object$surface_type == 'Door') {
    bound = object$outside_boundary_condition_object
    object$outside_boundary_condition_object = RnmStorey(bound, storey, index)
  }
  return(object)
}

# generate zones and surfaces for all storeys
GenStoreys = function(type, group, n, height, boundary) {
  indexes = 1:n
  storeys = LabelStoreys(n)
  if (type == 'zone') {
    heights = 0:(n -1)*height
    group = mapply(CpZones, storeys, indexes, heights,
                   SIMPLIFY = FALSE, USE.NAMES = FALSE,
                   MoreArgs = list(group, names(group)))
  } else if (type == 'surf') {
    group = mapply(ApplyBounds, storeys, indexes,
                   SIMPLIFY = FALSE, USE.NAMES = FALSE,
                   MoreArgs = list(group, names(group), boundary))
  } else if (type == 'afn') {
    group = mapply(ApplyAFNSurfs, storeys, indexes,
                   SIMPLIFY = FALSE, USE.NAMES = FALSE,
                   MoreArgs = list(group, names(group)))
  } else if (type == 'fen') {
    group = mapply(ApplyFens, storeys, indexes,
                   SIMPLIFY = FALSE, USE.NAMES = FALSE,
                   MoreArgs = list(group, names(group)))
  } else {
    stop('Not recognized group of objects!')
  }
  group = flatten(group)
  return(group)
}

# tag boundary surface for floor and roof
TagBoundSurf = function(tag) {
  opp = ifelse(grepl('floor', tag),
               str_replace(tag, 'floor', 'roof'),
               str_replace(tag, 'roof', 'floor'))
  return(opp)
}

# main function ####
BuildModel = function(geom_path, shell, boundary, n_strs, output_dir) {
  # load seed with phisical objects
  seed = read_json(geom_path)
  # zones
  # determine storey height
  index = grep('roof', names(seed$'BuildingSurface:Detailed'))[1]
  height = seed$'BuildingSurface:Detailed'[[index]]$vertices[[1]]$vertex_z_coordinate
  # generate zones, surfaces, fenestrations and airflow network surfaces
  groups = c('Zone', 'BuildingSurface:Detailed', rep('FenestrationSurface:Detailed', 2))
  items = groups
  items[4] = 'AirflowNetwork:MultiZone:Surface'
  types = c('zone', 'surf', 'fen', 'afn')
  model[items] = mapply(GenStoreys, types, seed[groups], n_strs, height, boundary)
  # add airflow network zones
  model$'AirflowNetwork:MultiZone:Zone' = lapply(names(model$'Zone'), AddAFNZone,
                                                fill$'AirflowNetwork:MultiZone:Zone')
  names(model$'AirflowNetwork:MultiZone:Zone') = paste0('afn_', names(model$'Zone'))
  # define construction shell
  model$'Construction' = append(model$'Construction', construction[[shell]])
  # add zone lists for occupied rooms
  occup_rooms = c('liv', 'dorm')
  model$'ZoneList' = lapply(occup_rooms, AddZoneList, names(model$'Zone'), fill$'ZoneList')
  names(model$'ZoneList') = paste0(occup_rooms, 's')
  # add internal loads from equipments, lights and people
  groups = c('ElectricEquipment', 'Lights', 'People')
  types = c('equip', 'light', 'people')
  model[groups] = mapply(ApplyIntLoads, types, fill[groups],
                        MoreArgs = list(names(model$'ZoneList')))
  # write json file 
  geometry = str_remove(basename(geom_path), '.json')
  index = ifelse(boundary == 'surface', '00', '01')
  output_path = paste0(output_dir, index, '_', geometry, '_', shell, '.epJSON')
  write_json(model, output_path, pretty = T, auto_unbox = T)
  print(output_path)
}

# application ####
# grid = expand.grid('shell' = c('ref17', 'ref8', 'tm', 'tv', 'sf'),
#                    'boundary' = c('surface', 'adiabatic'),
#                    stringsAsFactors = FALSE)
# geom_path = '/home/rodox/git/master_ufsc/seed/linear.json'
# n_strs = 5
# output_dir = '/home/rodox/git/master_ufsc/model/'
# mcmapply(BuildModel, geom_path, grid$shell, grid$boundary,
#          5, output_dir, mc.cores = detectCores())
