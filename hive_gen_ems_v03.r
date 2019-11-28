# load libraries
library(rjson)

# auxiliar functions ####
# is_csn()
is_csn = function (side) {
  is_csn = side == 'c' | side == 's' | side == 'n'
  return(is_csn)
}

# is_cew()
is_cew = function(side) {
  is_cew = side == 'c' | side == 'e' | side == 'w'
  return(is_cew)
}

# build_surf()
build_surf = function(side = 'c', surf, lx, ly, lz) {
  if (surf == 's' | surf == 'e' | surf == 'n' | surf == 'w') {
    vertex_1_x = ifelse(surf == 's', 0, ifelse(surf == 'e', ifelse(is_csn(side), lx, ly),
                                               ifelse(surf == 'n', ifelse(is_csn(side), lx, ly),
                                                      0)))
    vertex_1_y = ifelse(surf == 's', 0,
                        ifelse(surf == 'e', 0, ifelse(surf == 'n', ifelse(is_cew(side), ly, lx),
                                                      ifelse(is_cew(side), ly, lx))))
    vertex_1_z = lz
    vertex_2_x = ifelse(surf == 's', 0, ifelse(surf == 'e', ifelse(is_csn(side), lx, ly),
                                               ifelse(surf == 'n', ifelse(is_csn(side), lx, ly),
                                                      0)))
    vertex_2_y = ifelse(surf == 's', 0,
                        ifelse(surf == 'e', 0, ifelse(surf == 'n', ifelse(is_cew(side), ly, lx),
                                                      ifelse(is_cew(side), ly, lx))))
    vertex_2_z = 0
    vertex_3_x = ifelse(surf == 's', ifelse(is_csn(side), lx, ly),
                        ifelse(surf == 'e', ifelse(is_csn(side), lx, ly), ifelse(surf == 'n', 0,
                                                                                 0)))
    vertex_3_y = ifelse(surf == 's', 0, ifelse(surf == 'e', ifelse(is_cew(side), ly, lx),
                                               ifelse(surf == 'n', ifelse(is_cew(side), ly, lx),
                                                      0)))
    vertex_3_z = 0
    vertex_4_x = ifelse(surf == 's', ifelse(is_csn(side), lx, ly),
                        ifelse(surf == 'e', ifelse(is_csn(side), lx, ly), ifelse(surf == 'n', 0,
                                                                                 0)))
    vertex_4_y = ifelse(surf == 's', 0, ifelse(surf == 'e', ifelse(is_cew(side), ly, lx),
                                               ifelse(surf == 'n', ifelse(is_cew(side), ly, lx),
                                                      0)))
    vertex_4_z = lz
  }
  else {
    vertex_1_x = ifelse(surf == 'floor', ifelse(is_csn(side), lx, ly), 0)
    vertex_1_y = ifelse(is_cew(side), ly, lx)
    vertex_1_z = ifelse(surf == 'floor', 0, lz)
    vertex_2_x = ifelse(surf == 'floor', ifelse(is_csn(side), lx, ly), 0)
    vertex_2_y = 0
    vertex_2_z = ifelse(surf == 'floor', 0, lz)
    vertex_3_x = ifelse(surf == 'floor', 0, ifelse(is_csn(side), lx, ly))
    vertex_3_y = 0
    vertex_3_z = ifelse(surf == 'floor', 0, lz)
    vertex_4_x = ifelse(surf == 'floor', 0, ifelse(is_csn(side), lx, ly))
    vertex_4_y = ifelse(is_cew(side), ly, lx)
    vertex_4_z = ifelse(surf == 'floor', 0, lz)
  }
  return(list(
    list('vertex_x_coordinate' = vertex_1_x, 'vertex_y_coordinate' =
           vertex_1_y, 'vertex_z_coordinate' = vertex_1_z),
    list('vertex_x_coordinate' = vertex_2_x, 'vertex_y_coordinate' =
           vertex_2_y, 'vertex_z_coordinate' = vertex_2_z),
    list('vertex_x_coordinate' = vertex_3_x, 'vertex_y_coordinate' =
           vertex_3_y, 'vertex_z_coordinate' = vertex_3_z),
    list('vertex_x_coordinate' = vertex_4_x, 'vertex_y_coordinate' =
           vertex_4_y, 'vertex_z_coordinate' = vertex_4_z)
  ))
}

# fen_surf()
fen_surf = function(fen_type, side, lx, ly, lz, wfr) {
  if (fen_type == 'window') {
    wall_ratio = ifelse(is_csn(side), lz/lx, lz/ly)
    window_width = sqrt(lx*ly*wfr/wall_ratio)
    window_height = sqrt(lx*ly*wfr*wall_ratio)
    vertex_1_x = ifelse(side == 's', lx/2 - window_width/2,
                        ifelse(side == 'e', lx, ifelse(side == 'n', lx/2 + window_width/2, 0)))
    vertex_1_y = ifelse(side == 's', 0, ifelse(side == 'e', ly/2 - window_width/2,
                                               ifelse(side == 'n', ly, ly/2 + window_width/2)))
    vertex_1_z = lz/2 + window_height/2
    vertex_2_x = ifelse(side == 's', lx/2 - window_width/2,
                        ifelse(side == 'e', lx, ifelse(side == 'n', lx/2 + window_width/2, 0)))
    vertex_2_y = ifelse(side == 's', 0, ifelse(side == 'e', ly/2 - window_width/2,
                                               ifelse(side == 'n', ly, ly/2 + window_width/2)))
    vertex_2_z = lz/2 - window_height/2
    vertex_3_x = ifelse(side == 's', lx/2 + window_width/2,
                        ifelse(side == 'e', lx, ifelse(side == 'n', lx/2 - window_width/2, 0)))
    vertex_3_y = ifelse(side == 's', 0, ifelse(side == 'e', ly/2 + window_width/2,
                                               ifelse(side == 'n', ly, ly/2 - window_width/2)))
    vertex_3_z = lz/2 - window_height/2
    vertex_4_x = ifelse(side == 's', lx/2 + window_width/2,
                        ifelse(side == 'e', lx, ifelse(side == 'n', lx/2 - window_width/2, 0)))
    vertex_4_y = ifelse(side == 's', 0, ifelse(side == 'e', ly/2 + window_width/2,
                                               ifelse(side == 'n', ly, ly/2 - window_width/2)))
    vertex_4_z = lz/2 + window_height/2
  } else {
    vertex_1_x = ifelse(side == 's', lx/2 - 0.4,
                        ifelse(side == 'e', lx, ifelse(side == 'n', lx/2 + 0.4, 0)))
    vertex_1_y = ifelse(side == 's', 0,
                        ifelse(side == 'e', ly/2 - 0.4, ifelse(side == 'n', ly, ly/2 + 0.4)))
    vertex_1_z = 2.1
    vertex_2_x = ifelse(side == 's', lx/2 - 0.4,
                        ifelse(side == 'e', lx, ifelse(side == 'n', lx/2 + 0.4, 0)))
    vertex_2_y = ifelse(side == 's', 0,
                        ifelse(side == 'e', ly/2 - 0.4, ifelse(side == 'n', ly, ly/2 + 0.4)))
    vertex_2_z = 0
    vertex_3_x = ifelse(side == 's', lx/2 + 0.4,
                        ifelse(side == 'e', lx, ifelse(side == 'n', lx/2 - 0.4, 0)))
    vertex_3_y = ifelse(side == 's', 0,
                        ifelse(side == 'e', ly/2 + 0.4, ifelse(side == 'n', ly, ly/2 - 0.4)))
    vertex_3_z = 0
    vertex_4_x = ifelse(side == 's', lx/2 + 0.4,
                        ifelse(side == 'e', lx, ifelse(side == 'n', lx/2 - 0.4, 0)))
    vertex_4_y = ifelse(side == 's', 0,
                        ifelse(side == 'e', ly/2 + 0.4, ifelse(side == 'n', ly, ly/2 - 0.4)))
    vertex_4_z = 2.1
  }
  return(c(vertex_1_x, vertex_1_y, vertex_1_z, vertex_2_x, vertex_2_y, vertex_2_z,
           vertex_3_x, vertex_3_y, vertex_3_z, vertex_4_x, vertex_4_y, vertex_4_z))
}

# is_room()
is_room = function(type) {
  is_room = type == 'living' |  type == 'dorm'
  return(is_room)
}

# opos_side()
opos_side = function(side) {
  opos_side = ifelse(side == 's', 'n', ifelse(side == 'e', 'w', ifelse(side == 'n', 's', 'e')))
  return(opos_side)
}

# living_adj()
living_adj = function(zones) {
  living_adj = zones$name[which(zones$type == 'living')[1]]
  return(living_adj)
}

# num()
num = function(value) {
  num = as.numeric(value)
  return(num)
}

# zone_adj()
zone_adj = function(side, lx, ly) {
  x_origin = ifelse(side == 's' | side == 'n', 0, ifelse(side == 'e', lx, -ly))
  y_origin = ifelse(side == 's', -lx, ifelse(side == 'e' | side == 'w', 0, ly))
  return(c(x_origin, y_origin))
}

# hive_gen()
hive_gen = function(seed, lx, ly, lz, alt, room, bounds, output_dir, model_name) {
  # seed - epJSON's file full path filled with constant values
  # lx - zone's width
  # ly - zone's depth
  # lz - zone's heigth
  # alt - altitude of the zone
  # room - type of room's occupation
    # possible values: 'living' and 'dorm'
  # bounds - a list containing orientation, boundary condition and wfr (window to floor ratio)
    # possible for boundary conditions: 'outdoors', 'adiabatic', 'dorm' and 'living'
    # possible values for wfr: from 0 to the value correspondent to ('wall area' / 'floor area')
    # wfr equation: equation: wfr = (sum('glass area') / 'floor_area')
    # obs.: the columns of the data frame must have the all the orientations labelled as follow:
      # 's', 'e', 'n' and 'w'
    # e.g.: bounds = list(c('s', 'outdoors', 0.1),
      #                   c('e', 'outdoors', 0.05),
      #                   c('n', 'dorm', 0),
      #                   c('w', 'living', 0))
  # output_dir - directory where the model is saved
  # model_name - name of the file (model) to be saved

  # some pre-process ####
  seed = fromJSON(file = seed)
  bounds = lapply(bounds, function(x) append(x, paste0('hive_', x[1])))
  zones = data.frame('name' = c('core', 'hive_s', 'hive_e', 'hive_n', 'hive_w'),
                     'type' = c(room, bounds[[1]][2], bounds[[2]][2],
                                bounds[[3]][2], bounds[[4]][2]),
                     stringsAsFactors = F)
  
  # ems program code ####
  ems_codes = list(
    'living' = list(
      list('program_line' = paste0('SET uncomf = ((sensor_op_temp_zone >= 26) ||',
                                   ' (sensor_op_temp_zone <= 16))')),
      list('program_line' = 'IF ((sensor_occup_zone > 0) && (uncomf == 1))'),
      list('program_line' = 'Set act_hvac_zone = 1'),
      list('program_line' = 'Set act_afn_zone = 0'),
      list('program_line' = 'ELSEIF ((sensor_occup_zone > 0) && (sensor_hvac_zone > 0))'),
      list('program_line' = 'Set act_hvac_zone = 1'),
      list('program_line' = 'Set act_afn_zone = 0'),
      list('program_line' = 'ELSEIF (sensor_occup_zone > 0)'),
      list('program_line' = paste0('IF ((sensor_out_temp < sensor_mean_temp_zone) &&',
                                   ' (sensor_out_temp > 19))')),
      list('program_line' = 'Set act_hvac_zone = 0'),
      list('program_line' = 'Set act_afn_zone = 1'),
      list('program_line' = paste0('ELSEIF ((sensor_out_temp > sensor_mean_temp_zone) &&',
                                   ' (sensor_out_temp > 19))')),
      list('program_line' = 'Set act_hvac_zone = 0'),
      list('program_line' = 'Set act_afn_zone = 0'),
      list('program_line' = 'ELSEIF (sensor_out_temp < 19)'),
      list('program_line' = 'Set act_hvac_zone = 0'),
      list('program_line' = 'Set act_afn_zone = 0'),
      list('program_line' = 'ENDIF'),
      list('program_line' = 'ELSEIF (sensor_occup_zone == 0)'),
      list('program_line' = 'Set act_hvac_zone = 0'),
      list('program_line' = 'Set act_afn_zone = 0'),
      list('program_line' = 'ENDIF')
    ),
    'dorm' = list(
      list('program_line' = paste0('SET uncomf = ((sensor_op_temp_zone >= 26) ||',
                                   ' (sensor_op_temp_zone <= 16))')),
      list('program_line' = 'IF (sensor_occup_zone > 0) && (uncomf == 1)'),
      list('program_line' = 'Set act_hvac_zone = 1'),
      list('program_line' = 'Set act_afn_zone = 0'),
      list('program_line' = 'ELSEIF ((sensor_occup_zone > 0) && (sensor_hvac_zone > 0))'),
      list('program_line' = 'Set act_hvac_zone = 1'),
      list('program_line' = 'Set act_afn_zone = 0'),
      list('program_line' = 'ELSEIF ((sensor_occup_zone > 0) || (sensor_occup_living_adj > 0))'),
      list('program_line' = 'Set act_hvac_zone = 0'),
      list('program_line' = paste0('IF ((sensor_out_temp < sensor_mean_temp_zone) &&',
                                   ' (sensor_out_temp > 19))')),
      list('program_line' = 'Set act_afn_zone = 1'),
      list('program_line' = paste0('ELSEIF ((sensor_out_temp > sensor_mean_temp_zone) &&',
                                   ' (sensor_out_temp > 19))')),
      list('program_line' = 'Set act_afn_zone = 0'),
      list('program_line' = 'ELSEIF (sensor_out_temp < 19)'),
      list('program_line' = 'Set act_afn_zone = 0'),
      list('program_line' = 'ENDIF'),
      list('program_line' = 'ELSEIF (sensor_occup_zone == 0) && (sensor_occup_living_adj == 0)'),
      list('program_line' = 'Set act_hvac_zone = 0'),
      list('program_line' = 'Set act_afn_zone = 0'),
      list('program_line' = 'ENDIF')
    )
  )
  
  # airflow network simulation control (to be fixed) ####
  # ems program and ems calling manager ####
  names(seed[['EnergyManagementSystem:ProgramCallingManager']][['ems_call']][['programs']]) =
    zones$name
  calls = seed[['EnergyManagementSystem:ProgramCallingManager']][['ems_call']][['programs']]
  for (i in 1:dim(zones)[1]) {
    if (zones$type[i] == 'living') {
      seed[['EnergyManagementSystem:ProgramCallingManager']][['ems_call']][['programs']][[zones$name[i]]] =
        calls[[zones$name[i]]]
      seed[['EnergyManagementSystem:Program']][[paste0('ems_program_', zones$name[i])]][['lines']] =
        lapply(ems_codes$living,
               function(x) list('program_line' = gsub('zone', zones$name[i], x)))
    } else if (zones$type[i] == 'dorm') {
      seed[['EnergyManagementSystem:ProgramCallingManager']][['ems_call']][['programs']][[zones$name[i]]] =
        calls[[zones$name[i]]]
      seed[['EnergyManagementSystem:Program']][[paste0('ems_program_', zones$name[i])]][['lines']] =
        lapply(ems_codes$dorm,
               function(x) list('program_line' = gsub('zone', zones$name[i],
                                                      gsub('living_adj', living_adj(zones), x))))
    } else {
      seed[['EnergyManagementSystem:ProgramCallingManager']][['ems_call']][['programs']][[zones$name[i]]] =
        NULL
      seed[['EnergyManagementSystem:Program']][[paste0('ems_program_', zones$name[i])]] = NULL
    }
  }
  calls = vector('list',
                 length(seed[['EnergyManagementSystem:ProgramCallingManager']][['ems_call']][['programs']]))
  for (i in 1:length(calls)) {
    calls[[i]] =
      seed[['EnergyManagementSystem:ProgramCallingManager']][['ems_call']][['programs']][[i]]
  }
  seed[['EnergyManagementSystem:ProgramCallingManager']][['ems_call']][['programs']] = calls
  rm(calls, i)
  
  # core properties ####
  for (zone in c('core', 'hive_s', 'hive_e', 'hive_n', 'hive_w')) {
    seed[['Zone']][[zone]][['z_origin']] = alt
  }
  # building surface (geometry)
  seed[['BuildingSurface:Detailed']][['core_floor']][['vertices']] = build_surf('c', 'floor', lx,
                                                                                ly, lz)
  seed[['BuildingSurface:Detailed']][['core_roof']][['vertices']] = build_surf('c', 'roof', lx,
                                                                               ly, lz)
  seed[['BuildingSurface:Detailed']][['core_wall_s']][['vertices']] = build_surf('c', 's', lx,
                                                                                 ly, lz)
  seed[['BuildingSurface:Detailed']][['core_wall_e']][['vertices']] = build_surf('c', 'e', lx,
                                                                                 ly, lz)
  seed[['BuildingSurface:Detailed']][['core_wall_n']][['vertices']] = build_surf('c', 'n', lx,
                                                                                 ly, lz)
  seed[['BuildingSurface:Detailed']][['core_wall_w']][['vertices']] = build_surf('c', 'w', lx,
                                                                                 ly, lz)
  
  # internal loads (people, lights, electric equipment)
  if (room == 'living') {
    seed[['People']][['people_core']][['number_of_people']] = 4
    seed[['People']][['people_core']][['activity_level_schedule_name']] = 'sch_activ_living'
    seed[['People']][['people_core']][['number_of_people_schedule_name']] = 'sch_occup_living'
    seed[['Lights']][['lights_core']][['schedule_name']] = 'sch_ilum_living'
    seed[['ElectricEquipment']][['equip_core']][['schedule_name']] = 'sch_equip_living'
  } else {
    seed[['People']][['people_core']][['number_of_people']] = 2
    seed[['People']][['people_core']][['activity_level_schedule_name']] = 'sch_activ_dorm'
    seed[['People']][['people_core']][['number_of_people_schedule_name']] = 'sch_occup_dorm'
    seed[['Lights']][['lights_core']][['schedule_name']] = 'sch_ilum_dorm'
    seed[['ElectricEquipment']][['equip_core']] = NULL
  }
  
  # core and hive properties ####
  # define number of surface vertices
  vertices = c('vertex_1_x', 'vertex_1_y', 'vertex_1_z', 'vertex_2_x', 'vertex_2_y', 'vertex_2_z',
               'vertex_3_x', 'vertex_3_y', 'vertex_3_z', 'vertex_4_x', 'vertex_4_y', 'vertex_4_z')
  for (side in bounds) {
    # core
      # building surface construction name
    seed[['BuildingSurface:Detailed']][[paste0('core_wall_', side[1])]][['construction_name']] =
      ifelse(side[2] == 'outdoors', 'ext_wall', 'int_wall')
      # building surface boundary condition
    seed[['BuildingSurface:Detailed']][[paste0('core_wall_', side[1])]][['outside_boundary_condition']] =
      ifelse(side[2] == 'outdoors', 'Outdoors', ifelse(side[2] == 'adiabatic', 'Adiabatic',
                                                       'Surface'))
    # building surface sun exposure
    seed[['BuildingSurface:Detailed']][[paste0('core_wall_', side[1])]][['sun_exposure']] =
      ifelse(side[2] == 'outdoors', 'SunExposed', 'NoSun')
    # building surface wind exposure
    seed[['BuildingSurface:Detailed']][[paste0('core_wall_', side[1])]][['wind_exposure']] =
      ifelse(side[2] == 'outdoors', 'WindExposed', 'NoWind')
    if (is_room(side[2])) {
      # building surface boundary condition
      seed[['BuildingSurface:Detailed']][[paste0('core_wall_', side[1])]][['outside_boundary_condition_object']] =
        paste0('hive_', side[1], '_wall_', opos_side(side[1]))
      # building surface geometry
      seed[['BuildingSurface:Detailed']][[paste0('hive_', side[1], '_floor')]][['vertices']] =
        build_surf(side[1], 'floor', lx, ly, lz)
      seed[['BuildingSurface:Detailed']][[paste0('hive_', side[1], '_roof')]][['vertices']] =
        build_surf(side[1], 'roof', lx, ly, lz)
      seed[['BuildingSurface:Detailed']][[paste0('hive_', side[1], '_wall_s')]][['vertices']] =
        build_surf(side[1], 's', lx, ly, lz)
      seed[['BuildingSurface:Detailed']][[paste0('hive_', side[1], '_wall_e')]][['vertices']] =
        build_surf(side[1], 'e', lx, ly, lz)
      seed[['BuildingSurface:Detailed']][[paste0('hive_', side[1], '_wall_n')]][['vertices']] =
        build_surf(side[1], 'n', lx, ly, lz)
      seed[['BuildingSurface:Detailed']][[paste0('hive_', side[1], '_wall_w')]][['vertices']] =
        build_surf(side[1], 'w', lx, ly, lz)
      # zone
      seed[['Zone']][[paste0('hive_', side[1])]][['x_origin']] = zone_adj(side[1], lx, ly)[1]
      seed[['Zone']][[paste0('hive_', side[1])]][['y_origin']] = zone_adj(side[1], lx, ly)[2]
      seed[['FenestrationSurface:Detailed']][[paste0('core_window_', side[1])]] =
        seed[['AirflowNetwork:MultiZone:Surface']][[paste0('afn_core_window_', side[1])]] = NULL
      for (i in 1:length(vertices)) {
        seed[['FenestrationSurface:Detailed']][[paste0('core_door_', side[1])]][[paste0(vertices[i], '_coordinate')]] =
          fen_surf('door', side[1], lx, ly)[i]
        seed[['FenestrationSurface:Detailed']][[paste0('hive_', side[1], '_window_', side[1])]][[paste0(vertices[i], '_coordinate')]] =
          fen_surf('window', side[1], ifelse(side[1] == 's' | side[1] == 'n', lx, ly),
                   ifelse(side[1] == 's' | side[1] == 'n', lx, ly), lz, 0.17)[i]
        seed[['FenestrationSurface:Detailed']][[paste0('hive_', side[1], '_door_', opos_side(side[1]))]][[paste0(vertices[i], '_coordinate')]] =
          fen_surf('door', opos_side(side[1]), ifelse(side[1] == 's' | side[1] == 'n', lx, ly),
                   ifelse(side[1] == 's' | side[1] == 'n', lx, ly))[i]
      }
      if (side[2] == 'living') {
        seed[['People']][[paste0('people_hive_', side[1])]][['number_of_people']] = 4
        seed[['People']][[paste0('people_hive_', side[1])]][['activity_level_schedule_name']] =
          'sch_activ_living'
        seed[['People']][[paste0('people_hive_', side[1])]][['number_of_people_schedule_name']] =
          'sch_occup_living'
        seed[['Lights']][[paste0('lights_hive_', side[1])]][['schedule_name']] = 'sch_ilum_living'
        seed[['ElectricEquipment']][[paste0('equip_hive_', side[1])]][['schedule_name']] =
          'sch_equip_living'
      } else if (side[2] == 'dorm') {
        seed[['People']][[paste0('people_hive_', side[1])]][['number_of_people']] = 2
        seed[['People']][[paste0('people_hive_', side[1])]][['activity_level_schedule_name']] =
          'sch_activ_dorm'
        seed[['People']][[paste0('people_hive_', side[1])]][['number_of_people_schedule_name']] =
          'sch_occup_dorm'
        seed[['Lights']][[paste0('lights_hive_', side[1])]][['schedule_name']] = 'sch_ilum_dorm'
        seed[['ElectricEquipment']][[paste0('equip_hive_', side[1])]] = NULL
      }
    } else {
      seed[['Zone']][[paste0('hive_', side[1])]] =
        seed[['ZoneControl:Thermostat']][[paste0('thermostat_hive_', side[1])]] =
        seed[['ZoneHVAC:EquipmentConnections']][[paste0('hvac_equip_connect_hive_', side[1])]] =
        seed[['ZoneHVAC:EquipmentList']][[paste0('hvac_equip_hive_', side[1])]] =
        seed[['ZoneHVAC:IdealLoadsAirSystem']][[paste0('hvac_hive_', side[1])]] =
        seed[['FenestrationSurface:Detailed']][[paste0('core_door_', side[1])]] =
        seed[['FenestrationSurface:Detailed']][[paste0('hive_', side[1], '_window_', side[1])]] =
        seed[['FenestrationSurface:Detailed']][[paste0('hive_', side[1], '_door_', opos_side(side[1]))]] =
        seed[['AirflowNetwork:MultiZone:Surface']][[paste0('afn_core_door_', side[1])]] =
        seed[['AirflowNetwork:MultiZone:Surface']][[paste0('afn_hive_', side[1], '_window_', side[1])]] = 
        seed[['AirflowNetwork:MultiZone:Surface']][[paste0('afn_hive_', side[1], '_door_', opos_side(side[1]))]] =
        seed[['Output:Variable']][[paste0('output_hive_', side[1], '_occup_count')]] =
        seed[['EnergyManagementSystem:Sensor']][[paste0('sensor_occup_hive_', side[1])]] =
        seed[['Output:Variable']][[paste0('output_hive_', side[1], '_sch_afn')]] =
        seed[['EnergyManagementSystem:Actuator']][[paste0('act_afn_hive_', side[1])]] =
        seed[['Output:Variable']][[paste0('output_hive_', side[1], '_sch_hvac')]] =
        seed[['EnergyManagementSystem:Sensor']][[paste0('sensor_hvac_hive_', side[1])]] =
        seed[['EnergyManagementSystem:Actuator']][[paste0('act_hvac_hive_', side[1])]] =
        seed[['EnergyManagementSystem:Sensor']][[paste0('sensor_mean_temp_hive_', side[1])]] =
        seed[['EnergyManagementSystem:Sensor']][[paste0('sensor_op_temp_hive_', side[1])]] =
        seed[['Schedule:Constant']][[paste0('sch_hvac_hive_', side[1])]] =
        seed[['Schedule:Constant']][[paste0('sch_afn_hive_', side[1])]] =
        seed[['People']][[paste0('people_hive_', side[1])]] =
        seed[['Lights']][[paste0('lights_hive_', side[1])]] =
        seed[['ElectricEquipment']][[paste0('equip_hive_', side[1])]] =
        seed[['BuildingSurface:Detailed']][[paste0('hive_', side[1], '_floor')]] =
        seed[['BuildingSurface:Detailed']][[paste0('hive_', side[1], '_roof')]] =
        seed[['BuildingSurface:Detailed']][[paste0('hive_', side[1], '_wall_s')]] =
        seed[['BuildingSurface:Detailed']][[paste0('hive_', side[1], '_wall_e')]] =
        seed[['BuildingSurface:Detailed']][[paste0('hive_', side[1], '_wall_n')]] =
        seed[['BuildingSurface:Detailed']][[paste0('hive_', side[1], '_wall_w')]] = 
        seed[['BuildingSurface:Detailed']][[paste0('core_wall_', side[1])]][['outside_boundary_condition_object']] =
        seed[['AirflowNetwork:MultiZone:Zone']][[paste0('afn_hive_', side[1])]] =
        seed[['AirflowNetwork:MultiZone:Zone']][[paste0('afn_hive_', side[1])]] = NULL
      if (side[2] == 'outdoors' & side[3] > 0) {
        for (i in 1:length(vertices)) {
          seed[['FenestrationSurface:Detailed']][[paste0('core_window_', side[1])]][[paste0(vertices[i], '_coordinate')]] =
            fen_surf('window', side[1], lx, ly, lz, num(side[3]))[i]
        }
      } else {
        seed[['FenestrationSurface:Detailed']][[paste0('core_window_', side[1])]] =
          seed[['AirflowNetwork:MultiZone:Surface']][[paste0('afn_core_window_', side[1])]] = NULL
      }
    }
  }
  rm(i, vertices, side)
  
  # NEWS
  for (side in bounds) {
    if (is_room(side[2])) {
      for (s in c('s', 'e', 'n', 'w')) {
        out_bound_cond =
          seed[['BuildingSurface:Detailed']][[paste0('core_wall_', s)]]['outside_boundary_condition']
        if (side[1] != s & opos_side(side[1]) != s) {
          seed[['BuildingSurface:Detailed']][[paste0('hive_', side[1], '_wall_', s)]][['construction_name']] =
            ifelse(out_bound_cond == 'Outdoors', 'ext_wall', 'int_wall')
          seed[['BuildingSurface:Detailed']][[paste0('hive_', side[1], '_wall_', s)]][['outside_boundary_condition']] =
            ifelse(out_bound_cond == 'Outdoors', 'Outdoors', 'Adiabatic')
          seed[['BuildingSurface:Detailed']][[paste0('hive_', side[1], '_wall_', s)]][['sun_exposure']] =
            ifelse(out_bound_cond == 'Outdoors', 'SunExposed', 'NoSun')
          seed[['BuildingSurface:Detailed']][[paste0('hive_', side[1], '_wall_', s)]][['wind_exposure']] =
            ifelse(out_bound_cond == 'Outdoors', 'WindExposed', 'NoWind')
        }
      }
    }
  }

  
  # save the 'epJSON' file ####
  jsonlite::write_json(seed, paste0(output_dir, model_name, '.epJSON'), pretty = T, auto_unbox = T)
  # print file name
  print(paste0(model_name, '.epJSON'))
}

# application ####
prop = list(
  'sw_dorm_w' = list('dorm' = c(4, 3, 2.7, 0),
                     list(c('s', 'outdoors', 0.2), c('e', 'living', 0), c('n', 'adiabatic', 0),
                          c('w', 'dorm', 0))),
  'sw_living' = list('living' = c(5, 4, 2.7, 0),
                     list(c('s', 'outdoors', 0.2), c('e', 'dorm', 0), c('n', 'adiabatic', 0),
                          c('w', 'dorm', 0))),
  'sw_dorm_e' = list('dorm' = c(3, 4, 2.7, 0),
                     list(c('s', 'outdoors', 0.2), c('e', 'dorm', 0), c('n', 'adiabatic', 0),
                          c('w', 'living', 0))),
  'se_dorm_w' = list('dorm' = c(3, 4, 2.7, 0),
                     list(c('s', 'outdoors', 0.2), c('e', 'living', 0), c('n', 'adiabatic', 0),
                          c('w', 'dorm', 0))),
  'se_living' = list('living' = c(5, 4, 2.7, 0),
                     list(c('s', 'outdoors', 0.2), c('e', 'dorm', 0), c('n', 'adiabatic', 0),
                          c('w', 'dorm', 0))),
  'se_dorm_e' = list('dorm' = c(4, 3, 2.7, 0),
                     list(c('s', 'outdoors', 0.2), c('e', 'dorm', 0), c('n', 'adiabatic', 0),
                          c('w', 'living', 0))),
  'e_dorm_s' = list('dorm' = c(3, 3, 2.7, 0),
                    list(c('s', 'outdoors', 0.2), c('e', 'outdoors', 0), c('n', 'living', 0),
                         c('w', 'dorm', 0))),
  'e_living' = list('living' = c(3, 5, 2.7, 0),
                    list(c('s', 'dorm', 0), c('e', 'outdoors', 0.2), c('n', 'dorm', 0),
                         c('w', 'adiabatic', 0))),
  'e_dorm_n' = list('dorm' = c(3, 3, 2.7, 0),
                    list(c('s', 'living', 0), c('e', 'outdoors', 0), c('n', 'outdoors', 0.2),
                         c('w', 'dorm', 0))),
  'ne_dorm_e' = list('dorm' = c(4, 3, 2.7, 0),
                     list(c('s', 'adiabatic', 0), c('e', 'dorm', 0), c('n', 'outdoors', 0.2),
                          c('w', 'living', 0))),
  'ne_living' = list('living' = c(5, 4, 2.7, 0),
                     list(c('s', 'adiabatic', 0), c('e', 'dorm', 0), c('n', 'outdoors', 0.2),
                          c('w', 'dorm', 0))),
  'ne_dorm_w' = list('dorm' = c(3, 4, 2.7, 0),
                     list(c('s', 'adiabatic', 0), c('e', 'living', 0), c('n', 'outdoors', 0.2),
                          c('w', 'dorm', 0))),
  'nw_dorm_e' = list('dorm' = c(3, 4, 2.7, 0),
                     list(c('s', 'adiabatic', 0), c('e', 'dorm', 0), c('n', 'outdoors', 0.2),
                          c('w', 'living', 0))),
  'nw_living' = list('living' = c(5, 4, 2.7, 0),
                     list(c('s', 'adiabatic', 0), c('e', 'dorm', 0), c('n', 'outdoors', 0.2),
                          c('w', 'dorm', 0))),
  'nw_dorm_w' = list('dorm' = c(4, 3, 2.7, 0),
                     list(c('s', 'adiabatic', 0), c('e', 'living', 0), c('n', 'outdoors', 0.2),
                          c('w', 'dorm', 0))),
  'w_dorm_n' = list('dorm' = c(3, 3, 2.7, 0),
                    list(c('s', 'living', 0), c('e', 'dorm', 0), c('n', 'outdoors', 0.2),
                         c('w', 'outdoors', 0))),
  'w_living' = list('living' = c(3, 5, 2.7, 0),
                    list(c('s', 'dorm', 0), c('e', 'adiabatic', 0), c('n', 'dorm', 0),
                         c('w', 'outdoors', 0.2))),
  'w_dorm_s' = list('dorm' = c(3, 3, 2.7, 0),
                    list(c('s', 'outdoors', 0.2), c('e', 'dorm', 0), c('n', 'living', 0),
                         c('w', 'outdoors', 0)))
)

# application
for (i in 1:length(prop)) {
  hive_gen(seed = paste0('/home/rodox/Dropbox/00.master_ufsc/00.single_zone/00.seed/00.sz/',
                         'seed_3rd_model.epJSON'),
           lx = prop[[i]][[1]][1], ly = prop[[i]][[1]][2], lz = prop[[i]][[1]][3],
           alt = prop[[i]][[1]][4], names(prop[[i]])[1], bounds = prop[[i]][[2]],
           output_dir = paste0('/home/rodox/Dropbox/00.master_ufsc/00.single_zone/01.validation/',
                               '00.sz/02.3rd_model/00.model/'),
           model_name = names(prop)[i])
}

