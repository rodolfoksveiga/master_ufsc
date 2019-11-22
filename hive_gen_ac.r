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

# main function ####  
# hive_gen()
# hive_gen = function(seed, lx, ly, lz, alt, room, bounds, output_dir, model_name) {
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

  # test
  cond = 'ac'
  lx = 3
  ly = 3
  lz = 2.7
  alt = 0
  room = 'living'
  bounds = list(c('s', 'outdoors', 0.2), c('e', 'dorm', 0), c('n', 'living', 0),
                c('w', 'outdoors', 0))
  seed = paste0('/home/rodox/Dropbox/00.master_ufsc/00.single_zone/00.seed/00.sz/',
                'seed_ac_1st_model.epJSON')
  # some pre-process ####
  # load seed file
  seed = fromJSON(file = seed)
  # create variable with main boundary information to do the loops
  bounds = append(list(NULL), bounds)
  names(bounds) = c('hive_c', 'hive_s', 'hive_e', 'hive_n', 'hive_w')
  bounds[['hive_c']] = c('c', room, NA)
  bounds = lapply(bounds, function(x) append(paste0('hive_', x[1]), x))
  hives = bounds
  hives[['hive_c']] = NULL
  surfs = c('s', 'e', 'n', 'w')
  vertices = c('vertex_1_x', 'vertex_1_y', 'vertex_1_z', 'vertex_2_x', 'vertex_2_y', 'vertex_2_z',
               'vertex_3_x', 'vertex_3_y', 'vertex_3_z', 'vertex_4_x', 'vertex_4_y', 'vertex_4_z')
  
  # airflow network simulation control (to be fixed)
  
  for (bound in bounds) {
    
    # zone
    if (is_room(bound[3])) {
      # zone 'x' and 'y' origin vertices
      if (bound[2] != 'c') {
        seed$'Zone'[[bound[1]]]$'x_origin' = zone_adj(bound[2], lx, ly)[1]
        seed$'Zone'[[bound[1]]]$'y_origin' = zone_adj(bound[2], lx, ly)[2]
      }
      # zone 'z' origin vertex (altitude)
      seed$'Zone'[[bound[1]]]$'z_origin' = alt
    } else {
      seed$'Zone'[[bound[1]]] = NULL
    }

    # building surface (geometry)
    if (is_room(bound[3])) {
      seed$'BuildingSurface:Detailed'[[paste0(bound[1], '_floor')]]$'vertices' =
        build_surf(bound[2], 'floor', lx, ly, lz)
      seed$'BuildingSurface:Detailed'[[paste0(bound[1], '_roof')]]$'vertices' =
        build_surf(bound[2], 'roof', lx, ly, lz)
      for (surf in surfs) {
        seed$'BuildingSurface:Detailed'[[paste0(bound[1], '_wall_', surf)]]$'vertices' =
          build_surf(bound[2], surf, lx, ly, lz)
      }
    }
    else {
      seed$'BuildingSurface:Detailed'[[paste0(bound[1], '_floor')]] = NULL
        seed$'BuildingSurface:Detailed'[[paste0(bound[1], '_roof')]] = NULL
      for (surf in surfs) {
        seed$'BuildingSurface:Detailed'[[paste0(bound[1], '_wall_', surf)]] = NULL
      }
    }
    
    # building surface (boundary condition)
      # this is only necessary for core thermal zone ('hive_c')
    if (bound[2] == 'c') {
      for (hive in hives) {
        seed$'BuildingSurface:Detailed'[[paste0('hive_c_wall_', hive[2])]]$
          'outside_boundary_condition' = ifelse(hive[3] == 'outdoors', 'Outdoors',
                                                ifelse(hive[3] == 'adiabatic', 'Adiabatic',
                                                       'Surface'))
        if (is_room(hive[3])) {
          seed$'BuildingSurface:Detailed'[[paste0('hive_c_wall_', hive[2])]]$
            'outside_boundary_condition_object' = paste0(hive[1], '_wall_', opos_side(hive[2]))
        } else {
          seed$'BuildingSurface:Detailed'[[paste0('hive_c_wall_', hive[2])]]$
            'outside_boundary_condition_object' = NULL
        }
        seed$'BuildingSurface:Detailed'[[paste0('hive_c_wall_', hive[2])]]$'construction_name' =
          ifelse(hive[3] == 'outdoors', 'ext_wall', 'int_wall')
        seed$'BuildingSurface:Detailed'[[paste0('hive_c_wall_', hive[2])]]$'sun_exposure' =
          ifelse(hive[3] == 'outdoors', 'SunExposed', 'NoSun')
        seed$'BuildingSurface:Detailed'[[paste0('hive_c_wall_', hive[2])]]$'wind_exposure' =
          ifelse(hive[3] == 'outdoors', 'WindExposed', 'NoWind')
      }
    }
    
    # fenestration geometry
    if (is_room(bound[3])) {
      if (bound[2] == 'c') {
        for (i in 1:length(vertices)) {
          seed$'FenestrationSurface:Detailed'[[paste0('hive_c_door_',
                                                      bound[2])]][[paste0(vertices[i],
                                                                          '_coordinate')]] =
            fen_surf('door', bound[2], lx, ly)[i]
        }
        seed$'FenestrationSurface:Detailed'[[paste0('hive_c_window_', bound[2])]] = NULL
      } else {
        for (i in 1:length(vertices)) {
          seed$
            'FenestrationSurface:Detailed'[[paste0(bound[1], '_door_',
                                                   opos_side(bound[2]))]][[paste0(vertices[i],
                                                                                  '_coordinate')]] =
            fen_surf('door', opos_side(bound[2]), ifelse(bound[2] == 's' | bound[2] == 'n', lx, ly),
                     ifelse(bound[2] == 's' | bound[2] == 'n', lx, ly))[i]
          seed$'FenestrationSurface:Detailed'[[paste0(bound[1], '_window_',
                                                      bound[2])]][[paste0(vertices[i],
                                                                          '_coordinate')]] =
            fen_surf('window', bound[2], ifelse(bound[2] == 's' | bound[2] == 'n', lx, ly),
                     ifelse(bound[2] == 's' | bound[2] == 'n', lx, ly), lz, 0.17)[i]
        }
      }
      }
    }

  seed$'AirflowNetwork:MultiZone:Surface'[[paste0('afn_core_window_', side[1])]] = NULL  
  
    # internal load (people, lights, electric equipment)
    if (hive[3] == 'living') {
      seed$'People'[[paste0('people_', hive[1])]]$'number_of_people' = 4
      seed$'People'[[paste0('people_', hive[1])]]$'activity_level_schedule_name' =
        'sch_activ_living'
      seed$'People'[[paste0('people_', hive[1])]]$'number_of_people_schedule_name' =
        'sch_occup_living'
      seed$'Lights'[[paste0('lights_', hive[1])]]$'schedule_name' = 'sch_ilum_living'
      seed$'ElectricEquipment'[[paste0('equip_', hive[1])]]$'schedule_name' =
        'sch_equip_living'
    } else if (hive[3] == 'dorm') {
      seed$'People'[[paste0('people_', hive[1])]]$'number_of_people' = 2
      seed$'People'[[paste0('people_', hive[1])]]$'activity_level_schedule_name' =
        'sch_activ_dorm'
      seed$'People'[[paste0('people_', hive[1])]]$'number_of_people_schedule_name' =
        'sch_occup_dorm'
      seed$'Lights'[[paste0('lights_', hive[1])]]$'schedule_name' =
        'sch_ilum_dorm'
      seed$'ElectricEquipment'[[paste0('equip_', hive[1])]] = NULL
    } else {
      seed$'People'[[paste0('people_', hive[1])]] = NULL
      seed$'Lights'[[paste0('lights_', hive[1])]] = NULL
      seed$'ElectricEquipment'[[paste0('equip_', hive[1])]] = NULL
    }
    
  }


  

  
  # core and hive properties ####


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
  
  # save the 'epJSON' file ####
  jsonlite::write_json(seed, paste0(output_dir, model_name, '.epJSON'), pretty = T, auto_unbox = T)
  # print file name
  print(paste0(model_name, '.epJSON'))
# }

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
                         'seed_2nd_model.epJSON'),
           lx = prop[[i]][[1]][1], ly = prop[[i]][[1]][2], lz = prop[[i]][[1]][3],
           alt = prop[[i]][[1]][4], names(prop[[i]])[1], bounds = prop[[i]][[2]],
           output_dir = paste0('/home/rodox/Dropbox/00.master_ufsc/00.single_zone/01.validation/',
                               '00.sz/01.2nd_model/00.model/'),
           model_name = names(prop)[i])
}
