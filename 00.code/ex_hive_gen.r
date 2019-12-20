# base functions ####
# adj_side()
adj_side = function(side) {
  if (side == 's' | side == 'n') {
    adj_side = c('e', 'w')
  }
  else {
    adj_side = c('s', 'n')
  }
  return(adj_side)
}

# build_surf()
build_surf = function(side, surf, lx, ly, lz) {
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

# is_cew()
is_cew = function(side) {
  is_cew = side == 'c' | side == 'e' | side == 'w'
  return(is_cew)
}

# is_csn()
is_csn = function (side) {
  is_csn = side == 'c' | side == 's' | side == 'n'
  return(is_csn)
}

# is_room()
is_room = function(type) {
  is_room = type == 'living' |  type == 'dorm'
  return(is_room)
}

# num()
num = function(value) {
  num = as.numeric(value)
  return(num)
}

# opos_side()
opos_side = function(side) {
  opos_side = ifelse(side == 's', 'n', ifelse(side == 'e', 'w', ifelse(side == 'n', 's', 'e')))
  return(opos_side)
}

# zone_adj()
zone_adj = function(side, n, lx, ly) {
  x_origin = ifelse(is_csn(side), 0,
                    ifelse(side == 'e', ifelse(n == 1, lx, lx+ly), -ifelse(n == 1, ly, 2*ly)))
  y_origin = ifelse(is_cew(side), 0,
                    ifelse(side == 's', -ifelse(n == 1, lx, 2*lx), ifelse(n == 1, ly, lx+ly)))
  return(c(x_origin, y_origin))
}

# main function  ####
# hive_gen()
  # loads a seed file filled with all possible surfaces, fenestrations and conditioning system
    # ('hvac' and 'afn') objects and sorting them out
hive_gen = function(seed, cond, storey, lx, ly, lz, alt, room, bounds, wrap, output_dir,
                    model_name) {
  # seed - epJSON's file full path filled with constant values
  # cond - air conditioning type
    # possible values: 'hvac' and 'afn'
  # storey - storey level
    # possible values: 'floor', 'inter' and 'roof'
  # lx - zone's width
  # ly - zone's depth
  # lz - zone's heigth
  # alt - altitude of the zone
  # room - type of room's occupation
    # possible values: 'living' and 'dorm'
  # bounds - a list containing orientation, boundary condition and wfr (window to floor ratio)
    # possible for boundary conditions: 'outdoors', 'adiabatic', 'dorm' and 'living'
  # wfr - window to floor ratio
    # possible values for wfr: from 0 to the value correspondent to ('window area' / 'floor area')
    # wfr equation: wfr = (sum('glass area') / 'floor_area')
    # obs.: the columns of the data frame must have the all the orientations labelled as follow:
      # 's', 'e', 'n' and 'w'
    # e.g.: bounds = list(c('s', 'outdoors', 0.1), c('e', 'outdoors', 0.05),
                        # c('n', 'dorm', 0), c('w', 'living', 0))
  # wrap - refeers to the wall materials
    # possible values: 'c10' (concreto de 10 cm), 'tv' (tijolo vazado) and 'sf' (steel frame)
  # output_dir - directory where the model is saved
  # model_name - name of the file (model) to be saved

  # pre-process
  
  # load seed file
  seed = rjson::fromJSON(file = seed)
  # create variable with main boundary information to do the loops
  bounds = lapply(bounds, function(x) append(paste0('hive_', paste0(x[1], '1')), x))
  hives = bounds
  bounds = append(list(c('hive_c', 'c', room, NA)), bounds)
  names(bounds) = c('hive_c', 'hive_s', 'hive_e', 'hive_n', 'hive_w')
  for (i in 1:5) {
    bounds[[i]] = append(list(bounds[[i]]), list(c(sub('1', '2', bounds[[i]][1]), NA)))
    bounds[[i]][[2]][3] = ifelse(bounds[[i]][[1]][3] == 'living', 'dorm',
                                 ifelse(bounds[[i]][[1]][3] == 'dorm', 'living',
                                        bounds[[i]][[1]][3]))
  }
  surfs = c('floor', 'roof', 'wall_s', 'wall_e', 'wall_n', 'wall_w')
  vertices = c('vertex_1_x', 'vertex_1_y', 'vertex_1_z', 'vertex_2_x', 'vertex_2_y', 'vertex_2_z',
               'vertex_3_x', 'vertex_3_y', 'vertex_3_z', 'vertex_4_x', 'vertex_4_y', 'vertex_4_z')
  
  # construction
  
  # if - walls are constructed with concreto de 10 cm
  if (wrap == 'c10') {
    for (wall in c('ext_wall', 'int_wall')) {
      seed[['Construction']][[wall]]$outside_layer = 'concreto_10cm'
      seed[['Construction']][[wall]]$idf_max_fields = 2
    }
  }
  # else if - walls are constructed with tijolo vazado
  else if (wrap == 'tv') {
    seed[['Construction']][['ext_wall']]$outside_layer = 'argamassa_externa'
    seed[['Construction']][['int_wall']]$outside_layer = 'argamassa_interna'
    for (wall in c('ext_wall', 'int_wall')) {
      seed[['Construction']][[wall]]$idf_max_fields = 6
      seed[['Construction']][[wall]]$layer_2 = 'tijolo_9x19x19'
      seed[['Construction']][[wall]]$layer_3 = 'camara_parede'
      seed[['Construction']][[wall]]$layer_4 = 'tijolo_9x19x19'
      seed[['Construction']][[wall]]$layer_5 = 'argamassa_interna'
    }
  }
  # else if - walls are constructed with tijolo vazado
  else if (wrap == 'sf') {
    seed[['Construction']][['ext_wall']]$outside_layer = 'placa_cimenticia'
    seed[['Construction']][['int_wall']]$outside_layer = 'gesso'
    for (wall in c('ext_wall', 'int_wall')) {
      seed[['Construction']][[wall]]$idf_max_fields = 4
      seed[['Construction']][[wall]]$layer_2 = 'la_vidro_5cm'
      seed[['Construction']][[wall]]$layer_3 = 'gesso'
    }
  }
  # else - a warns about the absence of material
  else {
    print('Warning: material not selected!')
  }
  
  # for all boundary conditions
  for (bound in bounds) {

    # zone
      # the core zone only needs the z origin, because the x and y are 0 and 0, respectively
    
    # core
    if (bound[[1]][2] == 'c') {
      # 'z' origin vertex
      seed$'Zone'[[bound[[1]][1]]]$'z_origin' = alt
    }
    # hive
    else {
      # run for interior and exterior hives
      for (n in 1:2) {
        # boundary is a room ('living' or 'dorm')
        if (is_room(bound[[n]][3])) {
          # 'x' origin vertex
          seed$'Zone'[[bound[[n]][1]]]$'x_origin' = zone_adj(bound[[1]][2], n, lx, ly)[1]
          # 'y' origin vertex
          seed$'Zone'[[bound[[n]][1]]]$'y_origin' = zone_adj(bound[[1]][2], n, lx, ly)[2]
          # 'z' origin vertex
          seed$'Zone'[[bound[[n]][1]]]$'z_origin' = alt
        }
        # boundary is adiabatic or outdoors
        else {
          # remove all the zone objects
          seed$'Zone'[[bound[[n]][1]]] = NULL
        }
      }
    }
    
    # building surface (boundary conditions) - floors and roofs
    
    # core
    if (bound[[1]][2] == 'c') {
      # if - storey is a floor
      if (storey == 'floor') {
        # floor properties
        seed$'BuildingSurface:Detailed'$'hive_c_floor'$
          'outside_boundary_condition' = 'OtherSideConditionsModel'
        seed$'BuildingSurface:Detailed'$'hive_c_floor'$
          'outside_boundary_condition_object' = 'ground_coupled_oscm'
        # ceiling properties
        seed$'BuildingSurface:Detailed'$'hive_c_roof'$'construction_name' = 'ceiling'
        seed$'BuildingSurface:Detailed'$'hive_c_roof'$'surface_type' = 'Ceiling'
        seed$'BuildingSurface:Detailed'$'hive_c_roof'$'outside_boundary_condition' = 'Adiabatic'
        seed$'BuildingSurface:Detailed'$'hive_c_roof'$'sun_exposure' = 'NoSun'
        seed$'BuildingSurface:Detailed'$'hive_c_roof'$'wind_exposure' = 'NoWind'
      }
      # else - storey is an inter pav. or a roof
      else {
        # floor properties
        seed$'BuildingSurface:Detailed'$'hive_c_floor'$'outside_boundary_condition' = 'Adiabatic'
        seed$'Site:GroundDomain:Slab' = NULL
        seed$'Site:GroundTemperature:Undisturbed:FiniteDifference' = NULL
        seed$'SurfaceProperty:OtherSideConditionsModel' = NULL
        # if - storey is an inter pav.
        if (storey == 'inter') {
          # ceiling properties
          seed$'BuildingSurface:Detailed'$'hive_c_roof'$'construction_name' = 'ceiling'
          seed$'BuildingSurface:Detailed'$'hive_c_roof'$'surface_type' = 'Ceiling'
          seed$'BuildingSurface:Detailed'$'hive_c_roof'$'outside_boundary_condition' = 'Adiabatic'
          seed$'BuildingSurface:Detailed'$'hive_c_roof'$'sun_exposure' = 'NoSun'
          seed$'BuildingSurface:Detailed'$'hive_c_roof'$'wind_exposure' = 'NoWind'
        }
        # if - storey is a roof
        else {
          # roof properties
          seed$'BuildingSurface:Detailed'$'hive_c_roof'$'construction_name' = 'roof'
          seed$'BuildingSurface:Detailed'$'hive_c_roof'$'surface_type' = 'Roof'
          seed$'BuildingSurface:Detailed'$'hive_c_roof'$'outside_boundary_condition' = 'Outdoors'
          seed$'BuildingSurface:Detailed'$'hive_c_roof'$'sun_exposure' = 'SunExposed'
          seed$'BuildingSurface:Detailed'$'hive_c_roof'$'wind_exposure' = 'WindExposed'
        }
      }
    }
    # hive
    else {
      # run for interior and exterior hives
      for (n in 1:2) {
        # if - storey is a floor
        if (storey == 'floor') {
          # floor properties
          seed$'BuildingSurface:Detailed'[[paste0(bound[[n]][1], '_floor')]]$
            'outside_boundary_condition' = 'OtherSideConditionsModel'
          seed$'BuildingSurface:Detailed'[[paste0(bound[[n]][1], '_floor')]]$
            'outside_boundary_condition_object' = 'ground_coupled_oscm'
          # ceiling properties
          seed$'BuildingSurface:Detailed'[[paste0(bound[[n]][1], '_roof')]]$
            'construction_name' = 'ceiling'
          seed$'BuildingSurface:Detailed'[[paste0(bound[[n]][1], '_roof')]]$
            'surface_type' = 'Ceiling'
          seed$'BuildingSurface:Detailed'[[paste0(bound[[n]][1], '_roof')]]$
            'outside_boundary_condition' = 'Adiabatic'
          seed$'BuildingSurface:Detailed'[[paste0(bound[[n]][1], '_roof')]]$
            'sun_exposure' = 'NoSun'
          seed$'BuildingSurface:Detailed'[[paste0(bound[[n]][1], '_roof')]]$
            'wind_exposure' = 'NoWind'
        }
        # else - storey is an inter pav. or a roof
        else {
          # floor properties
          seed$'BuildingSurface:Detailed'[[paste0(bound[[n]][1], '_floor')]]$
            'outside_boundary_condition' = 'Adiabatic'
          # if - storey is an inter pav.
          if (storey == 'inter') {
            # ceiling properties
            seed$'BuildingSurface:Detailed'[[paste0(bound[[n]][1], '_roof')]]$
              'construction_name' = 'ceiling'
            seed$'BuildingSurface:Detailed'[[paste0(bound[[n]][1], '_roof')]]$
              'surface_type' = 'Ceiling'
            seed$'BuildingSurface:Detailed'[[paste0(bound[[n]][1], '_roof')]]$
              'outside_boundary_condition' = 'Adiabatic'
            seed$'BuildingSurface:Detailed'[[paste0(bound[[n]][1], '_roof')]]$
              'sun_exposure' = 'NoSun'
            seed$'BuildingSurface:Detailed'[[paste0(bound[[n]][1], '_roof')]]$
              'wind_exposure' = 'NoWind'
          }
          # else - storey is a roof
          else {
            # roof properties
            seed$'BuildingSurface:Detailed'[[paste0(bound[[n]][1], '_roof')]]$
              'construction_name' = 'roof'
            seed$'BuildingSurface:Detailed'[[paste0(bound[[n]][1], '_roof')]]$
              'surface_type' = 'Roof'
            seed$'BuildingSurface:Detailed'[[paste0(bound[[n]][1], '_roof')]]$
              'outside_boundary_condition' = 'Outdoors'
            seed$'BuildingSurface:Detailed'[[paste0(bound[[n]][1], '_roof')]]$
              'sun_exposure' = 'SunExposed'
            seed$'BuildingSurface:Detailed'[[paste0(bound[[n]][1], '_roof')]]$
              'wind_exposure' = 'WindExposed'
          }
        }
      }
    }
    
    # building surface (geometry) - walls
    
    # run for all the surfaces
    for (surf in surfs) {
      # core
      if (bound[[1]][2] == 'c') {
        # surfaces geometry
        seed$'BuildingSurface:Detailed'[[paste0('hive_c_', surf)]]$
          'vertices' = build_surf(bound[[1]][2],
                                  ifelse(grepl('wall', surf), stringr::str_sub(surf, -1), surf),
                                  lx, ly, lz)
      }
      # hive
      else {
        # run for interior and exterior hives
        for (n in 1:2) {
          # boundary is a room ('living' or 'dorm')
          if (is_room(bound[[n]][3])) {
            # surfaces geometry
            seed$'BuildingSurface:Detailed'[[paste0(bound[[n]][1], '_', surf)]]$
              'vertices' = build_surf(bound[[1]][2],
                                      ifelse(grepl('wall', surf), stringr::str_sub(surf, -1), surf),
                                      lx, ly, lz)
          }
          # boundary is outdoors or adiabatic
          else {
            # remove all the building surface objects
            seed$'BuildingSurface:Detailed'[[paste0(bound[[n]][1], '_', surf)]] = NULL
          }
        }
      }
    }
    
    # building surface (boundary condition)
      # walls
    
    # core
    if (bound[[1]][2] == 'c') {
      # run for hives boundary conditions
      for (hive in hives) {
        # construction name
        seed$'BuildingSurface:Detailed'[[paste0('hive_c_wall_', hive[2])]]$
          'construction_name' = ifelse(hive[3] == 'outdoors', 'ext_wall', 'int_wall')
        # outside boundary condition
        seed$'BuildingSurface:Detailed'[[paste0('hive_c_wall_', hive[2])]]$
          'outside_boundary_condition' = ifelse(hive[3] == 'outdoors', 'Outdoors',
                                                ifelse(hive[3] == 'adiabatic', 'Adiabatic',
                                                       'Surface'))
        # if - boundary is a room ('living' or 'dorm')
        if (is_room(hive[3])) {
          # outside boundary condition object
          seed$'BuildingSurface:Detailed'[[paste0('hive_c_wall_', hive[2])]]$
            'outside_boundary_condition_object' = paste0(hive[1], '_wall_', opos_side(hive[2]))
        }
        # else - boundary is outdoors or adiabatic
        else {
          # outside boundary condition object
          seed$'BuildingSurface:Detailed'[[paste0('hive_c_wall_', hive[2])]]$
            'outside_boundary_condition_object' = NULL
        }
        # sun exposure
        seed$'BuildingSurface:Detailed'[[paste0('hive_c_wall_', hive[2])]]$
          'sun_exposure' = ifelse(hive[3] == 'outdoors', 'SunExposed', 'NoSun')
        # wind exposure
        seed$'BuildingSurface:Detailed'[[paste0('hive_c_wall_', hive[2])]]$
          'wind_exposure' = ifelse(hive[3] == 'outdoors', 'WindExposed', 'NoWind')
      }
    }
    # hive
    else {
      # run for both adjacent sides
        # e.g. 'north' boundary has 'east' and 'west'
      for (a in 1:2) {
        # if - adjacent boundary is a room ('living' or 'dorm')
        if (is_room(bounds[[paste0('hive_', adj_side(bound[[1]][2])[a])]][[1]][3])) { 
          # if - boundary is outdoors
          if (bound[[1]][3] == 'outdoors') {
            # construction name
            seed$'BuildingSurface:Detailed'[[paste0('hive_', adj_side(bound[[1]][2])[a],
                                                    '1_wall_', bound[[1]][2])]]$
              'construction_name' = 'ext_wall'
            # outside boundary condition
            seed$'BuildingSurface:Detailed'[[paste0('hive_', adj_side(bound[[1]][2])[a],
                                                    '1_wall_', bound[[1]][2])]]$
              'outside_boundary_condition' = 'Outdoors'
            
            # sun exposure
            seed$'BuildingSurface:Detailed'[[paste0('hive_', adj_side(bound[[1]][2])[a],
                                                    '1_wall_', bound[[1]][2])]]$
              'sun_exposure' = 'SunExposed'
            # wind exposure
            seed$'BuildingSurface:Detailed'[[paste0('hive_', adj_side(bound[[1]][2])[a],
                                                    '1_wall_', bound[[1]][2])]]$
              'wind_exposure' = 'WindExposed'
          }
          # else - boundary is a room ('living' or 'dorm') or adiabatic
          else {
            # construction name
            seed$'BuildingSurface:Detailed'[[paste0('hive_', adj_side(bound[[1]][2])[a],
                                                    '1_wall_', bound[[1]][2])]]$
              'construction_name' = 'int_wall'
            # outside boundary condition
            seed$'BuildingSurface:Detailed'[[paste0('hive_', adj_side(bound[[1]][2])[a],
                                                    '1_wall_', bound[[1]][2])]]$
              'outside_boundary_condition' = 'Adiabatic'
            # sun exposure
            seed$'BuildingSurface:Detailed'[[paste0('hive_', adj_side(bound[[1]][2])[a],
                                                    '1_wall_', bound[[1]][2])]]$
              'sun_exposure' = 'NoSun'
            # wind exposure
            seed$'BuildingSurface:Detailed'[[paste0('hive_', adj_side(bound[[1]][2])[a],
                                                    '1_wall_', bound[[1]][2])]]$
              'wind_exposure' = 'NoWind'
          }
        }
      }
    }
    
    # fenestration surface (geometry)
    
    # core
    if (bound[[1]][2] == 'c') {
      # run for hives boundary conditions
      for (hive in hives) {
        # if - boundary is a room ('living' or 'dorm')
        if (is_room(hive[3])) {
          # run for the 9 vertices
          for (i in 1:length(vertices)) {
            # there is a door on this surface
            seed$'FenestrationSurface:Detailed'[[paste0('hive_c_door_',
                                                        hive[2])]][[paste0(vertices[i],
                                                                           '_coordinate')]] =
              fen_surf('door', hive[2], lx, ly)[i]
          }
          # there is no window on this surface
          seed$'FenestrationSurface:Detailed'[[paste0('hive_c_window_', hive[2])]] = NULL
        }
        # else if - boundary is outdoor and there is a window
        else if (hive[3] == 'outdoors' & hive[4] > 0) {
          # there is no door on this surface
          seed$'FenestrationSurface:Detailed'[[paste0('hive_c_door_', hive[2])]] = NULL
          for (i in 1:length(vertices)) { # run for the 9 vertices
            # there is a window on this surface
            seed$'FenestrationSurface:Detailed'[[paste0('hive_c_window_',
                                                        hive[2])]][[paste0(vertices[i],
                                                                           '_coordinate')]] =
              fen_surf('window', hive[2], lx, ly, lz, num(hive[4]))[i]
          }
        }
        # else - boundary is outdoors with no window or adiabatic
        else {
          # there is no door
          seed$'FenestrationSurface:Detailed'[[paste0('hive_c_door_', hive[2])]] = NULL
          # there is no window
          seed$'FenestrationSurface:Detailed'[[paste0('hive_c_window_', hive[2])]] = NULL
        }
      }
    }
    # hive
    else {
      # if - boundary is a room
      if (is_room(bound[[1]][3])) {
        # run for interior and exterior hives and both adjacent sides
        for (na in 1:2) {
          # run for the 9 vertices
          for (i in 1:length(vertices)) {
            # there is a door on the interior and exterior hives oposite surfaces
              # e.g. wall 'north' is a room, there is a door on 'south' surface of the interior and
                # exterior 'north' hives
            seed$
              'FenestrationSurface:Detailed'[[paste0(bound[[na]][1], '_door_',
                                                     opos_side(bound[[1]][2]))]][[paste0(vertices[i],
                                                                                         '_coordinate')]] =
              fen_surf('door', opos_side(bound[[1]][2]), ifelse(is_csn(bound[[1]][2]), lx, ly),
                       ifelse(is_csn(bound[[1]][2]), lx, ly))[i]
          }
          # there is no window on this surface of the adjacent interior hives
            # e.g. wall 'north' is a room, there is no window on 'north' surface of the interior
              # 'east' and 'west' hives
          seed$'FenestrationSurface:Detailed'[[paste0('hive_', adj_side(bound[[1]][2])[na],
                                                      '1_window_', bound[[1]][2])]] = NULL
        }
        # run for the 9 vertices
        for (i in 1:length(vertices)) {
          # there is a door on this surface of the interior hive
            # e.g. wall 'north' is a room, there is a door on 'north' surface of the interior
              # 'north' hive
          seed$
            'FenestrationSurface:Detailed'[[paste0(bound[[1]][1], '_door_',
                                                   bound[[1]][2])]][[paste0(vertices[i],
                                                                            '_coordinate')]] =
            fen_surf('door', bound[[1]][2], ifelse(is_csn(bound[[1]][2]), lx, ly),
                     ifelse(is_csn(bound[[1]][2]), lx, ly))[i]
          # there is a window on this surface of the exterior hive
            # e.g. wall 'north' is a room, there is a window on 'north' surface of the exterior
              # 'north' hive
          seed$
            'FenestrationSurface:Detailed'[[paste0(bound[[2]][1], '_window_',
                                                   bound[[1]][2])]][[paste0(vertices[i],
                                                                            '_coordinate')]] =
            fen_surf('window', bound[[1]][2], ifelse(is_csn(bound[[1]][2]), lx, ly),
                     ifelse(is_csn(bound[[1]][2]), lx, ly), lz, 0.17)[i]
        }
      }
      # else - boundary is outdoors or adiabatic
      else {
        # there is no door on this surface of the interior hive
          # e.g. wall 'north' outdoors or adiabatic, there is no door on 'north' surface of the
            # interior 'north' hive
        seed$'FenestrationSurface:Detailed'[[paste0(bound[[1]][1], '_door_', bound[[1]][2])]] = NULL
        # there is no window on this surface of the exterior hive
          # e.g. wall 'north' outdoors or adiabatic, there is no window on 'north' surface of the
            # exterior 'north' hive
        seed$'FenestrationSurface:Detailed'[[paste0(bound[[2]][1], '_window_', bound[[1]][2])]] = NULL
        # run for interior and exterior hives and both adjacent sides
        for (na in 1:2) {
          # there is no door on the interior and exterior hives oposite surfaces
            # e.g. wall 'north' is a room, there is no door on 'south' surface of the interior and
              # exterior 'north' hives
            # of course, there is no room on this boundary!
          seed$'FenestrationSurface:Detailed'[[paste0(bound[[na]][1], '_door_',
                                                      opos_side(bound[[1]][2]))]] = NULL
          # there is no window on the interior hive adjacent surfaces
            # e.g. wall 'north' is a outdoors, there is no window on 'south' surface of the interior
              # and exterior 'north' hives
            # of course, there is no room on this boundary!
          seed$'FenestrationSurface:Detailed'[[paste0(bound[[1]][1], '_window_',
                                                      adj_side(bound[[1]][2]))[na]]] = NULL
          # if - boundary is adiabatic
          if (bound[[1]][3] == 'adiabatic') {
            # there is no window on this surface of the adjacent interior hives
              # e.g. wall 'north' is a room, there is no window on 'north' surface of the interior
                # 'east' and 'west' hives
            seed$'FenestrationSurface:Detailed'[[paste0('hive_', adj_side(bound[[1]][2])[na],
                                                        '1_window_', bound[[1]][2])]] = NULL
          }
          # boundary surface is outdoors
          else {
            # if - adjacent boundary is a room
            if (is_room(bounds[[paste0('hive_', adj_side(bound[[1]][2])[na])]][[1]][3])) {
              # run for the 9 vertices
              for (i in 1:length(vertices)) {
                # there is a window on this surface of the adjacent interior hives
                  # e.g. wall 'north' is a room, there is a window on 'north' surface of the interior
                    # 'east' and 'west' hives
                seed$'FenestrationSurface:Detailed'[[paste0('hive_', adj_side(bound[[1]][2])[na],
                                                            '1_window_',
                                                            bound[[1]][2])]][[paste0(vertices[i],
                                                                                     '_coordinate')]] =
                  fen_surf('window', bound[[1]][2], ifelse(is_csn(adj_side(bound[[1]][2])[na]), lx, ly),
                           ifelse(is_csn(adj_side(bound[[1]][2])[na]), lx, ly), lz, 0.17)[i]
              }
            }
            # else - djacent boundary is adiabatic or outdoors
            else {
              # there is a window on this surface of the adjacent interior hives
                # e.g. wall 'north' is a room, there is a window on 'north' surface of the interior
                  # 'east' and 'west' hives
                # of course, there is no room on this adjacent boundary!
              seed$'FenestrationSurface:Detailed'[[paste0('hive_', adj_side(bound[[1]][2])[na],
                                                          '1_window_', bound[[1]][2])]] = NULL
            }
          }
        }
      }
    }
    
    # airflow network
      # all objects of airflow network surface and zone were already modeled, so, they have just to
        # be removed if it's necessary
      # when there is no window or door, there's no need to use airflow network
      # when there is no zone, both airflow network surface and zone objects can be removed
    
    # core
    if (bound[[1]][2] == 'c') {
      # run for hives boundary conditions
      for (hive in hives) {
        # if - boundary is a room ('living' or 'dorm')
        if (is_room(hive[3])) {
          # no window on this surface
          seed$'AirflowNetwork:MultiZone:Surface'[[paste0('afn_hive_c_window_', hive[2])]] = NULL
        }
        # else if - boundary is adiabatic
        else if (hive[3] == 'adiabatic') {
          # no door on this surface
          seed$'AirflowNetwork:MultiZone:Surface'[[paste0('afn_hive_c_door_', hive[2])]] = NULL
          # no window on this surface
          seed$'AirflowNetwork:MultiZone:Surface'[[paste0('afn_hive_c_window_', hive[2])]] = NULL
        }
        # else - boundary is outdoors
        else {
          # no door on this surface
          seed$'AirflowNetwork:MultiZone:Surface'[[paste0('afn_hive_c_door_', hive[2])]] = NULL
          # if - boundary surface has a window
          if (hive[4] > 0) {
            # if - zone is conditioned by naturally
            if (cond == 'afn') {
              # if - room is a 'living'
              if (room == 'living') {
                seed$'AirflowNetwork:MultiZone:Surface'[[paste0('afn_hive_c_window_', hive[2])]]$
                  'venting_availability_schedule_name' = 'ach_afn_living'
              }
              # else - room is a 'dorm'
              else {
                seed$'AirflowNetwork:MultiZone:Surface'[[paste0('afn_hive_c_window_', hive[2])]]$
                  'venting_availability_schedule_name' = 'sch_afn_dorm'
              }
            }
          }
          # else - boundary surface has no window
          else {
            # no window on this surface
            seed$'AirflowNetwork:MultiZone:Surface'[[paste0('afn_hive_c_window_', hive[2])]] = NULL
          }
        }
      }
    }
    # hive
    else {
      # if - boundary is a room ('living' or 'dorm')
      if (is_room(bound[[1]][3])) {
        # if - boundary is a room ('living'
        if (bound[[2]][3] == 'living') {
          seed$'AirflowNetwork:MultiZone:Surface'[[paste0('afn_', bound[[2]][1],
                                                          '_window_', bound[[1]][2])]]$
            'venting_availability_schedule_name' = 'sch_afn_living'
        }
        # else - boundary is a room ('living'
        else {
          seed$'AirflowNetwork:MultiZone:Surface'[[paste0('afn_', bound[[2]][1],
                                                          '_window_', bound[[1]][2])]]$
            'venting_availability_schedule_name' = 'sch_afn_dorm'
        }
        # run for both adjacent surfaces
        for (a in 1:2) {
          # there is no window on this surface of the adjacent interior hives
            # e.g. wall 'north' is a room, there is no window on 'north' surface of the interior
              # 'east' and 'west' hives
          seed$'AirflowNetwork:MultiZone:Surface'[[paste0('afn_hive_', adj_side(bound[[1]][2])[a],
                                                          '1_window_', bound[[1]][2])]] = NULL
        }
      }
      # else - boundary surface is outdoors or adiabatic
      else {
        # there is no door on this surface of the interior hive
          # e.g. wall 'north' outdoors or adiabatic, there is no door on 'north' surface of the
            # interior 'north' hive
          # of course, there is no room on this adjacent boundary!
        seed$'AirflowNetwork:MultiZone:Surface'[[paste0('afn_', bound[[1]][1], '_door_',
                                                        bound[[1]][2])]] = NULL
        # there is no window on this surface of the exterior hive
          # e.g. wall 'north' outdoors or adiabatic, there is no window on 'north' surface of the
            # exterior 'north' hive
          # of course, there is no room on this adjacent boundary!
        seed$'AirflowNetwork:MultiZone:Surface'[[paste0('afn_', bound[[2]][1], '_window_',
                                                        bound[[1]][2])]] = NULL
        # run for interior and exterior hives and both adjacent surfaces
        for (na in 1:2) {
          # there is no window on the interior hive adjacent surfaces
            # e.g. wall 'north' is a outdoors, there is no window on 'south' surface of the interior
              # and exterior 'north' hives
            # of course, there is no room on this boundary!
          seed$'AirflowNetwork:MultiZone:Surface'[[paste0('afn_', bound[[1]][1], '_window_',
                                                          adj_side(bound[[1]][2])[na])]] = NULL
          # there is no windows or doors on this hives
            # therefore, no airflow network
          seed$'AirflowNetwork:MultiZone:Zone'[[paste0('afn_', bound[[na]][1])]] = NULL
        }
        # if - boundary surface is outdoors
        if (bound[[1]][3] == 'outdoors') {
          # run for both adjacent surfaces
          for (a in 1:2) {
            # if - adjacent boundary is a room
            if (is_room(bounds[[paste0('hive_', adj_side(bound[[1]][2])[a])]][[1]][3])) {
              # if - adjacent boundary is a 'living'
              if (bounds[[paste0('hive_', adj_side(bound[[1]][2])[a])]][[1]][3] == 'living') {
                seed$'AirflowNetwork:MultiZone:Surface'[[paste0('afn_hive_', adj_side(bound[[1]][2])[a],
                                                                '1_window_', bound[[1]][2])]]$
                  'venting_availability_schedule_name' = 'sch_afn_living'
              }
              # else - adjacent boundary is a 'dorm'
              else {
                seed$'AirflowNetwork:MultiZone:Surface'[[paste0('afn_hive_', adj_side(bound[[1]][2])[a],
                                                                '1_window_', bound[[1]][2])]]$
                  'venting_availability_schedule_name' = 'sch_afn_dorm'
              }
            }
            # else - adjacent boundary is a outdoors or adiabatic
            else {
              # there is no window on this surface of the adjacent interior hives
                # e.g. wall 'north' is a room, there is no window on 'north' surface of the interior
                  # 'east' and 'west' hives
              seed$'AirflowNetwork:MultiZone:Surface'[[paste0('afn_hive_',
                                                              adj_side(bound[[1]][2])[a],
                                                              '1_window_', bound[[1]][2])]] = NULL
            }
          }
        } else { # boundary surface is adiabatic
          for (a in 1:2) {
            # there is no window on this surface of the adjacent interior hives
            # e.g. wall 'north' is a room, there is no window on 'north' surface of the interior
            # 'east' and 'west' hives
            seed$'AirflowNetwork:MultiZone:Surface'[[paste0('afn_hive_', adj_side(bound[[1]][2])[a],
                                                            '1_window_', bound[[1]][2])]] = NULL
          }
        }
      }
    }
    
    # internal load (people, lights, electric equipment)
    
    # core
    if (bound[[1]][2] == 'c') {
      # core is a living room
      if (bound[[1]][3] == 'living') {
        # number of occupants
        seed$'People'[['people_hive_c']]$'number_of_people' = 4
        # activity level schedule
        seed$'People'[['people_hive_c']]$'activity_level_schedule_name' = 'sch_activ_living'
        # occupation schedule
        seed$'People'[['people_hive_c']]$'number_of_people_schedule_name' = 'sch_occup_living'
        # lights schedule
        seed$'Lights'[['lights_hive_c']]$'schedule_name' = 'sch_ilum_living'
        # equipment schedule
        seed$'ElectricEquipment'[['equip_hive_c']]$'schedule_name' = 'sch_equip_living'
      }
      # core is a dormitory
      else {
        # number of occupants
        seed$'People'[['people_hive_c']]$'number_of_people' = 2
        # activity level schedule
        seed$'People'[['people_hive_c']]$'activity_level_schedule_name' = 'sch_activ_dorm'
        # occupation schedule
        seed$'People'[['people_hive_c']]$'number_of_people_schedule_name' = 'sch_occup_dorm'
        # lights schedule
        seed$'Lights'[['lights_hive_c']]$'schedule_name' = 'sch_ilum_dorm'
        # equipment schedule
        seed$'ElectricEquipment'[['equip_hive_c']] = NULL 
      }
    }
    # hive
    else {
      # run for interior and exterior hives
      for (n in 1:2) {
        # if - boundary is a living room
        if (bound[[n]][3] == 'living') {
          # number of occupants
          seed$'People'[[paste0('people_', bound[[n]][1])]]$'number_of_people' = 4
          # activity level schedule
          seed$'People'[[paste0('people_', bound[[n]][1])]]$'activity_level_schedule_name' =
            'sch_activ_living'
          # occupation schedule
          seed$'People'[[paste0('people_', bound[[n]][1])]]$'number_of_people_schedule_name' =
            'sch_occup_living'
          # lights schedule
          seed$'Lights'[[paste0('lights_', bound[[n]][1])]]$'schedule_name' = 'sch_ilum_living'
          # equipment schedule
          seed$'ElectricEquipment'[[paste0('equip_', bound[[n]][1])]]$'schedule_name' =
            'sch_equip_living'
        }
        # else if - boundary is a dormitory
        else if (bound[[n]][3] == 'dorm') {
          # number of occupants
          seed$'People'[[paste0('people_', bound[[n]][1])]]$'number_of_people' = 2
          # activity level schedule
          seed$'People'[[paste0('people_', bound[[n]][1])]]$'activity_level_schedule_name' =
            'sch_activ_dorm'
          # occupation schedule
          seed$'People'[[paste0('people_', bound[[n]][1])]]$'number_of_people_schedule_name' =
            'sch_occup_dorm'
          # lights schedule
          seed$'Lights'[[paste0('lights_', bound[[n]][1])]]$'schedule_name' =
            'sch_ilum_dorm'
          # equipment schedule
          seed$'ElectricEquipment'[[paste0('equip_', bound[[n]][1])]] = NULL
        }
        # else - boundary is outdoors or adiabatic
        else {
          # remove people internal loads
          seed$'People'[[paste0('people_', bound[[n]][1])]] = NULL
          # remove lights internal loads
          seed$'Lights'[[paste0('lights_', bound[[n]][1])]] = NULL
          # remove electric equipament internal loads
          seed$'ElectricEquipment'[[paste0('equip_', bound[[n]][1])]] = NULL
        }
      }
    }
    
    # hvac
    
    # if - zone is conditioned artificially
    if (cond == 'hvac') {
      # core
      if (bound[[1]][2] == 'c') {
        # if - core is a 'living'
        if (bound[[1]][3] == 'living') {
          # hvac availability schedule
          seed$'ZoneHVAC:IdealLoadsAirSystem'[['hvac_hive_c']]$
            'availability_schedule_name' = 'sch_hvac_living'
        }
        # else core is a 'dorm'
        else {
          seed$'ZoneHVAC:IdealLoadsAirSystem'[['hvac_hive_c']]$
            'availability_schedule_name' = 'sch_hvac_dorm'
        }
      }
      # hive
      else {
        # run for interior and exterior hives
        for (n in 1:2) {
          # if - boundary is a 'living'
          if (bound[[n]][3] == 'living') {
            # hvac availability schedule
            seed$'ZoneHVAC:IdealLoadsAirSystem'[[paste0('hvac_', bound[[n]][1])]]$
              'availability_schedule_name' = 'sch_hvac_living'
          }
          # else if - boundary is a 'dorm'
          else if (bound[[n]][3] == 'dorm') {
            # hvac availability schedule
            seed$'ZoneHVAC:IdealLoadsAirSystem'[[paste0('hvac_', bound[[n]][1])]]$
              'availability_schedule_name' = 'sch_hvac_dorm'
          }
          # else - boundary is outdoors or adiabatic
            # remove all the expanded hvac objects
          else {
            # thermostat
            seed$'ZoneControl:Thermostat'[[paste0('thermostat_', bound[[n]][1])]] = NULL
            # equipment connections
            seed$'ZoneHVAC:EquipmentConnections'[[paste0('hvac_equip_connect_', bound[[n]][1])]] = NULL
            # equipment list
            seed$'ZoneHVAC:EquipmentList'[[paste0('hvac_equip_', bound[[n]][1])]] = NULL
            # ideal loads air system
            seed$'ZoneHVAC:IdealLoadsAirSystem'[[paste0('hvac_', bound[[n]][1])]] = NULL
          }
        }
      }
    }
  }
  
  # write the 'epJSON' file
  jsonlite::write_json(seed, paste0(output_dir, model_name, '.epJSON'), pretty = T, auto_unbox = T)
  # print file name
  print(paste0(model_name, '.epJSON'))
}

# application ####
prop = list(
  'sw_dorm_1' = list('dorm' = c(4, 3, 2.7, 0),
                     list(c('s', 'outdoors', 0.2), c('e', 'living', 0), c('n', 'adiabatic', 0),
                          c('w', 'dorm', 0))),
  'sw_liv' = list('living' = c(5, 4, 2.7, 0),
                  list(c('s', 'outdoors', 0.2), c('e', 'dorm', 0), c('n', 'adiabatic', 0),
                       c('w', 'dorm', 0))),
  'sw_dorm_2' = list('dorm' = c(3, 4, 2.7, 0),
                     list(c('s', 'outdoors', 0.2), c('e', 'dorm', 0), c('n', 'adiabatic', 0),
                          c('w', 'living', 0))),
  'se_dorm_2' = list('dorm' = c(3, 4, 2.7, 0),
                     list(c('s', 'outdoors', 0.2), c('e', 'living', 0), c('n', 'adiabatic', 0),
                          c('w', 'dorm', 0))),
  'se_liv' = list('living' = c(5, 4, 2.7, 0),
                  list(c('s', 'outdoors', 0.2), c('e', 'dorm', 0), c('n', 'adiabatic', 0),
                       c('w', 'dorm', 0))),
  'se_dorm_1' = list('dorm' = c(4, 3, 2.7, 0),
                     list(c('s', 'outdoors', 0.2), c('e', 'dorm', 0), c('n', 'adiabatic', 0),
                          c('w', 'living', 0))),
  'e_dorm_s' = list('dorm' = c(3, 3, 2.7, 0),
                    list(c('s', 'outdoors', 0.2), c('e', 'outdoors', 0), c('n', 'living', 0),
                         c('w', 'dorm', 0))),
  'e_liv' = list('living' = c(3, 5, 2.7, 0),
                 list(c('s', 'dorm', 0), c('e', 'outdoors', 0.2), c('n', 'dorm', 0),
                      c('w', 'adiabatic', 0))),
  'e_dorm_n' = list('dorm' = c(3, 3, 2.7, 0),
                    list(c('s', 'living', 0), c('e', 'outdoors', 0), c('n', 'outdoors', 0.2),
                         c('w', 'dorm', 0))),
  'ne_dorm_1' = list('dorm' = c(4, 3, 2.7, 0),
                     list(c('s', 'adiabatic', 0), c('e', 'dorm', 0), c('n', 'outdoors', 0.2),
                          c('w', 'living', 0))),
  'ne_liv' = list('living' = c(5, 4, 2.7, 0),
                  list(c('s', 'adiabatic', 0), c('e', 'dorm', 0), c('n', 'outdoors', 0.2),
                       c('w', 'dorm', 0))),
  'ne_dorm_2' = list('dorm' = c(3, 4, 2.7, 0),
                     list(c('s', 'adiabatic', 0), c('e', 'living', 0), c('n', 'outdoors', 0.2),
                          c('w', 'dorm', 0))),
  'nw_dorm_2' = list('dorm' = c(3, 4, 2.7, 0),
                     list(c('s', 'adiabatic', 0), c('e', 'dorm', 0), c('n', 'outdoors', 0.2),
                          c('w', 'living', 0))),
  'nw_liv' = list('living' = c(5, 4, 2.7, 0),
                  list(c('s', 'adiabatic', 0), c('e', 'dorm', 0), c('n', 'outdoors', 0.2),
                       c('w', 'dorm', 0))),
  'nw_dorm_1' = list('dorm' = c(4, 3, 2.7, 0),
                     list(c('s', 'adiabatic', 0), c('e', 'living', 0), c('n', 'outdoors', 0.2),
                          c('w', 'dorm', 0))),
  'w_dorm_n' = list('dorm' = c(3, 3, 2.7, 0),
                    list(c('s', 'living', 0), c('e', 'dorm', 0), c('n', 'outdoors', 0.2),
                         c('w', 'outdoors', 0))),
  'w_liv' = list('living' = c(3, 5, 2.7, 0),
                 list(c('s', 'dorm', 0), c('e', 'adiabatic', 0), c('n', 'dorm', 0),
                      c('w', 'outdoors', 0.2))),
  'w_dorm_s' = list('dorm' = c(3, 3, 2.7, 0),
                    list(c('s', 'outdoors', 0.2), c('e', 'dorm', 0), c('n', 'living', 0),
                         c('w', 'outdoors', 0)))
)

wraps = c('c10', 'tv', 'sf')
storeys = c('floor', 'inter', 'roof')
# create 'epjsons'
n = 0
for (wrap in wraps) {
  m = 0
  for (storey in storeys) {
    for (i in 1:length(prop)) {
      hive_gen(seed = paste0('/home/rodox/00.git/00.master_ufsc/01.seed/seed_ex_hive_hvac.epJSON'),
               lx = prop[[i]][[1]][1], ly = prop[[i]][[1]][2], lz = prop[[i]][[1]][3],
               alt = prop[[i]][[1]][4], room = names(prop[[i]])[1], bounds = prop[[i]][[2]],
               cond = 'hvac', wrap = wrap, storey = storey, model_name =
                 paste0('hyp_', wrap, '_v04_', storey, '_', names(prop)[i]),
               output_dir = paste0('/home/rodox/00.git/00.master_ufsc/02.model/01.hvac/00.hyp/04/0',
                                   n, '.', wrap, '/'))
    }
    m = m + 1
  }
  n = n + 1
}
rm(output_dir, wraps, prop)

