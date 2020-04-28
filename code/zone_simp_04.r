# load libraries ####
library(stringr)

# base functions ####
# double_window()
  # creates two identic windows side by side, based on a main window
double_window = function(window, side, order) {
  if (side == 's') {
    width = window[['vertex_3_x_coordinate']] - window[['vertex_1_x_coordinate']]
    if (order == 1) {
      for (i in c('3', '4')) {
        window[[paste0('vertex_', i, '_x_coordinate')]] =
          window[['vertex_1_x_coordinate']] + width/2
      }
    } else {
      for (i in c('1', '2')) {
        window[[paste0('vertex_', i, '_x_coordinate')]] =
          window[['vertex_3_x_coordinate']] - width/2
      }
    }
  }
  if (side == 'e') {
    width = window[['vertex_3_y_coordinate']] - window[['vertex_1_y_coordinate']]
    if (order == 1) {
      for (i in c('3', '4')) {
        window[[paste0('vertex_', i, '_y_coordinate')]] =
          window[[paste0('vertex_1_y_coordinate')]] + width/2
      }
    } else {
      for (i in c('1', '2')) {
        window[[paste0('vertex_', i, '_y_coordinate')]] =
          window[[paste0('vertex_3_y_coordinate')]] - width/2
      }
    }
  }
  if (side == 'n') {
    width = window[['vertex_1_x_coordinate']] - window[['vertex_3_x_coordinate']]
    if (order == 1) {
      for (i in c('3', '4')) {
        window[[paste0('vertex_', i, '_x_coordinate')]] =
          window[[paste0('vertex_1_x_coordinate')]] - width/2
      }
    } else {
      for (i in c('1', '2')) {
        window[[paste0('vertex_', i, '_x_coordinate')]] =
          window[[paste0('vertex_3_x_coordinate')]] + width/2
      }
    }
  }
  if (side == 'w') {
    width = window[['vertex_1_y_coordinate']] - window[['vertex_3_y_coordinate']]
    if (order == 1) {
      for (i in c('3', '4')) {
        window[[paste0('vertex_', i, '_y_coordinate')]] =
          window[[paste0('vertex_1_y_coordinate')]] - width/2
      }
    } else {
      for (i in c('1', '2')) {
        window[[paste0('vertex_', i, '_y_coordinate')]] =
          window[[paste0('vertex_3_y_coordinate')]] + width/2
      }
    }
  }
  return(window)
}

# main function ####
# simp()
  # splits the floor in one thermal zone and the adjacent zones
    # also creates double windows in zones whose natural ventilation are impossible
simp = function(input_path, zone_name, cond, output_dir) {
  # input_path - full seed file path
  # zone_name - name of the zone to be extracted
  # cond - type of climatization
    # possible values: 'afn' (natural ventilation) or 'hvac' (artificial air conditioning)
  
  # test
  # typos = c('hyp', 'lin', 'h')
  # wraps = c('c10', 'tv', 'sf')
  # storeys = c('floor', 'inter', 'roof')
  # conds = c('afn', 'hvac')
  # zones = c('sw_dorm_1', 'sw_liv', 'sw_dorm_2', 'se_dorm_2', 'se_liv', 'se_dorm_1', 'e_dorm_s',
  #           'e_liv', 'e_dorm_n', 'ne_dorm_1', 'ne_liv', 'ne_dorm_2', 'nw_dorm_2', 'nw_liv',
  #           'nw_dorm_1', 'w_dorm_n', 'w_liv', 'w_dorm_s')
  # input_path = paste0('/home/rodox/00.git/00.master_ufsc/02.model/00.hyp/03/',
  #                     typos[1], '_', wraps[1], '_v03_', storeys[1], '_', conds[1], '.epJSON')
  # zone_name = zones[1]
  # cond = conds[1]
  # output_dir = '/home/rodox/00.git/00.master_ufsc/02.model/00.hyp/04/'
  
  # load '.epjson' file
  file = rjson::fromJSON(file = input_path)
  
  # building surfaces
  adj_surfs = c()
  pos_walls = grep(paste0('^', zone_name, '_wall'), names(file$'BuildingSurface:Detailed'))
  pos_bounds = c()
  n = 1
  for (i in pos_walls) {
    if (file$'BuildingSurface:Detailed'[[i]]$'outside_boundary_condition' == 'Surface') {
      adj_surfs[n] = c(file$'BuildingSurface:Detailed'[[i]]$'outside_boundary_condition_object')
      pos_bounds[n] = i
      n = n + 1
    }
  }
  zone_surfs = names(file$'BuildingSurface:Detailed')[pos_bounds]
  adj_zones = unique(
    sub('_wall_s', '',
        sub('_wall_e', '',
            sub('_wall_w', '',
                sub('_wall_n', '',
                    sub('[1234]$', '', adj_surfs)))))
  )
  zones = c(zone_name, adj_zones)
  
  pos_zone_surfs = c()
  for (zone in zones) {
    pos_zone_surfs = c(pos_zone_surfs, grep(paste0('^', zone),
                                            names(file$'BuildingSurface:Detailed')))
  }
  
  build_surfs = vector('list', length(pos_zone_surfs))
  n = 1
  for (i in pos_zone_surfs) {
    build_surfs[[n]] = file$'BuildingSurface:Detailed'[[i]]
    names(build_surfs)[n] = names(file$'BuildingSurface:Detailed')[i]
    n = n + 1
  }
  
  pos_adj_walls = c()
  for (adj_zone in adj_zones) {
    pos_adj_walls = c(pos_adj_walls, grep(paste0('^', adj_zone, '_wall'), names(build_surfs)))
  }
  
  adiab_surfs = c()
  for (i in pos_adj_walls) {
    if (build_surfs[[i]]$'outside_boundary_condition' == 'Surface') {
      if (any(zone_surfs == build_surfs[[i]]$'outside_boundary_condition_object')) {
      } else {
        build_surfs[[i]]$'outside_boundary_condition' = 'Adiabatic'
        build_surfs[[i]]$'outside_boundary_condition_object' = NULL
        adiab_surfs = c(adiab_surfs, names(build_surfs)[i])
      }
    }
  }
  
  file$'BuildingSurface:Detailed' = build_surfs
  
  # fenestration surfaces
  doors = c(adj_surfs, zone_surfs)
  doors = unique(sub('wall', 'door', sub('[1234]$', '', doors)))
  pos_doors = c()
  for (door in doors) {
    pos_doors = c(pos_doors, grep(door, names(file$'FenestrationSurface:Detailed')))
  }
  
  pos_windows = c()
  for (zone in zones) {
    pos_windows = c(pos_windows,
                    grep(paste0('^', zone, '_window'), names(file$'FenestrationSurface:Detailed')))
  }
  
  fen_surfs = vector('list', length(pos_doors) + length(pos_windows))
  n = 1
  for (i in pos_doors) {
    build_surf = file$'FenestrationSurface:Detailed'[[i]]$'building_surface_name'
    if (file$'BuildingSurface:Detailed'[[build_surf]]$'outside_boundary_condition' == 'Surface') {
      fen_surfs[[n]] = file$'FenestrationSurface:Detailed'[[i]]
      names(fen_surfs)[n] = names(file$'FenestrationSurface:Detailed')[i]
      n = n + 1
    }
  }
  for (i in pos_windows) {
    fen_surfs[[n]] = file$'FenestrationSurface:Detailed'[[i]]
    names(fen_surfs)[n] = names(file$'FenestrationSurface:Detailed')[i]
    n = n + 1
  }
  
  file$'FenestrationSurface:Detailed' = fen_surfs
  
  # afn surfaces
  pos_afn_surfs = c()
  for (zone in names(file$'FenestrationSurface:Detailed')) {
    pos_afn_surfs = c(pos_afn_surfs,
                      grep(paste0('afn_', zone), names(file$'AirflowNetwork:MultiZone:Surface')))
  }
  
  afn_surfs = vector('list', length(pos_afn_surfs))
  n = 1
  for (i in pos_afn_surfs) {
    afn_surfs[[n]] = file$'AirflowNetwork:MultiZone:Surface'[[i]]
    names(afn_surfs)[n] = names(file$'AirflowNetwork:MultiZone:Surface')[i]
    n = n + 1
  }
  
  file$'AirflowNetwork:MultiZone:Surface' = afn_surfs
  
  # add double windows
  zones_one_fen = c()
  for (zone in zones) {
    if (sum(grepl(paste0('^', zone), names(fen_surfs))) == 1) {
      zones_one_fen = c(zones_one_fen, zone)
    }
  }
  
  pos_one_fen = c()
  for (zone in zones_one_fen) {
    pos_one_fen = c(pos_one_fen, grep(paste0('^', zone), names(fen_surfs)))
  }
  
  for (i in pos_one_fen) {
    fen_surfs[[length(fen_surfs) + 1]] = fen_surfs[[i]]
    names(fen_surfs)[length(fen_surfs)] = paste0(names(fen_surfs)[i], '2')
    names(fen_surfs)[i] = paste0(names(fen_surfs)[i], '1')
    fen_surfs[[i]] =
      double_window(fen_surfs[[i]], str_sub(names(fen_surfs)[i], -2, -2), 1)
    fen_surfs[[length(fen_surfs)]] =
      double_window(fen_surfs[[length(fen_surfs)]],
                    str_sub(names(fen_surfs)[length(fen_surfs)], -2, -2), 2)
  }
  
  file$'FenestrationSurface:Detailed' = fen_surfs
  
  pos_one_afn = c()
  for (zone in zones_one_fen) {
    pos_one_afn = c(pos_one_afn, grep(paste0('afn_', zone), names(afn_surfs)))
  }
  
  for (i in pos_one_afn) {
    afn_surfs[[length(afn_surfs) + 1]] = afn_surfs[[i]]
    names(afn_surfs)[length(afn_surfs)] = paste0(names(afn_surfs)[i], '2')
    names(afn_surfs)[i] = paste0(names(afn_surfs)[i], '1')
    afn_surfs[[i]]$surface_name = str_sub(names(afn_surfs)[i], 5)
    afn_surfs[[length(afn_surfs)]]$surface_name =
      str_sub(names(afn_surfs)[length(afn_surfs)], 5)
  }
  
  file$'AirflowNetwork:MultiZone:Surface' = afn_surfs
  
  # afn zones
  pos_afn_zones = c()
  for (zone in zones) {
    pos_afn_zones = c(pos_afn_zones,
                      grep(paste0('afn_', zone), names(file$'AirflowNetwork:MultiZone:Zone')))
  }
  
  afn_zones = vector('list', length(pos_afn_zones))
  n = 1
  for (i in pos_afn_zones) {
    afn_zones[[n]] = file$'AirflowNetwork:MultiZone:Zone'[[i]]
    names(afn_zones)[n] = names(file$'AirflowNetwork:MultiZone:Zone')[i]
    n = n + 1
  }
  
  file$'AirflowNetwork:MultiZone:Zone' = afn_zones
  
  # zones
  pos_zones = c()
  for (zone in zones) {
    pos_zones = c(pos_zones, grep(paste0('^', zone), names(file$'Zone')))
  }
  
  zones_obj = vector('list', length(pos_zones))
  n = 1
  for (i in pos_zones) {
    zones_obj[[n]] = file$'Zone'[[i]]
    names(zones_obj)[n] = names(file$'Zone')[i]
    n = n + 1
  }
  
  file$'Zone' = zones_obj
  
  # zone list
  pos_dorms = c()
  pos_livs = c()
  for (zone in zones) {
    for (i in 1:length(file$'ZoneList'$dorms$zones)) {
      pos_dorms = c(pos_dorms,
                    ifelse(grepl(paste0('^', zone), file$'ZoneList'$dorms$zones[[i]]), i, NA))
    }
    for (i in 1:length(file$'ZoneList'$livs$zones)) {
      pos_livs = c(pos_livs,
                   ifelse(grepl(paste0('^', zone), file$'ZoneList'$livs$zones[[i]]), i, NA))
    }
  }
  pos_dorms = pos_dorms[!is.na(pos_dorms)]
  pos_livs = pos_livs[!is.na(pos_livs)]
  
  dorm_zones = vector('list', length(pos_dorms))
  n = 1
  for (i in pos_dorms) {
    dorm_zones[[n]] = file$'ZoneList'$dorms$zones[[i]]
    names(dorm_zones)[n] = names(file$'ZoneList'$dorms$zones)[i]
    n = n + 1
  }
  
  file$'ZoneList'$dorms$zones = dorm_zones
  
  liv_zones = vector('list', length(pos_livs))
  n = 1
  for (i in pos_livs) {
    liv_zones[[n]] = file$'ZoneList'$livs$zones[[i]]
    names(liv_zones)[n] = names(file$'ZoneList'$livs$zones)[i]
    n = n + 1
  }
  
  file$'ZoneList'$livs$zones = liv_zones
  
  # hvac
  if (cond == 'hvac') {
    # hvac thermostat
    pos_therms = c()
    for (zone in zones) {
      pos_therms = c(pos_therms,
                     grep(paste0('thermostat_', zone), names(file$'ZoneControl:Thermostat')))
    }
    therms = vector('list', length(pos_therms))
    n = 1
    for (i in pos_therms) {
      therms[[n]] = file$'ZoneControl:Thermostat'[[i]]
      names(therms)[n] = names(file$'ZoneControl:Thermostat')[i]
      n = n + 1
    }
    file$'ZoneControl:Thermostat' = therms
    
    # hvac equip con
    pos_equip_cons = c()
    for (zone in zones) {
      pos_equip_cons = c(pos_equip_cons, grep(paste0('equip_con_', zone),
                                              names(file$'ZoneHVAC:EquipmentConnections')))
    }
    equip_cons = vector('list', length(pos_equip_cons))
    n = 1
    for (i in pos_equip_cons) {
      equip_cons[[n]] = file$'ZoneHVAC:EquipmentConnections'[[i]]
      names(equip_cons)[n] = names(file$'ZoneHVAC:EquipmentConnections')[i]
      n = n + 1
    }
    file$'ZoneHVAC:EquipmentConnections' = equip_cons
    
    # hvac equip
    pos_equips = c()
    for (zone in zones) {
      pos_equips = c(pos_equips,
                     grep(paste0('hvac_equip_', zone), names(file$'ZoneHVAC:EquipmentList')))
    }
    equips = vector('list', length(pos_equips))
    n = 1
    for (i in pos_equips) {
      equips[[n]] = file$'ZoneHVAC:EquipmentList'[[i]]
      names(equips)[n] = names(file$'ZoneHVAC:EquipmentList')[i]
      n = n + 1
    }
    file$'ZoneHVAC:EquipmentList' = equips
    
    # hvac
    pos_hvacs = c()
    for (zone in zones) {
      pos_hvacs = c(pos_hvacs,
                    grep(paste0('hvac_', zone), names(file$'ZoneHVAC:IdealLoadsAirSystem')))
    }
    hvacs = vector('list', length(pos_hvacs))
    n = 1
    for (i in pos_hvacs) {
      hvacs[[n]] = file$'ZoneHVAC:IdealLoadsAirSystem'[[i]]
      names(hvacs)[n] = names(file$'ZoneHVAC:IdealLoadsAirSystem')[i]
      n = n + 1
    }
    file$'ZoneHVAC:IdealLoadsAirSystem' = hvacs
  }
  
  # write the 'epJSON' file
  output_path = paste0(output_dir, sub('3', '4', str_remove(basename(input_path), '.epJSON')),
                       '_', zone_name, '.epJSON')
  jsonlite::write_json(file, output_path, pretty = T, auto_unbox = T)
  # print file name
  print(output_path)
}

# application ####
typos = c('hyp', 'lin', 'h')
wraps = c('c10', 'tv', 'sf')
storeys = c('floor', 'inter', 'roof')
conds = c('afn', 'hvac')
zones = c('sw_dorm_1', 'sw_liv', 'sw_dorm_2', 'se_dorm_2', 'se_liv', 'se_dorm_1', 'e_dorm_s',
          'e_liv', 'e_dorm_n', 'ne_dorm_1', 'ne_liv', 'ne_dorm_2', 'nw_dorm_2', 'nw_liv',
          'nw_dorm_1', 'w_dorm_n', 'w_liv', 'w_dorm_s')
for (typo in typos) {
  for (wrap in wraps) {
    for (storey in storeys) {
      for (cond in conds) {
        for (zone in zones) {
          simp(input_path = paste0('/home/rodox/00.git/00.master_ufsc/02.model/00.hyp/03/',
                                   typo, '_', wrap, '_v03_', storey, '_', cond, '.epJSON'),
               zone_name = zone, cond = cond,
               output_dir = '/home/rodox/00.git/00.master_ufsc/02.model/00.hyp/04/')
        }
      }
    }
  }
}

