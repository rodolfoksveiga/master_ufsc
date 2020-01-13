# base functions ####
# cp_calc()
  # calculate the coefficient pressure values according to energyplus algorithm routine and return
    # the 'epJSON' object 'AirflowNetwork:MultiZone:WindPressureCoefficientValues' for each of the
    # 4 directions (south, east, north and west)
cp_calc = function(ratio = 1, build_type = 'low_rise', azimuth = 0) {
  # ratio - relation between the shortest and longest side of the building
    # formula: ratio = width / depth
  # build_type - 'low_rise' or 'high_rise'
  # azimuth - azimuth angle in degrees
    # possible values: from 0 to 359 degrees

  cp_values = list()
  # cp_hr_wall is related to each of the 12 angles that energyplus consider
  cp_hr_wall = list(c(0.60, 0.54, 0.23, -0.25, -0.61, -0.55,
                      -0.51, -0.55, -0.61, -0.25, 0.23, 0.54),
                    c(0.60, 0.48, 0.04, -0.56, -0.56, -0.42,
                      -0.37, -0.42, -0.56, -0.56, 0.04, 0.48),
                    c(0.60, 0.44, -0.26, -0.70, -0.53, -0.32,
                      -0.22, -0.32, -0.53, -0.70, -0.26, 0.44))
  
  surfs = c('afn_cp_surf_s', 'afn_cp_surf_e', 'afn_cp_surf_n', 'afn_cp_surf_w')
  
  for (i in 1:length(surfs)) {
    cp_values[[surfs[i]]] = list(
      'airflownetwork_multizone_windpressurecoefficientarray_name' = 'afn_cp_30_degrees',
      'idf_max_extensible_fields' = 0,
      'idf_max_fields' = 14
    )
    facade_ang = (azimuth + i*90) %% 360
    if (i == 2 | i == 4) {
      side_ratio = 1/ratio
    } else {
      side_ratio = ratio
    }
    side_ratio_fac = log(side_ratio)
    for (j in 1:12) {
      wind_ang = j*30
      inc_ang = abs(wind_ang - facade_ang)
      if (inc_ang > 180) {
        inc_ang = 360 - inc_ang
      }
      i_ang = as.integer((inc_ang/30) + 1)
      del_ang = inc_ang %% 30
      wt_ang = 1.0 - del_ang / 30
      
      if (build_type == 'low_rise') { # low rise buildings
        inc_rad = inc_ang*pi/180
        cp = 0.6 * log(1.248 - 0.703*sin(inc_rad/2) - 1.175*sin(inc_rad)^2 +
                         0.131*sin(2*inc_rad*log(ratio))^3 + 0.769*cos(inc_rad/2) +
                         0.07*log(ratio)^2 * sin(inc_rad/2)^2 + 0.717*cos(inc_rad/2)^2)
      } else { #high rise buildings
        sr = min(max(side_ratio, 0.25), 4)
        if (sr >= 0.25 & sr < 1) {
          isr = 1
          wtsr = (1 - sr) / 0.75
        } else {
          isr = 2
          wtsr = (4 - sr) / 3
        }
        cp = wtsr * (wt_ang * cp_hr_wall[[isr]][i_ang] +
                       (1 - wt_ang) * cp_hr_wall[[isr]][i_ang + 1]) +
          (1 - wtsr)*(wt_ang * cp_hr_wall[[isr + 1]][i_ang] +
                        (1 - wt_ang)*cp_hr_wall[[isr + 1]][i_ang + 1])
      }
      cp_values[[surfs[i]]][[paste0('wind_pressure_coefficient_value_', as.character(j))]] = cp
    }
  }
  
  return(cp_values)
}

# simplification

file_path = '/home/rodox/00.git/00.master_ufsc/02.model/00.hyp/04/hyp_c10_v03_floor_hvac.epJSON'
zone_name = 'e_dorm_n'
cond = 'hvac'

file = rjson::fromJSON(file = file_path)

# main function ####
# simp()
  # splits the building or the floor in one thermal zone and the adjacent zones
# simp = function(file_path, zone_name, cond)
# building surfaces

adj_surfs = c()
# position of the central zone walls
pos_walls = grep(paste0('^', zone_name, '_wall'), names(file$'BuildingSurface:Detailed'))
n = 1
# adjcent surfaces with the central zone surfaces
for (i in pos_walls) {
    adj_surfs[n] = file$'BuildingSurface:Detailed'[[i]]$'outside_boundary_condition_object'
  n = n + 1
}
# central zone surfaces with adjacent surfaces
zone_surfs = names(file$'BuildingSurface:Detailed')[pos_walls[!is.na(adj_surfs)]]
# remove na's to the adjcent surfaces vector
adj_surfs = adj_surfs[!is.na(adj_surfs)]
# adjacent zones
adj_zones = unique(sub(
  '_wall_s', '', sub('_wall_e', '', sub('_wall_w', '', sub('_wall_n', '', adj_surfs))))
  )
# all the zones
zones = c(zone_name, adj_zones)

# position of all the zones
pos_zone_surfs = c()
for (zone in zones) {
pos_zone_surfs = c(pos_zone_surfs, grep(paste0('^', zone), names(file$'BuildingSurface:Detailed')))
}

build_surfs = vector('list', length(pos_zone_surfs))
n = 1
for (i in pos_zone_surfs) {
  build_surfs[[n]] = file$'BuildingSurface:Detailed'[[i]]
  names(build_surfs)[n] = names(file$'BuildingSurface:Detailed')[i]
  n = n + 1
}
file$'BuildingSurface:Detailed' = build_surfs

pos_adj_walls = c()
for (adj_zone in adj_zones) {
  pos_adj_walls = c(pos_adj_walls,
                    grep(paste0('^', adj_zone, '_wall'), names(file$'BuildingSurface:Detailed')))
}

crack_surfs = c()
for (i in pos_adj_walls) {
  if (file$'BuildingSurface:Detailed'[[i]]$'outside_boundary_condition' == 'Surface') {
    if (any(zone_surfs ==
            file$'BuildingSurface:Detailed'[[i]]$'outside_boundary_condition_object')) {
    } else {
      file$'BuildingSurface:Detailed'[[i]]$'outside_boundary_condition' = 'Adiabatic'
      file$'BuildingSurface:Detailed'[[i]]$'outside_boundary_condition_object' = NULL
      crack_surfs = c(crack_surfs, names(file$'BuildingSurface:Detailed')[i])
    }
  }
}
crack_surfs = sub('wall', 'door', crack_surfs)


# fenestration surfaces

doors = c(adj_surfs, zone_surfs)
doors = sub('wall', 'door', doors)

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
  fen_surfs[[n]] = file$'FenestrationSurface:Detailed'[[i]]
  names(fen_surfs)[n] = names(file$'FenestrationSurface:Detailed')[i]
  n = n + 1
}
for (i in pos_windows) {
  fen_surfs[[n]] = file$'FenestrationSurface:Detailed'[[i]]
  names(fen_surfs)[n] = names(file$'FenestrationSurface:Detailed')[i]
  n = n + 1
}
file$'FenestrationSurface:Detailed' = fen_surfs


# afn surfaces

pos_afn_surfs = c()
for (fen in names(file$'FenestrationSurface:Detailed')) {
  pos_afn_surfs = c(pos_afn_surfs,
                    grep(paste0('afn_', fen), names(file$'AirflowNetwork:MultiZone:Surface')))
}

afn_surfs = vector('list', length(pos_afn_surfs))
n = 1
for (i in pos_afn_surfs) {
  afn_surfs[[n]] = file$'AirflowNetwork:MultiZone:Surface'[[i]]
  names(afn_surfs)[n] = names(file$'AirflowNetwork:MultiZone:Surface')[i]
  n = n + 1
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
    pos_equip_cons = c(pos_equip_cons,
                       grep(paste0('equip_con_', zone), names(file$'ZoneHVAC:EquipmentConnections')))
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


# cp

# step 1 - afn control
file$'AirflowNetwork:SimulationControl'$'afn_control'$'wind_pressure_coefficient_type' = 'Input'

# step 2 - cp array  
file$'AirflowNetwork:MultiZone:WindPressureCoefficientArray'$
  'afn_cp_30_degrees' = vector('list', 14)
d = 0
for (i in 1:12) {
  file$'AirflowNetwork:MultiZone:WindPressureCoefficientArray'$'afn_cp_30_degrees'[[i]] = d
  names(file$'AirflowNetwork:MultiZone:WindPressureCoefficientArray'$'afn_cp_30_degrees')[i] =
    paste0('wind_direction_', i)
  d = d + 30
}
file$'AirflowNetwork:MultiZone:WindPressureCoefficientArray'$'afn_cp_30_degrees'$
  'idf_max_extensible_fields' = 0
file$'AirflowNetwork:MultiZone:WindPressureCoefficientArray'$'afn_cp_30_degrees'$
  'idf_max_fields' = 13

# step 3 - cp values
file$'AirflowNetwork:MultiZone:WindPressureCoefficientValues' = cp_calc()

# step 4 - external nodes
afn_surfs = sub('afn_', '', names(file$'AirflowNetwork:MultiZone:Surface'))
for (i in 1:length(afn_surfs)) {
  file$'AirflowNetwork:MultiZone:Surface'[[i]]$
    'external_node_name' = paste0('afn_node_', afn_surfs[i])
  file$'AirflowNetwork:MultiZone:Surface'[[i]]$'idf_max_fields' = 12
}
afn_surfs = c(afn_surfs, crack_surfs)
alt =
  file$'BuildingSurface:Detailed'[[paste0(zone_name, '_roof')]]$vertices[[1]]$'vertex_z_coordinate'
for (surf in afn_surfs) {
  file$'AirflowNetwork:MultiZone:ExternalNode'[[paste0('afn_node_', surf)]]$
    'idf_max_extensible_fields' = 0
  file$'AirflowNetwork:MultiZone:ExternalNode'[[paste0('afn_node_', surf)]]$
    'idf_max_fields' = 3
  if (grepl('window', surf)) {
    file$'AirflowNetwork:MultiZone:ExternalNode'[[paste0('afn_node_', surf)]]$
      'external_node_height' = file$'Zone'[[zone_name]]$'z_origin' + alt/2
  } else {
    file$'AirflowNetwork:MultiZone:ExternalNode'[[paste0('afn_node_', surf)]]$
      'external_node_height' = file$'Zone'[[zone_name]]$'z_origin' + 2.1/2
  }
  side = ifelse(grepl('_s$', surf), 's',
                ifelse(grepl('_e$', surf), 'e',
                       ifelse(grepl('_n$', surf), 'n', 'w')))
  file$'AirflowNetwork:MultiZone:ExternalNode'[[paste0('afn_node_', surf)]]$
    'wind_pressure_coefficient_curve_name' = paste0('afn_cp_surf_', side)
}

# step 4 - crack
out_walls = c()
for (i in pos_adj_walls) {
  if (grepl('wall', names(file$'BuildingSurface:Detailed')[i]) &
      file$'BuildingSurface:Detailed'[[i]]$'outside_boundary_condition' == 'Outdoors') {
    out_walls = c(out_walls, names(file$'BuildingSurface:Detailed')[i])
  }
}
for (surf in crack_surfs) {
  for (adj_zone in adj_zones) {
    if (grepl(paste0('^', adj_zone), surf)) {
      crack_zone = adj_zone
    }
  }
  pos_crack_out_walls = grep(paste0('^', sub('door', 'wall', crack_zone)), out_walls)
  file$'AirflowNetwork:MultiZone:Crack'[[paste0('afn_crack_', surf)]]$
    'idf_max_extensible_fields' = 0
  file$'AirflowNetwork:MultiZone:Crack'[[paste0('afn_crack_', surf)]]$'idf_max_fields' = 4
  file$'AirflowNetwork:MultiZone:Crack'[[paste0('afn_crack_', surf)]]$
    'name_of_surface_crack_component' = out_walls[pos_crack_out_walls[1]]
  file$'AirflowNetwork:MultiZone:Crack'[[paste0('afn_crack_', surf)]]$
    'air_mass_flow_coefficient_at_reference_conditions' = 0.0024
  file$'AirflowNetwork:MultiZone:Crack'[[paste0('afn_crack_', surf)]]$
    'air_mass_flow_exponent' = 0.59
  
  file$'AirflowNetwork:MultiZone:Surface'[[paste0('afn_', surf)]]$'idf_max_fields' = 12
  file$'AirflowNetwork:MultiZone:Surface'[[paste0('afn_', surf)]]$
    'idf_max_extensible_fields' = 0
  file$'AirflowNetwork:MultiZone:Surface'[[paste0('afn_', surf)]]$
    'leakage_component_name' = paste0('afn_crack_', surf)
  file$'AirflowNetwork:MultiZone:Surface'[[paste0('afn_', surf)]]$
    'external_node_name' = paste0('afn_node_', surf)
  file$'AirflowNetwork:MultiZone:Surface'[[paste0('afn_', surf)]]$
    'surface_name' = out_walls[pos_crack_out_walls[1]]
}

# write the 'epJSON' file
jsonlite::write_json(file, '/home/rodox/Desktop/test.epJSON', pretty = T, auto_unbox = T)
# print file name
print('test.epJSON')
