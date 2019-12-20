# simplification

file_path = '/home/rodox/00.git/00.master_ufsc/02.model/00.hyp/04/hyp_c10_v03_floor_hvac.epJSON'
zone_name = 'e_dorm_n'

file = rjson::fromJSON(file = file_path)
adj_surfs = c()
pos_walls = grep(paste0('^', zone_name, '_wall'), names(file$'BuildingSurface:Detailed'))
n = 1
for (i in pos_walls) {
    adj_surfs[n] = file$'BuildingSurface:Detailed'[[i]]$'outside_boundary_condition_object'
  n = n + 1
}
adj_surfs = adj_surfs[!is.na(adj_surfs)]
adj_zones = unique(sub(
  '_wall_s', '', sub('_wall_e', '', sub('_wall_w', '', sub('_wall_n', '', adj_surfs))))
  )
zones = c(zone_name, adj_zones)
# n = 1
# for (build_surf in file$'BuildingSurface:Detailed') {
#   for (zone in zones) {
#     if (!grepl(paste0('^', zone), names(buil_surf)[n])) {
#       file$'BuildingSurface:Detailed'[[i]] = NULL
#     }
#   }
#   n = n + 1
# }


pos_zones = c()
for (zone in zones) {
pos_zones = c(pos_zones, grep(paste0('^', zone), names(file$'BuildingSurface:Detailed')))
}

build_surfs = vector('list', length(pos_zones))
n = 1
for (i in pos_zones) {
  build_surfs[[n]] = append(build_surfs, file$'BuildingSurface:Detailed'[[i]])
  names(build_surfs)[n] = names(file$'BuildingSurface:Detailed')[i]
  n = n + 1
}
file$'BuildingSurface:Detailed' = build_surfs


