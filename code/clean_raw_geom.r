# load libraries and environment ####
pkgs = c('jsonlite', 'purrr')
lapply(pkgs, library, character.only = TRUE)
source('~/git/handy/rm_idf_order.r')

# base functions ####
# edit building surface characteristics
EditBuildSurf = function(surf) {
  type = surf$surface_type
  discard = 'view_factor_to_ground'
  if (type == 'Floor') {
    discard = c(discard, 'construction_name', 'outside_boundary_condition')
  } else if (type == 'Roof') {
    discard = c(discard, 'construction_name', 'outside_boundary_condition',
                'sun_exposure', 'wind_exposure')
  } else {
    surf$construction_name = ifelse(surf$outside_boundary_condition == 'Outdoors',
                                    'ext_wall', 'int_wall')
  }
  index = !names(surf) %in% discard
  surf = surf[index]
  return(surf)
}
# edit fenestrations (add the 4th z vertex)
EditFen = function(fen) {
  type = fen$surface_type
  if (type == 'Door') {
    const = 'door_3cm'
  } else {
    const = 'simple_window'
  }
  fen$construction_name = const
  fen$vertex_4_z_coordinate = fen$vertex_1_z_coordinate
  return(fen)
}
# remove z_origin from zones
EditZone = function(zone) {
  zone$z_origin = NULL
  return(zone)
}

# main function ####
CleanRawGeom = function(geom_path) {
  geom = read_json(geom_path)
  # clean original json file (leave only the objects below)
  objects = c('Zone', 'BuildingSurface:Detailed', 'FenestrationSurface:Detailed')
  geom = geom[objects]
  # edit zones
  geom$Zone = lapply(geom$'Zone', EditZone)
  # edit building surfaces
  geom$'BuildingSurface:Detailed' = lapply(geom$'BuildingSurface:Detailed', EditBuildSurf)
  # edit fenestration surfaces
  geom$'FenestrationSurface:Detailed' = lapply(geom$'FenestrationSurface:Detailed', EditFen)
  # write new json
  write_json(geom, geom_path, pretty = TRUE, auto_unbox = TRUE)
}

# application ####
seed_path = '/home/rodox/git/nbr/auto_gen/seed_uni.json'
RmIDFOrder(seed_path)
CleanRawGeom(seed_path)
