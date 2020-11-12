# load libraries and functions ####
library(jsonlite)
source('~/git/handy/rm_idf_order.r')

# base functions ####
# fix zone characteristics
FixZone = function(zone) {
  zone$z_origin = NULL
  return(zone)
}
# fix surface characteristics
FixSurface = function(surf) {
  if (surf$surface_type == 'Floor') {
    surf$construction_name = NULL
    surf$outside_boundary_condition = NULL
  } else if (surf$surface_type == 'Roof') {
    surf$construction_name = NULL
    surf$outside_boundary_condition = NULL
    surf$sun_exposure = NULL
    surf$wind_exposure = NULL
  }
  return(surf)
}
# fix fenestration characteristics
FixFenestration = function(fen) {
  fen$vertex_4_z_coordinate = fen$vertex_1_z_coordinate
  return(fen)
}

# main function ####
FixSeed = function(seed_path) {
  RmIDFOrder(seed_path)
  seed = read_json(seed_path)
  groups = c('Zone', 'BuildingSurface:Detailed', 'FenestrationSurface:Detailed')
  seed = seed[groups]
  seed$'Zone' = lapply(seed$'Zone', FixZone)
  seed$'BuildingSurface:Detailed' = lapply(seed$'BuildingSurface:Detailed', FixSurface)
  seed$'FenestrationSurface:Detailed' = lapply(seed$'FenestrationSurface:Detailed', FixFenestration)
  write_json(seed, seed_path, pretty = TRUE, auto_unbox = TRUE)
}

# application ####
FixSeed('~/rolante/consult/consult4_rv_90.epJSON')
