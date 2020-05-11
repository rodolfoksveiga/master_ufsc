# load libraries ####
library(jsonlite)

# base functions ####
ChangeObj = function(object, field, value) {
  object[[field]] = value
  return(object)
}

# main function ####
OpenDoors = function(model_path, single_floor,
                     doors = c('sw_liv_door_w', 'sw_liv_door_e', 'se_liv_door_w',
                               'se_liv_door_e', 'e_liv_door_s', 'e_liv_door_n',
                               'ne_liv_door_e', 'ne_liv_door_w', 'nw_liv_door_e',
                               'nw_liv_door_w', 'w_liv_door_n', 'w_liv_door_s')) {
  # model_path: 
  # doors: 
  # single_floor: 
  
  # load model
  model = read_json(model_path)
  # define afn objects
  if (single_floor) {
    afn_doors = paste0('afn_', doors)
  } else {
    afn_doors = paste0(rep(paste0('afn_f', 1:5), each = length(doors)), '_', doors)
  }
  # change ventilation control mode value to constant for all doors between rooms
  model$'AirflowNetwork:MultiZone:Surface'[afn_doors] =
    lapply(model$'AirflowNetwork:MultiZone:Surface'[afn_doors],
           ChangeObj, 'ventilation_control_mode', 'Constant')
  # write .epJSON file
  write_json(model, model_path, pretty = T, auto_unbox = T)
}

# application ####
model_paths = dir('~/git/master_ufsc/model/new', '.epJSON', full.names = T, recursive = T)
mapply(OpenDoors, model_paths, c(rep(F, 2*3), rep(T, 2*9)))
