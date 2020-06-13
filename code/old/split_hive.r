# load libraries ####
pkgs = c('data.table', 'parallel', 'stringr')
lapply(pkgs, library, character.only = T)

# base functions ####
# read, process, select columns and write the data frame
ProcessHive = function(input_path, zone, output_dir) {
  # load data
  df = as.data.frame(fread(input_path))
  # select the columns related to zone name
  col_names = c('Date/Time', 'Environment:Site Outdoor Air Drybulb', zone)
  col_names = str_flatten(col_names, collapse = '|')
  df = df[, grepl(col_names, colnames(df))]
  colnames(df) = str_replace(colnames(df), zone, 'HIVE_C')
  # write file
  file_name = paste0(output_dir, basename(input_path))
  write.csv(df, file_name)
}

# main function ####
# splits the hive simulation's outputs into only the hive core
SplitHive = function(input_dir, pattern = NULL, output_dir) {
  # input_dir - directory where the full floor simulation's outputs are located
  # pattern - pattern to be found inside input_dir
  # zone - name of the zone to be splitted
  # output_dir - directory where the splitted files will be saved
  
  # match pattern in the directory
  files_names = dir(input_dir, paste0('*', pattern, '.*csv'), full.names = T)
  zones = ifelse(grepl('floor', files_names), 'HIVE_BC',
                 ifelse(grepl('inter', files_names), 'HIVE_MC', 'HIVE_TC'))
  # process data frame
  mcmapply(ProcessHive, files_names, zones, output_dir, mc.cores = detectCores())
}

# application ####
SplitHive(input_dir = '~/in_progress/split/08/',
          output_dir = '~/in_progress/split/08/hive/')