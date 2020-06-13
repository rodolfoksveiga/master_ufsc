# load libraries and global environment ####
# load libraries
pkgs = c('data.table', 'dplyr', 'stringr')
lapply(pkgs, library, character.only = TRUE)

# base functions ####
# calculate differences between base and simplified simplifications
CalcDiff = function(df_base, df_simp, label, rel) {
  # df_base: data frame with full simulation results
  # df_simp: data frame with single zone results
  # label: 
  # rel: 
  df = df_base[, !label] - df_simp[, !label]
  if (rel) {
    df = df/abs(df_simp[, !label])*100
  }
  df = cbind(df, df_simp[, label])
  return(df)
}
# read and bind files
ReadBindFiles = function(files_paths) {
  df = files_paths %>%
    lapply(read.csv, stringsAsFactors = FALSE) %>%
    bind_rows()
  df$sim = str_pad(df$sim, 2, side = 'left', pad = 0)
  df$level = as.character(df$level)
  return(df)
}

# main function ####
ProcessDiff = function(sim, rel, input_dir, output_dir) {
  sim = str_pad(sim, 2, side = 'left', pad = 0)
  pattern = paste0(sim, '.*\\.csv')
  files_paths = dir(input_dir, pattern, full.names = TRUE)
  dfs_list = lapply(list('00' = str_replace(files_paths, '(?<=\\/\\/)..', '00'),
                         '01' = files_paths), ReadBindFiles)
  label = sapply(dfs_list$'00', is.character)
  df = CalcDiff(dfs_list[[1]], dfs_list[[2]], label, rel)
  # write data frame
  type = ifelse(rel, 'rel', 'abs')
  output_path = paste0(output_dir, sim, '_diff_', type, '.csv')
  write.csv(df, file = output_path, row.names = FALSE)
  # clean cache
  rm(dfs_list, df)
  gc()
}

# application ####