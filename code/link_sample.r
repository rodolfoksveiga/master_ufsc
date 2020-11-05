# main function ####
LinkSample = function(sample, seeds_dir, models_dir) {
  seed_paths = paste0(seeds_dir, sample$typo, '.json')
  construction = list(ref17 = c(1, 1), ref8 = c(1, 2), tm20 = c(3, 1), tv = c(4, 3), sf = c(5, 4))
  index = sapply(sample$shell, grep, names(construction))
  shells = index %>% lapply(function(x, y) c(shell_wall = y[[x]][1], shell_roof = y[[x]][2]),
                            construction) %>% bind_rows()
  model_paths = paste0(models_dir, sample$simp, '_', sample$typo, '_', sample$shell, '.epJSON')
  sample = add_column(sample, seed_path = seed_paths, shell_wall = shells$shell_wall,
                      shell_roof = shells$shell_roof, model_path = model_paths, .before = 1)
  sample$boundary = ifelse(sample$simp == 0, 'surface', 'adiabatic')
  return(sample)
}