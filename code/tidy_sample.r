# main function ####
TidySample = function(sample_path, seeds_dir, models_dir, epws_dir, inmet) {
  sample = read.csv(sample_path, stringsAsFactors = FALSE)
  quals = c('seed', 'shell_wall', 'shell_roof', 'blind', 'facade', 'epw')
  sample[, quals] = floor(sample[, quals])
  sample$balcony = ifelse(sample$balcony <= 0, 0, sample$balcony)
  balcony = ifelse(sample$balcony > 0, 'b', '')
  sample$facade = ifelse(sample$facade == 1, 1, 2)
  seed_paths = paste0(seeds_dir, 'seed', sample$seed, 'c', sample$facade, balcony, '.json')
  epw_paths = paste0(epws_dir, inmet$arquivo_climatico[sample$epw], '.epw')
  sample = add_column(sample, seed_path = seed_paths, epw_path = epw_paths, .before = 1)
  sample = unique(sample)
  cases = str_pad(1:nrow(sample), 5, 'left', 0)
  sample = add_column(sample, prefix = paste0('case', cases), .before = 2)
  model_paths = paste0(models_dir, sample$prefix, '.epJSON')
  sample = add_column(sample, model_path = model_paths, .before = 2)
  return(sample)
}