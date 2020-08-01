# main function ####
TidySample = function(sample_paths, seeds_dir, models_dir, epws_dir, inmet) {
  sample = sample_paths %>%
    lapply(read.csv, stringsAsFactors = FALSE)
  sample = mapply(function(x, y) add_column(x, set = y),
                  sample, c('train', 'test'), SIMPLIFY = FALSE)
  sample = bind_rows(sample)
  quals = c('seed', 'shell_wall', 'shell_roof', 'epw')
  sample[, quals] = floor(sample[, quals])
  sample = add_column(sample, seed_path = sample$seed, epw_path = sample$epw, .before = 1)
  seed_paths = paste0(seeds_dir, 'seed', 1:3, '.json')
  vars = c('seed_path', 'epw_path')
  # walls = c('concreto_10cm', 'concreto_20cm', 'tijolo_macico_10cm',
  #           'tijolo_macico_20cm', 'tijolo_vazado', 'steel_frame', 'concreto_la_vidro')
  # roofs = c('fibrocimento_concreto', 'fibrocimento_isolamento_zb8_concreto',
  #           'fibrocimento_la_vidro_concreto', 'fibrocimento_la_vidro_gesso')
  sample[, vars] = mapply(function(x, y) y[x], sample[, vars],
                          list(seed_paths, inmet$arquivo_climatico))
  sample$epw_path = paste0(epws_dir, sample$epw_path, '.epw')
  sample = unique(sample)
  cases = str_pad(1:nrow(sample), 5, 'left', 0)
  sample = add_column(sample, prefix = paste0('case', cases), .before = 1)
  model_paths = paste0(models_dir, sample$prefix, '.epJSON')
  sample = add_column(sample, model_path = model_paths, .before = 1)
  return(sample)
}