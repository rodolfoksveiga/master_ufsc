# main functions ####
EditSample = function(sample_path, inmet_path, seed_dir) {
  sample = read.csv(sample_path)
  inmet = read.csv(inmet_path, stringsAsFactors = FALSE)
  quals = c('seed_path', 'shell_wall', 'shell_roof', 'weather')
  seeds = paste0(seed_dir, 'seed', 1:3, '.json')
  walls = c('concreto_10cm', 'concreto_20cm', 'tijolo_macico_10cm',
            'tijolo_macico_20cm', 'tijolo_vazado', 'steel_frame', 'concreto_la_vidro')
  roofs = c('fibrocimento_concreto', 'fibrocimento_isolamentozb8_concreto',
            'fibrocimento_la_vidro_concreto', 'fibrocimento_la_vidro_gesso')
  sample[, quals] = floor(sample[, quals])
  sample[, quals] = mapply(function(x, y) y[x], sample[, quals],
                           list(seeds, walls, roofs, inmet$arquivo_climatico))
  sample = unique(sample)
  return(sample)
}