# base functions ####
# calculate percentage of hours feeling uncomfortable (ph)
CalcPH = function (lim, op_temp, occup, mean_temp) {
  lim_sup = DefLimSup(mean_temp)
  occup = occup > 0
  if (lim == 'sup') {
    ph = sum(occup & op_temp > lim_sup)/sum(occup)*100
  } else {
    if (mean_temp < 25) {
      ph = sum(occup & op_temp < 18)/sum(occup)*100
    } else {
      ph = 0
    }
  }
  return(ph)
}
# define superior limit
DefLimSup = function(x) ifelse(x < 25, 26, ifelse(x < 27, 28, 30))
# calc phft
CalcPHFT = function(op_temp, occup, mean_temp) {
  phs = sapply(c(ph_sup = 'sup', ph_inf = 'inf'), CalcPH, op_temp, occup, mean_temp)
  phft = 100 - sum(phs)
  return(phft)
}

# main function ####
CalcTarget = function(input_path, weather, occup, inmet) {
  df = input_path %>% fread() %>% as.data.frame()
  colnames(df) = df %>% colnames() %>% str_remove(':.*') %>% str_to_lower()
  df = df[str_detect(colnames(df), '(?<=_)(liv|dorm)')]
  index = match(weather, inmet$arquivo_climatico)
  zones = df %>% colnames() %>% str_extract('(?<=_)(liv|dorm)')
  target = mapply(CalcPHFT, df, occup[zones], MoreArgs = list(inmet[index, 'tbsm']))
  target = sapply(1:3, function(x, y) mean(y[grepl(paste0('^f', x), names(target))]), target)
  target = data.frame(storey = 1:3, targ = target)
  rm(df)
  gc()
  return(target)
}
# add targets to sample data frame
ApplyCalcTarget = function(sample, input_dir, occup, inmet) {
  input_paths = dir(input_dir, '\\.csv', full.names = TRUE)
  weathers = str_remove(basename(sample$epw_path), '\\.epw')
  targets = mapply(CalcTarget, input_paths, weathers,
                   MoreArgs = list(occup, inmet), SIMPLIFY = FALSE)
  targets = bind_rows(targets)
  sample = sample %>% slice(rep(1:n(), each = 3)) %>% cbind(targets) %>%
    select(seed, area, ratio, height, storey, azimuth, shell_wall, abs_wall, shell_roof, abs_roof,
           wwr_liv, wwr_dorm, u_window, shgc, open_factor, blind, balcony, facade, epw, targ)
  return(sample)
}