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

# main functions ####
CalcTarget = function(input_path, storey, weather, period, occup, inmet) {
  df = read.csv(input_path, nrows = 1)
  cols = df %>% colnames() %>% str_which(paste0('^F', storey, '_(LIV|DORM)'))
  df = input_path %>%
    fread(nrows = 52560, select = cols, colClasses = 'numeric') %>%
    as.data.frame()
  index = match(weather, inmet$arquivo_climatico)
  zones = df %>% colnames() %>% str_extract('(?<=_)(LIV|DORM)') %>% str_to_lower()
  if (period == 'month') {
    dfs = split(df, as.factor(occup$month))
    occups = split(occup[zones], as.factor(occup$month))
    target = mapply(function(x, w, y, z) mapply(y, x, w, MoreArgs = z), dfs, occups,
                    MoreArgs = list(CalcPHFT, list(inmet[index, 'tbsm'])), SIMPLIFY = FALSE) %>%
      sapply(mean)
  } else {
    target = mapply(CalcPHFT, df, occup[zones], MoreArgs = list(inmet[index, 'tbsm'])) %>% mean()
  }
  rm(df)
  gc()
  return(target)
}
# add targets to sample data frame
ApplyCalcTarget = function(sample, input_dir, occup, inmet, period = 'year') {
  input_paths = dir(input_dir, '\\.csv', full.names = TRUE)
  weathers = sample %>% pull(epw_path) %>% basename() %>% str_remove('\\.epw')
  targets = mapply(CalcTarget, input_paths, sample$storey,
                   weathers, MoreArgs = list(period, occup, inmet))
  cols = c('seed_path', 'model_path', 'prefix', 'epw_path')
  sample = sample %>%
    select(-all_of(cols))
  if (period == 'month') {
    sample = sample %>%
      slice(rep(1:n(), each = 12)) %>%
      mutate(targ = as.vector(targets), month = rep(1:12, length(input_paths)))
  } else {
    sample = mutate(sample, targ = targets)
  }
  return(sample)
}