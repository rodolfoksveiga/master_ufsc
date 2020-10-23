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
# extract string in between other two patterns
ExtrStrBetween = function(string, pattern, before = '_', after = '_') {
  string = str_extract(string, paste0('(?<=', before, ')', pattern, '(?=', after, ')'))
  return(string)
}
# generate full report
GenReport = function(df, tag, geometry, occup, out_temp, unit = 'kwh') {
  div = ifelse(unit == 'kwh', 3600000, 1000)
  label = LabelZone(tag)
  typo = label[['typo']]
  position = label[['position']]
  room = label[['room']]
  area = geometry[[typo]][[position]][[room]][[1]]
  cols = colnames(df)
  cols = cols[grepl('hg|hl', cols)]
  report = mapply(SumCol, df[, cols], cols)/(div*area)
  report['afn_hg'] = report['afn_hg'] - report['afn_hl']
  sides = c('s', 'e', 'n', 'w')
  patterns = c(paste0('wall_', sides), 'int', 'ext', 'window', 'door')
  names(patterns) = paste0(c(paste0('walls_', c(sides, 'int', 'ext')), 'windows', 'doors'), '_hg')
  surfs = sapply(patterns, function(x, y) sum(y[grepl(x, names(y))]), report)
  air = c('air_change' = mean(df$air_change))
  top = c('top_max' = max(df$zone_top), 'top_min' = min(df$zone_top))
  room = str_remove(label[['room']], '[12]$')
  phs = sapply(c('ph_sup' = 'sup', 'ph_inf' = 'inf'), CalcPH, df$zone_top,
               occup[, room], mean(out_temp[, label['weather']]))
  phft = c('phft' = 100 - sum(phs))
  report = report[c('il_hg', 'afn_hg', 'floor_hg', 'roof_hg')]
  report = report %>% c(surfs, air, top, phs, phft) %>% round(1) %>% c(label)
  return(report)
}
# label zone according to simlification, typology, shell, position, orientation, room,
  # room index and weather
LabelZone = function(tag) {
  sim = ExtrStrBetween(tag, '[0-9]')
  typo = ExtrStrBetween(tag, '(h|l)')
  shell = ExtrStrBetween(tag, '(ref|tm|tv|sf)')
  room = str_extract(tag, '(?<=_)(liv|dorm[12])')
  storey = ExtrStrBetween(tag, '[1-9]?', '_f')
  hab = ExtrStrBetween(tag, '.*', paste0('f', storey, '_'), paste0('_', room))
  position = hab %>% str_detect('^c') %>% ifelse('corner', 'middle')
  orient =  str_sub(hab, -2, -1)
  weather = str_extract(tag, paste0('.*(?=_', sim, ')'))
  label = c('sim' = sim, 'typo' = typo, 'shell' = shell, 'storey' = storey,
            'position' = position, 'orient' = orient, 'room' = room, 'weather' = weather)
  return(label)
}
# rename data frame columns
RnmCols = function(df, tag) {
  cols = df %>% colnames() %>% tolower()
  len = length(cols)
  cols[c(1, (len - 3):len)] = c('il_hg', 'zone_top', 'afn_hg', 'afn_hl', 'air_change')
  room = tag %>% str_extract('(?<=f[0-9]_).*')
  cond = ifelse(grepl('temperature', cols), 'temp', 'hg')
  colnames(df) = cols %>% str_remove(paste0(room, '_')) %>%
    str_replace('(?<=:).*', cond) %>% str_replace(':', '_')
  return(df)
}
# sume an output column considering the heat flow direction
SumCol = function(val, var) {
  mult = ifelse(str_detect(var, '(?<!afn|il)_hg'), -1, 1)
  val = sum(val)*mult
  return(val)
}

# main functions ####
# apply ProcessOutput()
ApplyProcessOut = function(input_dir, output_dir, geometry, occup, out_temp) {
  file_paths = dir(input_dir, '\\.csv', full.names = TRUE)
  sapply(file_paths, ProcessOutput, output_dir, geometry, occup, out_temp)
}
# process output and generate a summarized table
ProcessOutput = function(file_path, output_dir, geometry, occup, out_temp) {
  pattern = file_path %>% basename() %>% str_remove('\\.csv$')
  file_path %>% fread() %>% as.data.frame() %>% RnmCols(pattern) %>%
    GenReport(pattern, geometry, occup, out_temp) %>% t() %>%
    write.csv(paste0(output_dir, pattern, '.csv'), row.names = FALSE)
}
