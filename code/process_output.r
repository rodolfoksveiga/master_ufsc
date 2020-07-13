# load global environment ####
load('~/git/master/seed/occup_temp.rdata')
load('~/git/master/seed/areas.rds')

# base functions ####
# calculate percentage of hours feeling uncomfortable (ph)
CalcPH = function (lim, op_temp, occup, mean_temp) {
  # lim: 'sup' (ph superior) or 'inf' (ph inferior)
  # op_temp: operative temperature vector
  # occup: occupancy vector
  # mean_temp: dry bulb temperature annual mean
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
  # string: 
  string = str_extract(string, paste0('(?<=', before, ')', pattern,
                                      '(?=', after, ')'))
  return(string)
}
# label zone according to simlification, typology, shell, position, orientation, room,
  # room index and weather
LabelZone = function(tag) {
  # tag: 
  sim = ExtrStrBetween(tag, '[0-9]+')
  typo = ExtrStrBetween(tag, '(h|linear)')
  shell = ExtrStrBetween(tag, '(ref|tm|tv|sf)')
  room = str_extract(tag, '(?<=_)(liv|dorm[12])')
  level = ExtrStrBetween(tag, '[1-9]?', '_f')
  hab = ExtrStrBetween(tag, '.*', paste0('f', level, '_'), paste0('_', room))
  position = hab %>% str_detect('^c') %>% ifelse('corner', 'middle')
  orient =  str_sub(hab, -2, -1)
  weather = str_extract(tag, paste0('.*(?=_', sim, ')'))
  label = c('sim' = sim, 'typo' = typo, 'shell' = shell, 'level' = level,
            'position' = position, 'orient' = orient, 'room' = room, 'weather' = weather)
  return(label)
}
# load files
LoadFiles = function(pattern, input_dir) {
  # pattern: 
  # input_dir: 
  files_paths = dir(input_dir, paste0('*', pattern, '.*csv'), full.names = TRUE)
  dfs_list = lapply(files_paths, function(x) as.data.frame(fread(x)))
  names(dfs_list) = str_remove(basename(files_paths), '\\.csv$')
  return(dfs_list)
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
  # val: 
  # var: 
  mult = ifelse(str_detect(var, '(?<!afn|il)_hg'), -1, 1)
  val = sum(val)*mult
  return(val)
}

# main functions ####
# generate full report
GenReport = function(df, tag, unit = 'kwh') {
  div = ifelse(unit == 'kwh', 3600000, 1000)
  label = LabelZone(tag)
  area = filter(df_areas, typo == label[['typo']], room == label[['room']],
                position == label[['position']])$area
  cols = colnames(df)
  cols = cols[grepl('hg|hl', cols)]
  report = mapply(SumCol, df[, cols], cols)/(div*area)
  report['afn_hg'] = report['afn_hg'] - report['afn_hl']
  sides = c('s', 'e', 'n', 'w')
  patterns = c(paste0('wall_', sides), 'int', 'ext', 'window', 'door')
  names(patterns) = paste0(c(paste0('walls_', c(sides, 'int', 'ext')),
                             'windows', 'doors'), '_hg')
  surfs = sapply(patterns, function(x, y) sum(y[grepl(x, names(y))]), report)
  air = c('air_change' = mean(df$air_change))
  top = c('top_max' = max(df$zone_top), 'top_min' = min(df$zone_top))
  room = str_remove(label[['room']], '[12]$')
  phs = sapply(c('ph_sup' = 'sup', 'ph_inf' = 'inf'), CalcPH, df$zone_top,
               occup[, room], mean(out_temp[, label['weather']]))
  phft = c('phft' = 100 - sum(phs))
  report = report[c('il_hg', 'afn_hg', 'floor_hg', 'roof_hg')]
  report = report %>% c(surfs, air, top, phs, phft) %>%
    round(1) %>% c(label)
  return(report)
}
# process output and generate a summarized table
ProcessOutput = function(input_dir, sim, typo, shell, level, output_dir) {
  # load files
  sim = str_pad(sim, 2, side = 'left', pad = 0)
  pattern = paste0(sim, '_', typo, '_', shell, '_f', level)
  dfs_list = LoadFiles(pattern, input_dir)
  RmConduction = function(df) {
    index = df %>% colnames() %>% str_detect('Conduction')
    df = df[!index]
    return(df)
  }
  dfs_list = lapply(dfs_list, RmConduction)
  # rename columns
  dfs_list = mapply(RnmCols, dfs_list, names(dfs_list), SIMPLIFY = FALSE)
  # generates a data frame report
  df = mapply(GenReport, dfs_list, names(dfs_list)) %>%
    as.data.frame() %>% t() %>% as.data.frame(row.names = FALSE)
  # write report
  output_path = paste0(output_dir, pattern, '.csv')
  write.csv(df, file = output_path, row.names = FALSE)
  # clean cache
  rm(dfs_list, df)
  gc()
}