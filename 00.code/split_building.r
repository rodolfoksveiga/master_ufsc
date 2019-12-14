# load libraries
library('data.table')

# split building

# curitiba
curitiba_base_v00 = as.data.frame(fread(paste0('/home/rodox/01.going_on/00.hive/00.hyp/00/00.c10/',
                                               'curitiba_hyp_c10_v00.csv')))
storeys = c('floor', 'inter', 'roof')

n = 1
m = 0
for (i in 1:length(storeys)) {
  storey = curitiba_base_v00[, grepl('Date/Time', colnames(curitiba_base_v00)) |
                               grepl('Environment:Site Outdoor Air',
                                     colnames(curitiba_base_v00)) |
                               grepl(paste0('F', n, '_'), colnames(curitiba_base_v00))]
  write.csv(storey, paste0('/home/rodox/01.going_on/00.hive/00.hyp/00/00.c10/0', m, '.',
                           storeys[i], '/curitiba_base_v00_', storeys[i], '.csv'))
  n = n + 2
  m = m + 1
}
rm(curitiba_base_v00, storeys, n, m)

# sao_paulo
sao_paulo_base_v00 = as.data.frame(fread(paste0('/home/rodox/01.going_on/00.hive/00.hyp/00/00.c10/',
                                               'sao_paulo_hyp_c10_v00.csv')))
storeys = c('floor', 'inter', 'roof')
n = 1
m = 0
for (i in 1:length(storeys)) {
  storey = sao_paulo_base_v00[, grepl('Date/Time', colnames(sao_paulo_base_v00)) |
                               grepl('Environment:Site Outdoor Air',
                                     colnames(sao_paulo_base_v00)) |
                               grepl(paste0('F', n, '_'), colnames(sao_paulo_base_v00))]
  write.csv(storey, paste0('/home/rodox/01.going_on/00.hive/00.hyp/00/00.c10/0', m, '.',
                           storeys[i], '/sao_paulo_base_v00_', storeys[i], '.csv'))
  n = n + 2
  m = m + 1
}
rm(sao_paulo_base_v00, storeys, n, m)

# rio_de_janeiro
rio_de_janeiro_base_v00 = as.data.frame(fread(paste0('/home/rodox/01.going_on/00.hive/00.hyp/00/',
                                                     '00.c10/rio_de_janeiro_hyp_c10_v00.csv')))
storeys = c('floor', 'inter', 'roof')
n = 1
m = 0
for (i in 1:length(storeys)) {
  storey = rio_de_janeiro_base_v00[, grepl('Date/Time', colnames(rio_de_janeiro_base_v00)) |
                               grepl('Environment:Site Outdoor Air',
                                     colnames(rio_de_janeiro_base_v00)) |
                               grepl(paste0('F', n, '_'), colnames(rio_de_janeiro_base_v00))]
  write.csv(storey, paste0('/home/rodox/01.going_on/00.hive/00.hyp/00/00.c10/0', m, '.',
                           storeys[i], '/rio_de_janeiro_base_v00_', storeys[i], '.csv'))
  n = n + 2
  m = m + 1
}
rm(rio_de_janeiro_base_v00, storeys, n, m)
