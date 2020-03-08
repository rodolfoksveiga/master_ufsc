# main function ####
# reduction()
  # takes raw reports as input and calculates the difference between tv/sf and the reference (c10)
reduction = function(report_path, cond, write_results = T, output_dir) {
  # report - 
  # write_results - 
  # output_dir - 
  
  # calculates the reduction
  reports = lapply(report_path, read.csv)
  if (cond == 'afn') {
    red = reports[['real']][, 1:18] - reports[['ref']][, 1:18]
    red = cbind(red, reports[['real']][, 19:26])
  } else {
    red = reports[['real']][, 1:16] - reports[['ref']][, 1:16]
    red = cbind(red, reports[['real']][, 17:24])
  }
  
  # write result files
  if (write_results == T) {
    write.csv(red, paste0(output_dir, '/', sub('raw', 'red', basename(report_path[['real']]))),
              row.names = F)
  } else {
    return(red)
  }
}

# application ####
typos = c('hyp')
simps = c('00', '01', '02', '03', '04', '05', '06', '07')
wraps = c('tv', 'sf')
storeys = c('floor', 'inter', 'roof')
conds = c('afn', 'hvac')
for (typo in typos) {
  for (simp in simps) {
    for (wrap in wraps) {
      for (storey in storeys) {
        for (cond in conds) {
          reduction(list('real' = paste0('/home/rodox/00.git/00.master_ufsc/03.result/',
                                         typo, '_v', simp, '_', wrap, '_', storey, '_', cond,
                                         '_raw.csv'),
                         'ref' = paste0('/home/rodox/00.git/00.master_ufsc/03.result/',
                                        typo, '_v', simp, '_c10_', storey, '_', cond, '_raw.csv')),
                    cond, write_results = T,
                    '/home/rodox/00.git/00.master_ufsc/03.result/')
        }
      }
    }
  }
}
