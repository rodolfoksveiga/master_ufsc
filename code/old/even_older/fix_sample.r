# load libraries
library(stringr)

# functions ####
# bind_sample()
bind_samples = function(samples_paths) {
  # samples_paths - 
  
  samples = vector('list', length(samples_paths))
  sample = data.frame()
  for (i in 1:length(samples_paths)) {
    samples[[i]] = read.csv(samples_paths[i])
    samples[[i]][, 1] = NULL
    names(samples)[i] = str_sub(samples_paths[i], -6, -5)
    samples[[i]]$t = names(samples)[i]
    samples[[i]]$op = ifelse(samples[[i]]$t == '11' | samples[[i]]$t == '22' |
                               samples[[i]]$t == '31' | samples[[i]]$t == '32', NA, samples[[i]]$op)
    samples[[i]]$wwr_2 = ifelse(grepl('1', samples[[i]]$t), 0, samples[[i]]$wwr_2)
    samples[[i]]$app = ifelse(grepl('4', samples[[i]]$t), 0, samples[[i]]$app)
    
    sample = rbind(sample, samples[[i]])
  }
  
  return(sample)
}

# fix_sample()
  # adjust values of sample variables according to simulations
fix_sample = function(sample){
  # sample - 
  
  sample$azi = ifelse(sample$azi <= 90, 0,
                      ifelse(sample$azi <= 180, 90,
                             ifelse(sample$azi <= 270, 180,
                                    270)))
  sample$room = ifelse(sample$room <= 1, 'liv', 'dorm')
  sample$storey = ifelse(sample$storey <= 1, 'floor',
                         ifelse(sample$storey <= 2, 'inter',
                                'roof'))
  sample$wrap = ifelse(sample$wrap <= 1, 'c10',
                       ifelse(sample$wrap <= 2, 'tv',
                              'sf'))
  # sample$weather = ifelse(sample$weather <= 1, 'pr',
  #                         ifelse(sample$weather <= 2, 'rj',
  #                                'sp'))
  sample$app = ifelse(str_starts(sample$t, '1'),
                      ifelse(sample$app <= 1, 'e',
                             ifelse(sample$app <= 2, 'n',
                                    ifelse(sample$app <= 3, 'w',
                                           ifelse(sample$app <= 4, 'en',
                                                  ifelse(sample$app <= 5, 'nw', 'ew'))))),
                      ifelse(str_starts(sample$t, '2'),
                             ifelse(sample$app <= 1, 'e',
                                    ifelse(sample$app <= 2, 'n', 'en')),
                             ifelse(str_starts(sample$t, '3'),
                                    ifelse(sample$app <= 1, 'e',
                                           ifelse(sample$app <= 2, 'w', 'ew')), 'w')))
  sample$op = ifelse(sample$t == '11', 's',
                     ifelse(sample$t == '21',
                            ifelse(sample$op <= 1, 's', 'e'),
                            ifelse(sample$t == '22', 'se',
                                   ifelse(sample$t == '31', 's',
                                          ifelse(sample$t == '32', 'sn',
                                                 ifelse(sample$t == '41',
                                                        ifelse(sample$op <= 1, 's',
                                                               ifelse(sample$op <= 2, 'e', 'n')),
                                                               ifelse(sample$op <= 1, 'se',
                                                                      ifelse(sample$op <= 2, 'sn', 'en'))))))))
  
  return(sample)
}



# boundaries()
  # build boundary conditions to use in 'ex_hive_gen' code
boundaries = function(sample) {
  # sample -
  
  sample$bound_s = 'outdoors'
  sample$bound_e = ifelse(str_starts(sample$t, '2') | str_starts(sample$t, '4'), 'outdoors',
                          ifelse(grepl('e', sample$app),
                                 ifelse(sample$room == 'living', 'dorm', 'living'), 'adiabatic'))
  sample$bound_n = ifelse(str_starts(sample$t, '3') | str_starts(sample$t, '4'), 'outdoors',
                          ifelse(grepl('n', sample$app),
                                 ifelse(sample$room == 'living', 'dorm', 'living'), 'adiabatic'))
  sample$bound_w = ifelse(grepl('w', sample$app),
                          ifelse(sample$room == 'living', 'dorm', 'living'), 'adiabatic')
  
  return(sample)
}

# application ####
sample = bind_samples(c('/home/rodox/00.git/00.master_ufsc/05.sample/saltelli_sample_11.csv',
                        '/home/rodox/00.git/00.master_ufsc/05.sample/saltelli_sample_21.csv',
                        '/home/rodox/00.git/00.master_ufsc/05.sample/saltelli_sample_22.csv',
                        '/home/rodox/00.git/00.master_ufsc/05.sample/saltelli_sample_31.csv',
                        '/home/rodox/00.git/00.master_ufsc/05.sample/saltelli_sample_32.csv',
                        '/home/rodox/00.git/00.master_ufsc/05.sample/saltelli_sample_41.csv',
                        '/home/rodox/00.git/00.master_ufsc/05.sample/saltelli_sample_42.csv'))
sample = fix_sample(sample)
sample = boundaries(sample)
write.csv(sample, '/home/rodox/00.git/00.master_ufsc/05.sample/sample.csv')
