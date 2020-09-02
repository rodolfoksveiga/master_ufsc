JoinSamples = function(saltelli_path, sample_path) {
  sample = read.csv(sample_path)
  qual_vars = c('seed', 'shell_wall', 'shell_roof', 'blind', 'facade', 'epw')
  saltelli = saltelli_path %>%
    read.csv() %>%
    mutate_at(qual_vars, floor) %>%
    mutate(balcony = ifelse(balcony <= 0, 0, balcony),
           facade = ifelse(facade == 1, 1, 2)) %>%
    slice(rep(1:n(), each = 3)) %>%
    mutate(storey = rep(1:3, times = n()/3)) %>%
    relocate(storey, .after = height)
  write.csv(saltelli, saltelli_path, row.names = FALSE)
  saltelli = saltelli_path %>%
    read.csv() %>%
    left_join(sample, by = colnames(saltelli))
  write.csv(saltelli, saltelli_path, row.names = FALSE)
}