# load libraries and global environment ####
pkgs = c('jsonlite', 'purrr', 'parallel', 'reticulate', 'stringr', 'tibble')
invisible(lapply(pkgs, library, character.only = TRUE))
codes = c('build_case', 'edit_sample')
codes = paste0('./code/', codes, '.r')
invisible(lapply(codes, source))

# generate sample
py_run_file('./code/saltelli_sample.py')
# read and edit sample
sample_paths = c('train' = './model/sample_train.csv', 'test' = './model/sample_test.csv')
sample = lapply(sample_paths, EditSample, './source/epw/inmet_list.csv', './seed/')
# label cases
cases = 1:nrow(sample$train)
start = length(cases) + 1
end = length(cases) + nrow(sample$test)
cases = list(cases, start:end)
sample = mapply(function(x, y) add_column(x, 'case' = y, .before = 1),
                sample, cases, SIMPLIFY = FALSE)

# build cases
ApplyBuildCase = function(sample, complements, output_dir, n_cores) {
  mcmapply(BuildCase, sample$seed_path, sample$area, sample$ratio, sample$height, sample$azimuth,
           sample$shell_wall, sample$shell_roof, sample$abs_wall, sample$abs_roof, sample$wwr_liv,
           sample$wwr_dorm, sample$u_window, sample$shgc, sample$open_factor, sample$case,
           output_dir, MoreArgs = complements, mc.cores = n_cores)
}
complements = list(construction, fill, setup, geometry)
lapply(sample, ApplyBuildCase, complements, '/home/rodox/in_progress/model/', 4)

