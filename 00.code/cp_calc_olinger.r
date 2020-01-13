# cp_calc = function(ratio = 1, build_type = 'low_rise', azimuth = 0) {
  
# test
ratio = 0.5
build_type = 'low_rise'
azimuth = 0

  cp_values = list()
  # cp_hr_wall is related to each of the 12 angles that energyplus consider
  cp_hr_wall = list(c(0.60, 0.54, 0.23, -0.25, -0.61, -0.55,
                      -0.51, -0.55, -0.61, -0.25, 0.23, 0.54),
                    c(0.60, 0.48, 0.04, -0.56, -0.56, -0.42,
                      -0.37, -0.42, -0.56, -0.56, 0.04, 0.48),
                    c(0.60, 0.44, -0.26, -0.70, -0.53, -0.32,
                      -0.22, -0.32, -0.53, -0.70, -0.26, 0.44))
  
  surfs = c('afn_cp_surf_s', 'afn_cp_surf_e', 'afn_cp_surf_n', 'afn_cp_surf_w')
  
  for (i in 1:length(surfs)) {
    cp_values[[surfs[i]]] = list(
      'airflownetwork_multizone_windpressurecoefficientarray_name' = 'afn_cp',
      'idf_max_extensible_fields' = 0,
      'idf_max_fields' = 14
    )
    facade_ang = (azimuth + i*90) %% 360
    if (i == 2 | i == 4) {
      side_ratio = 1/ratio
    } else {
      side_ratio = ratio
    }
    side_ratio_fac = log(side_ratio)
    for (j in 1:12) {
      wind_ang = j*30
      inc_ang = abs(wind_ang - facade_ang)
      if (inc_ang > 180) {
        inc_ang = 360 - inc_ang
      }
      i_ang = as.integer((inc_ang/30) + 1)
      del_ang = inc_ang %% 30
      wt_ang = 1.0 - del_ang / 30
    
      if (build_type == 'low_rise') { # low rise buildings
        inc_rad = inc_ang*pi/180
        cp = 0.6 * log(1.248 - 0.703*sin(inc_rad/2) - 1.175*sin(inc_rad)^2 +
                        0.131*sin(2*inc_rad*log(ratio))^3 + 0.769*cos(inc_rad/2) +
                         0.07*log(ratio)^2 * sin(inc_rad/2)^2 + 0.717*cos(inc_rad/2)^2)
      } else { #high rise buildings
        sr = min(max(side_ratio, 0.25), 4)
        if (sr >= 0.25 & sr < 1) {
          isr = 1
          wtsr = (1 - sr) / 0.75
        } else {
          isr = 2
          wtsr = (4 - sr) / 3
        }
        cp = wtsr * (wt_ang * cp_hr_wall[[isr]][i_ang] +
              (1 - wt_ang) * cp_hr_wall[[isr]][i_ang + 1]) +
               (1 - wtsr)*(wt_ang * cp_hr_wall[[isr + 1]][i_ang] +
                (1 - wt_ang)*cp_hr_wall[[isr + 1]][i_ang + 1])
      }
      cp_values[[surfs[i]]][[paste0('wind_pressure_coefficient_value_', as.character(j))]] = cp
    }
  }
  
  return(cp_values)
}