cost <- function(cost_function, crit_slope) {
  
    cfs <- c("tobler", "tobler offpath", "davey", 'rees', "irmischer-clarke male", "irmischer-clarke offpath male", "irmischer-clarke female", "irmischer-clarke offpath female", "modified tobler", 'garmy', 'kondo-saino', "wheeled transport", "herzog", "llobera-sluckin", "naismith", "minetti", "campbell","campbell 2019 1","campbell 2019 5" ,"campbell 2019 10","campbell 2019 15","campbell 2019 20","campbell 2019 25","campbell 2019 30","campbell 2019 35","campbell 2019 40","campbell 2019 45","campbell 2019 50","campbell 2019 55","campbell 2019 60","campbell 2019 65","campbell 2019 70","campbell 2019 75","campbell 2019 80","campbell 2019 85","campbell 2019 90","campbell 2019 95","campbell 2019 99", "sullivan 167","sullivan 5", "sullivan 833")
      
    # mathematical slope to degrees
    slope2deg <- function(slope) { (atan(slope) * 180/pi)}
      
    # degrees to radians
    deg2rad <- function(deg) {(deg * pi) / (180)}
    
    tobler_cf <- function(x) {
      # Tobler Hiking Function measured in km/h. Divide by 3.6 to turn into m/s
      # 3.6 converts from km/h to m/s
      (6 * exp(-3.5 * abs(x + 0.05))) / 3.6
    }
    
    tobler_mod_cf <- function(x) {
      ((6 * exp(-3.5 * abs(x + 0.05))) * 0.6) / 3.6
    }
    
    davey_cf <- function(x, y = 1.40, z = 2.8) { 
      # requires mathematical slope to be in radians. Necessary to convert mathematical slope to degrees and then to radians
      x_deg <- slope2deg(x)
      x_rad <- deg2rad(x_deg)
      
      (y * exp(-z*abs(x_rad)))
    }
    
    rees_cf <- function(x) { 
      (1 / (0.75 + 0.09 * abs(x) + 14.6 * (abs(x))^2))
    }
    
    ic_m_cf <- function(x) {
      (0.11 + exp(-(abs(x) * 100 + 5)^2/(2 * 30^2)))
    }
    
    ic_o_m_cf <- function(x) {
      (0.11 + 0.67 * exp(-(abs(x) * 100 + 2)^2/(2 * 30^2)))
    }
    
    ic_f_cf <- function(x) {
      (0.95 * (0.11 + exp(-(abs(x) * 100 + 5)^2/(2 * 30^2))))
    }
    
    ic_o_f_cf <- function(x) {
      0.95 * (0.11 + 0.67 * exp(-(abs(x) * 100 + 2)^2/(2 * 30^2)))
    }
    
    mod_tobler_cf <- function(x) {
      (4.8 * exp(-5.3 * abs((x * 0.7) + 0.03)))
    }
    
    garmy_cf <- function(x) { 
      (4 * exp(-0.008 * (slope2deg(x)^2)))
    }
    
    ks_cf <- function(x) { 
      ifelse(abs(x) >= -0.07, (5.1 * exp(-2.25 * abs(x + 0.07))), (5.1 * exp(-1.5 * abs(x + 0.07))))
    }
    
    wt_cf <- function(x) {
      (1/(1 + ((abs(x) * 100)/crit_slope)^2))
    }
    
    h_cf <- function(x) {
      
      (1/((1337.8 * (x)^6) + (278.19 * (x)^5) - (517.39 * (x)^4) - (78.199 * (x)^3) + (93.419 * (x)^2) + (19.825 * (x)) + 1.64))
      
    }
    
    ls_cf <- function(x) {
      (1/(2.635 + (17.37 * (x)) + (42.37 * (x)^2) - (21.43 * (x)^3) + (14.93 * (x)^4)))
    }
    
    n_cf <- function(x) {
      
      x_deg <- slope2deg(x)
      x_rad <- deg2rad(x_deg)
      
      ifelse(x_deg > 0, (1 / (0.72 + 6 * tan(x_rad))), ifelse(x_deg <= -12, (1 / (0.72 - 2 * tan(x_rad))), ifelse(x_deg > -12 & x_deg <= -5, (1 / (0.72 + 2 * tan(x_rad))), 1.40)))
    }
    
    m_cf <- function(x) {
      (1/((280.5 * abs(x)^5) - (58.7 * abs(x)^4) - (76.8 * abs(x)^3) + (51.9 * abs(x)^2) + (19.6 * abs(x)) + 2.5))
    }
    
    c_cf <- function(x) { 
      x_deg <- slope2deg(x)
      
      1.662 - (5.191 * 10^-3) * x_deg - (1.127*10^-3) * x_deg^2
    }
    
    cf2_001_cf <- function(x) { 
      (21.816*(1/(pi*12.273*(1+((slope2deg(x)--2.1)/12.273)^2)))+0.263)+-0.00193*slope2deg(x)
    }
    
    cf2_005_cf <- function(x) { 
      (36.813*(1/(pi*14.041*(1+((slope2deg(x)--1.527)/14.041)^2)))+0.32)+-0.00273*slope2deg(x)
    }
    
    cf2_010_cf <- function(x) { 
      (38.892*(1/(pi*13.328*(1+((slope2deg(x)--1.568)/13.328)^2)))+0.404)+-0.00323*slope2deg(x)
    }
    
    cf2_015_cf <- function(x) { 
      (38.231*(1/(pi*11.847*(1+((slope2deg(x)--1.626)/11.847)^2)))+0.481)+-0.00356*slope2deg(x)
    }
    
    cf2_020_cf <- function(x) { 
      (36.905*(1/(pi*10.154*(1+((slope2deg(x)--1.71)/10.154)^2)))+0.557)+-0.00389*slope2deg(x)
    }
    
    cf2_025_cf <- function(x) { 
      (37.111*(1/(pi*8.827*(1+((slope2deg(x)--1.822)/8.827)^2)))+0.616)+-0.00402*slope2deg(x)
    }
    
    cf2_030_cf <- function(x) { 
      (39.995*(1/(pi*8.412*(1+((slope2deg(x)--1.858)/8.412)^2)))+0.645)+-0.0043*slope2deg(x)
    }
    
    cf2_035_cf <- function(x) { 
      (44.852*(1/(pi*8.584*(1+((slope2deg(x)--1.891)/8.584)^2)))+0.649)+-0.00443*slope2deg(x)
    }
    
    cf2_040_cf <- function(x) { 
      (50.34*(1/(pi*8.96*(1+((slope2deg(x)--1.958)/8.96)^2)))+0.649)+-0.00457*slope2deg(x)
    }
    
    cf2_045_cf <- function(x) { 
      (56.172*(1/(pi*9.402*(1+((slope2deg(x)--2.05)/9.402)^2)))+0.646)+-0.0046*slope2deg(x)
    }
    
    cf2_050_cf <- function(x) { 
      (63.66*(1/(pi*10.064*(1+((slope2deg(x)--2.171)/10.064)^2)))+0.628)+-0.00463*slope2deg(x)
    }
    
    cf2_055_cf <- function(x) { 
      (71.572*(1/(pi*10.712*(1+((slope2deg(x)--2.317)/10.712)^2)))+0.608)+-0.00451*slope2deg(x)
    }
    
    cf2_060_cf <- function(x) { 
      (79.287*(1/(pi*11.311*(1+((slope2deg(x)--2.459)/11.311)^2)))+0.599)+-0.00461*slope2deg(x)
    }
    
    cf2_065_cf <- function(x) { 
      (89.143*(1/(pi*12.089*(1+((slope2deg(x)--2.647)/12.089)^2)))+0.576)+-0.00465*slope2deg(x)
    }
    
    cf2_070_cf <- function(x) { 
      (98.697*(1/(pi*12.784*(1+((slope2deg(x)--2.823)/12.784)^2)))+0.566)+-0.00493*slope2deg(x)
    }
    
    cf2_075_cf <- function(x) { 
      (113.655*(1/(pi*13.888*(1+((slope2deg(x)--3.067)/13.888)^2)))+0.518)+-0.00488*slope2deg(x)
    }
    
    cf2_080_cf <- function(x) { 
      (134.409*(1/(pi*15.395*(1+((slope2deg(x)--3.371)/15.395)^2)))+0.443)+-0.00472*slope2deg(x)
    }
    
    cf2_085_cf <- function(x) { 
      (159.027*(1/(pi*17.137*(1+((slope2deg(x)--3.661)/17.137)^2)))+0.385)+-0.00534*slope2deg(x)
    }
    
    cf2_090_cf <- function(x) { 
      (138.875*(1/(pi*16.653*(1+((slope2deg(x)--3.06)/16.653)^2)))+0.823)+-0.01386*slope2deg(x)
    }
    
    cf2_095_cf <- function(x) { 
      (138.04*(1/(pi*17.033*(1+((slope2deg(x)--3.485)/17.033)^2)))+1.179)+-0.01252*slope2deg(x)
    }
    
    cf2_099_cf <- function(x) { 
      (123.515*(1/(pi*13.903*(1+((slope2deg(x)--4)/13.903)^2)))+1.961)+-0.01081*slope2deg(x)
    }
    
    s_0167_cf <- function(x) { 
      (92.6594*(1/(pi*25.8255*(1+((slope2deg(x)--3.3717)/25.8255)^2)))+-0.1624)+0.0019*slope2deg(x)
    }
    
    s_05_cf <- function(x) { 
      (77.6346*(1/(pi*20.9482*(1+((slope2deg(x)--2.8292)/20.9482)^2)))+-0.2228)+0.0004*slope2deg(x)
    }
    
    s_0833_cf <- function(x) { 
      (65.3577*(1/(pi*19.4024*(1+((slope2deg(x)--2.2893)/19.4024)^2)))+-0.6226)+0.0020*slope2deg(x)
    }
  
    if (is.function(cost_function)) {
      cf <- cost_function
    } else {
      cf <- switch(cost_function,
                   "tobler" = tobler_cf,
                   "tobler offpath" = tobler_mod_cf,
                   "davey" = davey_cf,
                   "rees" = rees_cf,
                   "irmischer-clarke male" = ic_m_cf,
                   "irmischer-clarke offpath male" = ic_o_m_cf,
                   "irmischer-clarke female" = ic_f_cf,
                   "irmischer-clarke offpath female" = ic_o_f_cf,
                   "modified tobler" = mod_tobler_cf,
                   "garmy" = garmy_cf,
                   "kondo-saino" = ks_cf,
                   "wheeled transport" = wt_cf,
                   "herzog" = h_cf,
                   "llobera-sluckin" = ls_cf,
                   "naismith" = n_cf,
                   "minetti" = m_cf,
                   "campbell" = c_cf,
                   "campbell 2019 1" = cf2_001_cf,
                   "campbell 2019 5" = cf2_005_cf,
                   "campbell 2019 10" = cf2_010_cf,
                   "campbell 2019 15" = cf2_015_cf,
                   "campbell 2019 20" = cf2_020_cf,
                   "campbell 2019 25" = cf2_025_cf,
                   "campbell 2019 30" = cf2_030_cf,
                   "campbell 2019 35" = cf2_035_cf,
                   "campbell 2019 40" = cf2_040_cf,
                   "campbell 2019 45" = cf2_045_cf,
                   "campbell 2019 50" = cf2_050_cf,
                   "campbell 2019 55" = cf2_055_cf,
                   "campbell 2019 60" = cf2_060_cf,
                   "campbell 2019 65" = cf2_065_cf,
                   "campbell 2019 70" = cf2_070_cf,
                   "campbell 2019 75" = cf2_075_cf,
                   "campbell 2019 80" = cf2_080_cf,
                   "campbell 2019 85" = cf2_085_cf,
                   "campbell 2019 90" = cf2_090_cf,
                   "campbell 2019 95" = cf2_095_cf,
                   "campbell 2019 99" = cf2_099_cf,
                   "sullivan 167" = s_0167_cf,
                   "sullivan 5" = s_05_cf,
                   "sullivan 833" = s_0833_cf)
    }
    
    return(cf)
}
