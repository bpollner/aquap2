#'  @param spl.var NULL or character vector. If NULL, no splitting of the 
#'  dataset will be performed. Provide a character vector with the column names 
#'  of class variables to split the dataset along these variables.
#'  @param spl.wl NULL or character vector. If NULL, all in the dataset 
#'  available wavelengths will be used. Provide a character vector in the format 
#'  "wlFrom-to-wlTo" (e.g. c("1000-to-2000", "1300-to-1600", ...)) 
#'  to use all previously defined splits in these wavelengths.
#'  @param spl.do.smo Logical. If smoothing of the NIR data should be performed. The 
#'  values for the smoothing operation itself are defined in the settings.
#'  @param spl.smo.raw Logical. If, should smoothing be performed, the raw 
#'  unsmoothed data will be used as well in addition to the smoothed data.
#'  @param spl.do.noise Logical. If artifical noise should be added to the dataset.
#'  @param spl.noise.raw  If, should the noise-test be performed, the raw data 
#'  will be used as well in addition to the noise-data.
