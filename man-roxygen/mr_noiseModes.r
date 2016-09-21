#' @section Modes for noise calculation:
#' Via the parameter \code{noi_addMode} in the settings file it can be specified 
#' which one of the available methods to calculate and add noise to the actual 
#' dataset should be used. The following values for \code{noi_addMode} are 
#' possible: 
#' \describe{
#' \item{<%=pv_noiseAddModes[1]%>}{Factory default. Standard deviation based, 
#' sampling from \strong{normal} distribution. The noise distribution is described 
#' by \code{Noise_Mean  Noise_sd} whithin each single wavelength of the noise 
#' dataset. If noise should be added now to an actual dataset, for each single 
#' observation and there for each single wavelength the noise-value to be added 
#' to the actual spectral value can be described by 
#' \code{noiseValue = sample(rnorm(noi_sampleSize, mean=Noise_Mean, sd=Noise_sd),1)}. 
#' Additionally, all values outside the range of the noise-data in this specific 
#' wavelength get clipped. (\code{noi_sampleSize} is a parameter in the 
#' settings file.)}
#' \item{<%=pv_noiseAddModes[2]%>}{Standard deviation based, 
#' sampling from \strong{uniform} distribution. The noise distribution is 
#' described by \code{Noise_Mean  Noise_sd} whithin each single wavelength of 
#' the noise dataset. If noise should be added now to an actual dataset, for each 
#' single observation and there for each single wavelength the noise-value to 
#' be added to the actual spectral value can be described by \code{noiseValue = 
#' sample(runif(noi_sampleSize, min=Noise_Mean-Noise_sd, max=Noise_Mean-Noise_sd),1)}. 
#' (\code{noi_sampleSize} is a parameter in the settings file.)}
#' \item{<%=pv_noiseAddModes[3]%>}{The noise distribution simply represents the 
#' range of absorbance values within each single wavelength over all observations 
#' of the noise dataset. If noise should be added now to a dataset, for each single 
#' observation and there for each single wavelength a random process is selecting 
#' one of the two values from the corresponding wavelength in the noise 
#' distribution to be added to the original NIR data. In other words, for each 
#' single wavelength the original NIR-data absorbance values get increased or 
#' reduced by the maximum of (positive or negative) noise occuring at this 
#' specific wavelength.}
#' \item{<%=pv_noiseAddModes[4]%>}{For this mode, no noise-data file is required. 
#' For every observation in the actual dataset and there for every single 
#' wavelength, a static noise normal-distribution is calculated using the 
#' parameters \code{noi_sampleSize} and \code{noi_staticValue} from the settings 
#' file: From the resulting pool of values then one is sampled and used to add to 
#' the spectral value of the actual dataset. 
#' (\code{noiseValue = sample(rnorm(noi_sampleSize, mean=0, 
#' sd=noi_staticValue),1)})}
#' }
