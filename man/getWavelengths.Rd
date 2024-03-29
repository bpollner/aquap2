% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cl_classDefs.r, R/doc_methods.r
\name{getWavelengths,aquap_data-method}
\alias{getWavelengths,aquap_data-method}
\alias{getWavelengths,aquap_set-method}
\alias{getWavelengths}
\title{Get Wavelengths}
\usage{
\S4method{getWavelengths}{aquap_data}(object)

\S4method{getWavelengths}{aquap_set}(object)
}
\arguments{
\item{object}{An object of class 'aquap_data' as produced e.g. by 
\code{\link{gfd}}.}
}
\value{
A numeric vector containing the wavelengths of the NIR-data of the 
object.
}
\description{
Get a numeric vector containing the wavelengths of a given 
dataset.
}
\details{
The wavelengths are simply extracted from the column names of the 
NIR-data.
}
\examples{
\dontrun{
fd <- gfd()
wls <- getWavelengths(fd)
####
cu <- gdmm(fd)
wls <- getWavelengths(cu[[1]]) # extract the wavelengths from the dataset 
# of the first set in the cube
}
}
\seealso{
Other Extract Elements: 
\code{\link{getHeader}()},
\code{\link{getcd}()},
\code{\link{getcm}()},
\code{\link{siWl}()},
\code{\link{siWlg}()}
}
\concept{Extract Elements}
