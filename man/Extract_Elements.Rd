% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cl_classDefs.r, R/doc_methods.r
\name{getHeader}
\alias{getHeader}
\alias{getHeader,aquap_data-method}
\alias{getHeader,aquap_set-method}
\alias{Extract_Elements}
\title{Extract Elements}
\usage{
getHeader(object)

\S4method{getHeader}{aquap_data}(object)

\S4method{getHeader}{aquap_set}(object)
}
\arguments{
\item{object}{Either a dataset as produced by \code{\link{gfd}} or a set (see 
examples).}
}
\description{
Extract single elements of a dataset or of a set, i.e. a single 
list element of a cube.
}
\details{
Function \code{getHeader} will return only the header of a dataset 
without the NIR-data.
}
\examples{
\dontrun{
fd <- gfd()
cube <- gdmm(fd)
header <- getHeader(fd)
header1 <- getHeader(cube[[1]]) # gets the header from the first dataset in 
# the cube.
}
}
\seealso{
Other Extract Elements: 
\code{\link{getWavelengths,aquap_data-method}},
\code{\link{getcd}()},
\code{\link{getcm}()},
\code{\link{siWl}()},
\code{\link{siWlg}()}
}
\concept{Extract Elements}
