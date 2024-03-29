% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cl_classDefs.r, R/mod_modifyDatasets.r
\name{mergeDatasets,aquap_data,aquap_data,missing-method}
\alias{mergeDatasets,aquap_data,aquap_data,missing-method}
\alias{mergeDatasets,aquap_data,aquap_data,aquap_mergeLabels-method}
\alias{mergeDatasets,list,missing,missing-method}
\alias{mergeDatasets,list,missing,aquap_mergeLabels-method}
\alias{mergeDatasets,list,aquap_mergeLabels,missing-method}
\alias{mergeDatasets}
\title{Merge Datasets}
\usage{
\S4method{mergeDatasets}{aquap_data,aquap_data,missing}(
  ds1,
  ds2,
  mergeLabels = NULL,
  noMatchH = getstn()$gen_merge_noMatchH,
  noMatchW = getstn()$gen_merge_noMatchW,
  resaTo = "best",
  resaMethod = getstn()$gen_resample_method,
  dol = getstn()$gen_merge_detectOutliers
)

\S4method{mergeDatasets}{aquap_data,aquap_data,aquap_mergeLabels}(
  ds1,
  ds2,
  mergeLabels,
  noMatchH = getstn()$gen_merge_noMatchH,
  noMatchW = getstn()$gen_merge_noMatchW,
  resaTo = "best",
  resaMethod = getstn()$gen_resample_method,
  dol = getstn()$gen_merge_detectOutliers
)

\S4method{mergeDatasets}{list,missing,missing}(
  ds1,
  ds2 = NULL,
  mergeLabels = NULL,
  noMatchH = getstn()$gen_merge_noMatchH,
  noMatchW = getstn()$gen_merge_noMatchW,
  resaTo = "best",
  resaMethod = getstn()$gen_resample_method,
  dol = getstn()$gen_merge_detectOutliers
)

\S4method{mergeDatasets}{list,missing,aquap_mergeLabels}(
  ds1,
  ds2 = NULL,
  mergeLabels,
  noMatchH = getstn()$gen_merge_noMatchH,
  noMatchW = getstn()$gen_merge_noMatchW,
  resaTo = "best",
  resaMethod = getstn()$gen_resample_method,
  dol = getstn()$gen_merge_detectOutliers
)

\S4method{mergeDatasets}{list,aquap_mergeLabels,missing}(
  ds1,
  ds2,
  mergeLabels = NULL,
  noMatchH = getstn()$gen_merge_noMatchH,
  noMatchW = getstn()$gen_merge_noMatchW,
  resaTo = "best",
  resaMethod = getstn()$gen_resample_method,
  dol = getstn()$gen_merge_detectOutliers
)
}
\arguments{
\item{ds1}{An object of class 'aquap_data' or a list containing any number of 
objects of class 'aquap_data'}

\item{ds2}{An object of class 'aquap_data', can be missing.}

\item{mergeLabels}{An object of class 'aquap_mergeLabels' as generated by 
\code{\link{generateMergeLabels}}, can be missing.}

\item{noMatchH}{Character length one. Defines what should happen in the case of 
non-matching header structures, i.e. the column names of the headers of the 
datasets to me merged can not be overlapped. The default value is defined in 
the settings.r file (\code{gen_merge_noMatchH}). Possible values are: 
\describe{
\item{ask}{The non-matching header-columns in each dataset are 
displayed, and the user is asked interactively what to do, with the three options 
below as possible options.}
\item{delete}{Non-matching header columns are automatically deleted.}
\item{fill}{Each column name not existing in all of the datasets 
to be merged is added to those datasets where it does not exist. The data is filled 
in with 'NAs'.}
\item{stop}{In case of non-overlapping header structures, the merging 
process is stopped, with possibly a message being displayed.}
}}

\item{noMatchW}{Character length one. Defines what should happen in the case of 
non-matching wavelengths, i.e. the wavelengths in the datasets to be merged are 
not identical. The default value is defined in the settings.r file 
(\code{gen_merge_noMatchH}). Possible values are:
\describe{
\item{ask}{The non-matching wavelenghts in each dataset are displayed, and the 
user is asked interactively what to do, with the five options below as possible 
options.}
\item{cut}{All wavelengths outside a range common to all datasets will be deleted. 
In other words, for some datasets the 'outsiders', i.e. the wavelengths outside 
of that common range, will be deleted.}
\item{fill}{Missing wavelengths will be filled in with 'NAs'. In other words, the 
wavelengths of all datasets will be expanded to encompass the overal mimimum and 
the overal maximum of the wavelengths of the datasets.}
\item{resacut}{Same as 'cut', but datasets are resampled to have all the same delta
wavelength.}
\item{resafill}{Same as 'fill', but datasets are resampled to have all the same delta
wavelength.}
\item{stop}{In case of non-matching wavelengths, the merging process is stopped, 
with possibly a message being displayed.}
}}

\item{resaTo}{Target wavelength for a (possible) resampling process (which uses the 
function \code{\link{do_resampleNIR}}. Can be one of the following:
\describe{
\item{"best"}{If left at the default 'best' the best target wavelength will be 
automatically determined. The best target wavelength is a solution where as few 
as possible datasets get resampled.}
\item{Character length one}{The name of the dataset (if a named list is provided) 
containing the target wavelength.}
\item{Integer length one}{The number of the dataset (e.g. in the provided list) 
containing the target wavelength.}
\item{Numeric Vector}{Provide a numeric vector as target wavelengths to which all 
datasets will be resampled. The vector will be checked for plausibility, i.e. if 
it is in range of the provided datasets etc. For 'filling in' (option 'fill' or 
'resafill' in argument 'noMatchW') only numeric vectors x with 
\code{length(unique(diff(x))) == 1} are accepted.}
}}

\item{resaMethod}{Character length one. Which of the resampling methods should be 
used. Factory-fresh defaults to 'cubic'; the default can be changed in the settings.r file 
parameter \code{gen_resample_method}. See \code{\link{do_resampleNIR}} and 
\code{\link[pracma]{interp1}}. 'linear' is much faster than e.g. 'spline' or 
'cubic', but the quality of the resampling is not as good.}

\item{dol}{Logical length one. If outliers should be detected based on the scope 
of the new, merged dataset. The default value is defined in the settings file at
\code{gen_merge_detectOutliers}.}
}
\value{
An object of class 'aquap_data', with all the single datasets merged
together.
}
\description{
Merge together two or more datasets, and possibly add class- or 
numerical variables to each dataset via the 'mergeLabels' object.
}
\details{
The resulting dataset is void of metadata (object@metadata) and analysis 
procedures. The order of column names in each header in a dataset is irrelevant, 
e.g. a header with the column names 'AA, BB, CC' does overlap with a header with 
the column names 'AA, CC, BB'.
}
\seealso{
generateMergeLabels

Other dataset modification functions: 
\code{\link{[,aquap_data-method}},
\code{\link{calculateVariable}()},
\code{\link{combineVariable}()},
\code{\link{generateMergeLabels,aquap_data,aquap_data,character,character-method}}
}
\concept{dataset modification functions}
