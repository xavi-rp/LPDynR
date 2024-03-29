% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/04_state_change.R
\name{state_change}
\alias{state_change}
\title{state_change}
\usage{
state_change(
  obj2process = NULL,
  yearsBaseline = 3,
  changeNclass = 1,
  cores2use = 1,
  filename = ""
)
}
\arguments{
\item{obj2process}{SpatRaster object (or its file name). If time series, each layer is one year}

\item{yearsBaseline}{Numeric. Number of years to be averaged at the beginning and end of the time series. Optional. Default is 3}

\item{changeNclass}{Numeric. Number of classes changed for classification. Optional. Default is 1}

\item{cores2use}{Numeric. Number of cores to use for parallelization. Optional. Default is 1 (no parallelization)}

\item{filename}{Character. Output filename. Optional}
}
\value{
SpatRaster object
}
\description{
state_change derives land productivity state change between the beginning and the end of the time series
on study, resulting in a 3-class SpatRaster object with (1) no change, (2) changed between 1 and x classes or
(3) changed more than x classes, where x can be defined by the user (default is 1)
}
\details{
state_change uses the average of 'yearsBaseline' number of years at the beginning and the end of the time series
}
\examples{
\donttest{
sb <- terra::rast(paste0(system.file(package='LPDynR'), "/extdata/sb_cat.tif"))
state_change(obj2process = sb,
             yearsBaseline = 3,
             changeNclass = 1,
             cores2use = 2)
}
}
\author{
Xavier Rotllan-Puig
}
