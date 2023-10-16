## Steadiness Index

#'
#' @author Xavier Rotllan-Puig
#' @title steadiness
#' @description steadiness derives the Steadiness Index from a land productivity variable
#' @details The Steadiness Index is based on the combination of two metrics calculated per pixel: (1) the slope derived
#' from a linear regression of the different years of the time series and (2) the net change on the same period. It
#' results in a 4-class SpatRaster object ranging from (1) strong negative to (4) strong positive ecosystem dynamics.
#' See Ivits et al. (2013) for further explanations.
#'
#' Values = 0 in the final map indicates that there is a scarcity of data in the productivity variable
#' (i.e. only 1 year with data), so that the indicator cannot be calculated
#'
#' @rawNamespace import(terra, except = na.omit)
#' @param obj2process SpatRaster object (or its file name). If time series, each layer is one year
#' @param cores2use Numeric. Number of cores to use for parallelization. Optional. Default is 1 (no parallelization)
#' @param filename Character. Output filename. Optional
#' @return SpatRaster object
#' @name steadiness
#' @references Ivits, E., M. Cherlet, W. Mehl, and S. Sommer. 2013. “Ecosystem Functional Units Characterized by
#' Satellite Observed Phenology and Productivity Gradients: A Case Study for Europe.” Ecological Indicators 27: 17–28.
#' \doi{10.1016/j.ecolind.2012.11.010}
#' @export
#' @examples
#' \donttest{
#' sb <- terra::rast(paste0(system.file(package='LPDynR'), "/extdata/sb_cat.tif"))
#' steadiness(obj2process = sb)
#'}
#'


steadiness <- function(obj2process = NULL,
                       cores2use = 1,
                       filename = ""){

  ## Reading in data (Standing Biomass)
  if(is.null(obj2process)) stop("Please provide an object of classe SpatRaster (or a file names to read in from)")

  if(is.character(obj2process)){
    obj2process <- terra::rast(obj2process)
  }else if(!class(obj2process) %in% c("SpatRaster")){
    stop("Please provide an object of classe SpatRaster (or a file name to read in from)")
  }

  ## Fitting a linear regression and getting the slope

  yrs <- 1:nlyr(obj2process)
  #slope_rstr <- app(obj2process, fun = function(yrs, ff) ff(yrs), cores = cores2use, ff = slp_lm)
  slope_rstr <- app(obj2process, fun = slp_lm, cores = cores2use, yrs = yrs)


  ## Computing net change: MTID (Multi Temporal Image Differencing)

  mtid_rstr <- app(obj2process, fun = mtid_function, cores = cores2use)



  ## Calculating steadiness classes
  SteadInd_rstr <- mtid_rstr
  names(SteadInd_rstr) <- "SteadInd"

  slope_rstr_vals <- values(slope_rstr, mat = FALSE)
  mtid_rstr_vals <- values(mtid_rstr, mat = FALSE)
  SteadInd_rstr_vals <- rep(NA, length(mtid_rstr_vals))


  SteadInd_rstr_vals[slope_rstr_vals < 0 & mtid_rstr_vals > 0] <- 1     # strong negative ecosystem dynamics
  SteadInd_rstr_vals[slope_rstr_vals < 0 & mtid_rstr_vals < 0] <- 2     # moderate negative ecoystem dynamics
  SteadInd_rstr_vals[slope_rstr_vals > 0 & mtid_rstr_vals < 0] <- 3     # moderate positive ecosystem dynamics
  SteadInd_rstr_vals[slope_rstr_vals > 0 & mtid_rstr_vals > 0] <- 4     # strong positive ecosystem dynamics
  SteadInd_rstr_vals[slope_rstr_vals == 0 | mtid_rstr_vals == 0] <- 0   # stable ecosystem dynamics

  SteadInd_rstr <- setValues(SteadInd_rstr, SteadInd_rstr_vals)


  ## Saving results
  #rm(list = c("yrs", "years"), envir = globalenv())
  #rm(list = c("yrs"), envir = globalenv())
  if (filename != "") writeRaster(SteadInd_rstr, filename = filename, overwrite = TRUE)
  return(SteadInd_rstr)

}
