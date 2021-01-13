## Steadiness Index

#'
#' @author Xavier Rotllan-Puig
#' @title steadiness
#' @description steadiness derives the Steadiness Index from a land productivity variable
#' @details The Steadiness Index is based on the combination of two metrics calculated per pixel: (1) the slope derived
#' from a linear regression of the different years of the time series and (2) the net change on the same period. It
#' results in a 4-class RasterLayer object ranging from (1) strong negative to (4) strong positive ecosystem dynamics.
#' See Ivits et al. (2013) for further explanations.
#'
#' Values = 0 in the final map indicates that there is a scarcity of data in the productivity variable
#' (i.e. only 1 year with data), so that the indicator cannot be calculated
#'
#' @import raster parallel
#' @param obj2process Raster* object (or its file name). If time series, each layer is one year
#' @param cores2use Numeric. Number of cores to use for parallelization. Optional. Default is 1 (no parallelization)
#' @param filename Character. Output filename. Optional
#' @return RasterLayer object
#' @name steadiness
#' @references Ivits, E., M. Cherlet, W. Mehl, and S. Sommer. 2013. “Ecosystem Functional Units Characterized by
#' Satellite Observed Phenology and Productivity Gradients: A Case Study for Europe.” Ecological Indicators 27: 17–28.
#' \doi{10.1016/j.ecolind.2012.11.010}
#' @export
#' @examples
#' \donttest{
#' sb <- raster::brick(paste0(system.file(package='LPDynR'), "/extdata/sb_cat.tif"))
#' steadiness(obj2process = sb)
#'}
#'


steadiness <- function(obj2process = NULL,
                       cores2use = 1,
                       filename = ""){

  ## Reading in data (Standing Biomass)
  if(is.null(obj2process)) stop("Please provide an object of classe Raster* (or a file names to read in from)")

  if(is.character(obj2process)){
    obj2process <- stack(obj2process)
  }else if(!class(obj2process) %in% c("RasterLayer", "RasterStack", "RasterBrick")){
    stop("Please provide an object of classe Raster* (or a file name to read in from)")
  }

  #obj2process <- stack(paste0("/Users/xavi_rp/Documents/D6_LPD/phenolo_data_Cat", "/mi_clean_Cat.tif"))

  ## Fitting a linear regression and getting the slope
  beginCluster(cores2use)
  yrs <- c()
  yrs <<- 1:nlayers(obj2process)
  slope_rstr <- clusterR(obj2process, calc, args = list(fun = slp_lm), export = "yrs")
  endCluster()


  ## Computing net change: MTID (Multi Temporal Image Differencing)
  beginCluster(cores2use)
  #years <<- length(yrs)
  mtid_rstr <- clusterR(obj2process, calc, args = list(fun = mtid_function))#, export = "years")
  endCluster()


  ## Calculating steadiness classes
  SteadInd_rstr <- raster(mtid_rstr)
  SteadInd_rstr[slope_rstr < 0 & mtid_rstr > 0] <- 1   # strong negative ecosystem dynamics
  SteadInd_rstr[slope_rstr < 0 & mtid_rstr < 0] <- 2   # moderate negative ecoystem dynamics
  SteadInd_rstr[slope_rstr > 0 & mtid_rstr < 0] <- 3   # moderate positive ecosystem dynamics
  SteadInd_rstr[slope_rstr > 0 & mtid_rstr > 0] <- 4   # strong positive ecosystem dynamics
  SteadInd_rstr[slope_rstr == 0 | mtid_rstr == 0] <- 0   # stable ecosystem dynamics


  ## Saving results
  #rm(list = c("yrs", "years"), envir = globalenv())
  #rm(list = c("yrs"), envir = globalenv())
  if (filename != "") writeRaster(SteadInd_rstr, filename = filename, overwrite = TRUE)
  return(SteadInd_rstr)

}
