## State Change (Standing Biomass)

#'
#' @author Xavier Rotllan-Puig
#' @title state_change
#' @description state_change derives land productivity state change between the beginning and the end of the time series
#' on study, resulting in a 3-class RasterLayer object with (1) no change, (2) changed between 1 and x classes or
#' (3) changed more than x classes, where x can be defined by the user (default is 1)
#' @details state_change uses the average of 'yearsBaseline' number of years at the beginning and the end of the time series
#' @import raster parallel
#' @param obj2process Raster* object (or its file name). If time series, each layer is one year
#' @param yearsBaseline Numeric. Number of years to be averaged at the beginning and end of the time series. Optional. Default is 3
#' @param changeNclass Numeric. Number of classes changed for classification. Optional. Default is 1
#' @param cores2use Numeric. Number of cores to use for parallelization. Optional. Default is 1 (no parallelization)
#' @param filename Character. Output filename. Optional
#' @return RasterLayer object
#' @name state_change
#' @export
#' @examples
#' \donttest{
#' sb <- raster::brick(paste0(system.file(package='LPDynR'), "/extdata/sb_cat.tif"))
#' state_change(obj2process = sb,
#'              yearsBaseline = 3,
#'              changeNclass = 1,
#'              cores2use = 2)
#' }


state_change <- function(obj2process = NULL,
                         yearsBaseline = 3,
                         changeNclass = 1,
                         cores2use = 1,
                         filename = ""){

  ## Reading in data (Standing Biomass)
  if(is.null(obj2process)) stop("Please provide an object of classe Raster* (or a file names to read in from)")

  if(is.character(obj2process)){
    obj2process <- stack(obj2process)
  }else if(!class(obj2process) %in% c("RasterLayer", "RasterStack", "RasterBrick")){
    stop("Please provide an object of classe Raster* (or a file name to read in from)")
  }

  if(yearsBaseline > nlayers(obj2process)) yearsBaseline <- nlayers(obj2process)

  ## Averaging first years
  beginCluster(cores2use)
  yrs <- c()
  yrs <<- 1:yearsBaseline
  obj2process_avg13 <- clusterR(obj2process, calc, args = list(fun = mean_years_function), export = "yrs")
  endCluster()

  ## Classification of averaged pixels
  pix_categs <- raster::quantile(obj2process_avg13, probs =  seq(0, 1, 0.1), names = TRUE)
  pix_categs01 <- c(pix_categs[- length(pix_categs)])
  pix_categs1 <- as.data.frame(pix_categs[-1])
  pix_categs1$from <- pix_categs01
  pix_categs1$becomes <- 1:(length(pix_categs) - 1)
  pix_categs1 <- pix_categs1[, c(2, 1, 3)]
  names(pix_categs1)[2] <- "to"
  pix_categs1[1, 1] <- pix_categs1[1, 1] - 1

  obj2process_10class_beg <- reclassify(obj2process_avg13, rcl = pix_categs1, filename = '', include.lowest = FALSE, right = TRUE)



  ## Averaging last years
  beginCluster(cores2use)
  num_yrs <- dim(obj2process)[3]
  yrs <- c()
  yrs <<- ((num_yrs) - (yearsBaseline - 1)):num_yrs
  obj2process_avgLast3 <- clusterR(obj2process, calc, args = list(fun = mean_years_function), export = "yrs")
  endCluster()


  ## Reclassifying average-last-3 into 10 categories
  # using same thresholds of average-first-3 to be consistent when calculating category shifts
  pix_categs1[1, 1] <- 0
  pix_categs1[nrow(pix_categs1), 2] <- max(getValues(obj2process_avgLast3), na.rm = TRUE) + 1
  obj2process_10class_end <- reclassify(obj2process_avgLast3, rcl = pix_categs1, filename='', include.lowest = FALSE, right = TRUE)


  ## Calculating Standing Biomass change (difference begin - end; reclassifying into 3 categories)
  obj2process_10class_dif <- obj2process_10class_beg - obj2process_10class_end

  pix_categs2 <- as.data.frame(matrix(nrow = 5, ncol = 0))
  pix_categs2$from    <- c(-10, - changeNclass - 0.5, - 0.5, 0.5, changeNclass + 0.5)
  pix_categs2$to      <- c(- changeNclass - 1, - 1, 0, changeNclass, 9)
  pix_categs2$becomes <- c( 3, 2, 1, 2, 3)
  pix_categs2
  obj2process_3classChange <- reclassify(obj2process_10class_dif, rcl = pix_categs2, filename='', include.lowest = TRUE, right = TRUE)


  ## Saving results
  if (filename != "") writeRaster(obj2process_3classChange, filename = filename, overwrite = TRUE)
  return(obj2process_3classChange)

}
