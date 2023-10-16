## Base Line Calculation (Standing Biomass)

#'
#' @author Xavier Rotllan-Puig
#' @title baseline_lev
#' @description baseline_lev() derives land productivity at the beginning of the time series on study, resulting in a 3-class SpatRaster
#' object with (1) low, (2) medium and (3) high productivity
#' @details baseline_lev() uses the proportion of drylands over the total land ('drylandProp') to classify the level of productivity into
#' low level. UNPD declares that 40 percent of the World’s land resources are drylands (Middleton et al., 2011) and, therefore, 40 percent
#' of pixels at the global level can be classified as low productivity land. This assumption is the default, but it should be adjusted for
#' local and regional studies. In addition, baseline_lev() classifies by default 10 percent of pixels as high level of land productivity
#' and the rest (100 - ('drylandProp' + 10)) as medium level. Proportion of pixels classified as 'high' can be also modified by passing the
#' argument 'highprodProp'
#' @rawNamespace import(terra, except = na.omit)
#' @param obj2process SpatRaster object (or its file name). If time series, each layer is one year
#' @param yearsBaseline Numeric. Number of years to be averaged and used as baseline. Optional. Default is 3
#' @param drylandProp Numeric. Proportion of drylands over total land, either expressed as a fraction of unity or percentage. Optional. Default is 0.4
#' @param highprodProp Numeric. Proportion of land classified as 'highly productive' over total land, either expressed as a fraction of unity or percentage. Optional. Default is 0.1
#' @param cores2use Numeric. Number of cores to use for parallelization. Optional. Default is 1 (no parallelization)
#' @param filename Character. Output filename. Optional
#' @return SpatRaster object
#' @name baseline_lev
#' @references Middleton, N., L. Stringer, A. Goudie, and D. Thomas. 2011. “The Forgotten Billion. MDG Achievement in the Drylands.” New York,
#' NY, 10017, USA: United Nations Development Programme.
#' @export
#' @examples
#' \donttest{
#' sb <- terra::rast(paste0(system.file(package='LPDynR'), "/extdata/sb_cat.tif"))
#' baseline_lev(obj2process = sb,
#'              yearsBaseline = 3,
#'              drylandProp = 0.4,
#'              cores2use = 2)
#' }
#'


baseline_lev <- function(obj2process = NULL,
                         yearsBaseline = 3,
                         drylandProp = 0.4,
                         highprodProp = 0.1,
                         cores2use = 1,
                         filename = ""){

  ## Reading in data (Standing Biomass)
  if(is.null(obj2process)) stop("Please provide an object of classe Raster* (or a file names to read in from)")

  if(is.character(obj2process)){
    obj2process <- terra::rast(obj2process)
  }else if(!class(obj2process) %in% c("SpatRaster")){
    stop("Please provide an object of classe SpatRaster (or a file name to read in from)")
  }

  if(yearsBaseline > nlyr(obj2process)) yearsBaseline <- nlyr(obj2process)


  ## Averaging first years
  yrs <- 1:yearsBaseline
  obj2process_avg13 <- app(obj2process, fun = mean_years_function, cores = cores2use, yrs = yrs)


  #some checks...
  rning_tsts <- "y"
  rning_tsts <- "n"
  if(rning_tsts == "y"){
    chk_cell <- spatSample(obj2process_avg13, size = 1, cells = TRUE)[1]
    chk_avg <- round(mean(as.numeric(obj2process[as.numeric(chk_cell)][1:yearsBaseline])), 0) == round(as.numeric(obj2process_avg13[as.numeric(chk_cell)]), 0) # has to be TRUE
    if(chk_avg != TRUE) stop("Something wrong in the averaging process")
  }


  ## Classification of averaged pixels
  #The assumption of 40% of World's land area is dryland and, therefore, 40% of pixels is "low"
  #is fine at Global level. It must be adjusted for local studies!!!
  pix_categs <- terra::global(obj2process_avg13, fun = quantile, probs = seq(0, 1, 0.1), na.rm = TRUE)
  pix_categs01 <- unlist(pix_categs[- length(pix_categs)])
  pix_categs1 <- as.data.frame(t(pix_categs[-1]))
  pix_categs1$from <- unlist(pix_categs01)

  if(drylandProp > 100){
    stop("Proportion of drylands ('drylandProp') must be < 100%")
  }else if(drylandProp > 1){
    drylandProp <- round((drylandProp / 10), 0)
  }else if(drylandProp < 1){
    drylandProp <- round((drylandProp * 10), 0)
  }else if(drylandProp == 1 | drylandProp == 100){
    drylandProp <- 10
  }
  if(highprodProp > 100){
    stop("Proportion of land classified as highgly productive ('highprodProp') must be < 100%")
  }else if(highprodProp > 1){
    highprodProp <- round((highprodProp / 10), 0)
  }else if(highprodProp < 1){
    highprodProp <- round((highprodProp * 10), 0)
  }else if(highprodProp == 1 | highprodProp == 100){
    highprodProp <- 10
  }
  if(highprodProp + drylandProp > 10) stop("'highprodProp' + 'drylandProp' cannot be > 100%")

  otherLand <- 10 - highprodProp - drylandProp

  pix_categs1$becomes <- c(rep(1, drylandProp), rep(2, otherLand), rep(3, highprodProp))
  pix_categs1 <- pix_categs1[, c(2, 1, 3)]
  names(pix_categs1)[2] <- "to"
  pix_categs1[1, 1] <- pix_categs1[1, 1] - 1

  obj2process_3class <- classify(obj2process_avg13, rcl = pix_categs1, filename = '', include.lowest = FALSE, right = TRUE)

  ## Saving results
  if (filename != "") writeRaster(obj2process_3class, filename = filename, overwrite = TRUE)
  return(obj2process_3class)

}

