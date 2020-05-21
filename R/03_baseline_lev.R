## Base Line Calculation (Standing Biomass)

#'
#' @author Xavier Rotllan-Puig
#' @title baseline_lev
#' @description baseline_lev() derives land productivity at the beginning of the time series on study, resulting in a 3-class RasterLayer
#' object with (1) low, (2) medium and (3) high productivity
#' @details baseline_lev() uses the proportion of drylands over the total land ('drylandProp') to classify the level of productivity into
#' low level. UNPD declares that 40% of the World’s land resources are drylands (Middleton et al., 2011) and, therefore, 40% of pixels
#' at the global level can be classified as low productivity land. This assumption is the default, but it should be adjusted for local
#' and regional studies. In addition, baseline_lev() classifies 10% of pixels as high level of land productivity and the rest
#' (100 - ('drylandProp' + 10) ) as medium level.
#' @import raster parallel
#' @importFrom data.table as.data.table rbind
#' @param obj2process Raster* object (or its file name). If time series, each layer is one year
#' @param yearsBaseline Numeric. Number of years to be averaged and used as baseline. Optional. Default is 3
#' @param drylandProp Numeric. Proportion of drylands over total land, either expressed as a fraction of unity or %. Optional. Default is 0.4
#' @param combineSteadiness Logical. If TRUE (default), Baseline levels are combined with Steadiness Index. Optional
#' @param SteadinessIndex Raster* object (or its file name). Steadiness Index. Optional
#' @param cores2use Numeric. Number of cores to use for parallelization. Optional. Default is 1 (no parallelization)
#' @param filename Character. Output filename. Optional
#' @return RasterLayer object
#' @name baseline_lev()
#' @references Middleton, N., L. Stringer, A. Goudie, and D. Thomas. 2011. “The Forgotten Billion. MDG Achievement in the Drylands.” New York,
#' NY, 10017, USA: United Nations Development Programme.
#' @export
#' @examples
#' \dontrun{
#' LPDynR:::baseline_lev(obj2process = obj2process_raster,
#'                       yearsBaseline = 3,
#'                       drylandProp = 0.4,
#'                       combineSteadiness = TRUE,
#'                       SteadinessIndex = SteadinessIndex_raster)
#' }
#'


baseline_lev <- function(obj2process = NULL,
                         yearsBaseline = 3,
                         drylandProp = 0.4,
                         cores2use = 1,
                         combineSteadiness = TRUE,
                         SteadinessIndex = NULL,
                         filename = "",
                         ...){

  ## Reading in data (Standing Biomass)
  if(is.null(obj2process)) stop("Please provide an object of classe Raster* (or a file names to read in from)")

  if(is.character(obj2process)){
    obj2process <- stack(obj2process)
  }else if(!class(obj2process) %in% c("RasterLayer", "RasterStack", "RasterBrick")){
    stop("Please provide an object of classe Raster* (or a file name to read in from)")
  }

  if(combineSteadiness & is.null(SteadinessIndex)) stop("Please provide an object of classe Raster* (or a file name to read in from) with the Steadiness Index; or set combineSteadiness = FALSE")


  #obj2process <- stack(paste0("/Users/xavi_rp/Documents/D6_LPD/phenolo_data_Cat", "/mi_clean_Cat.tif"))


  ## Averaging first years
  beginCluster(cores2use)
  yrs <- 1:yearsBaseline
  obj2process_avg13 <- clusterR(obj2process, calc, args = list(fun = mean_years_function), export = "yrs")
  endCluster()

  #some checks...
  rning_tsts <- "n"
  rning_tsts <- "y"
  if(rning_tsts == "y"){
    chk_avg <- round(mean(as.vector(obj2process[20, 40][1:3])), 0) == round(as.vector(obj2process_avg13[20, 40]), 0) # has to be TRUE
    if(chk_avg != TRUE) stop("Something wrong in the averaging process")
  }


  ## Classification of averaged pixels
  #The assumption of 40% of World's land area is dryland and, therefore, 40% of pixels is "low"
  #is fine at Global level. It must be adjusted for local studies!!!
  pix_categs <- raster::quantile(obj2process_avg13, probs =  seq(0, 1, 0.1), names = TRUE)
  pix_categs01 <- c(pix_categs[- length(pix_categs)])
  pix_categs1 <- as.data.frame(pix_categs[-1])
  pix_categs1$from <- pix_categs01

  if(drylandProp > 100){
    stop("Proportion of drylands ('drylandProp') must be < 100%")
  }else if(drylandProp > 1){
    drylandProp <- round((drylandProp / 10), 0)
  }else if(drylandProp < 1){
    drylandProp <- round((drylandProp * 10), 0)
  }else if(drylandProp == 1 | drylandProp == 100){
    drylandProp <- 10
  }
  otherLand <- 9 - drylandProp
  #pix_categs1$becomes <- 1:(length(pix_categs) - 1)
  pix_categs1$becomes <- c(rep(1, drylandProp), rep(2, otherLand), 3)
  pix_categs1 <- pix_categs1[, c(2, 1, 3)]
  names(pix_categs1)[2] <- "to"
  pix_categs1[1, 1] <- pix_categs1[1, 1] - 1

  obj2process_3class <- reclassify(obj2process_avg13, rcl = pix_categs1, filename = '', include.lowest = FALSE, right = TRUE)

  if(combineSteadiness == FALSE){
    ## Saving results
    if (filename != "") writeRaster(obj2process_3class, filename = filename, overwrite = TRUE)
    print("Baseline levels calculated, but not combined with Steadiness Index")
    return(obj2process_3class)
  }


  if(combineSteadiness){
    ## Combining Steadiness Index with baseline levels for Standing Biomass
    if(is.character(SteadinessIndex)){
      SteadinessIndex <- raster(SteadinessIndex)
    }else if(!class(obj2process) %in% c("RasterLayer")){
      stop("Please provide an object of classe RasterLayer (or a file name to read in from) for Steadiness Index")
    }

    SteadInd_Baseline <- raster(obj2process_3class)

    SteadInd_Baseline[SteadinessIndex == 1 & obj2process_3class == 1] <- 1   # baseline_lev Index 1 (Strong Negative) - Standing Biomass 1 (low)      -> St1-low
    SteadInd_Baseline[SteadinessIndex == 1 & obj2process_3class == 2] <- 2   # baseline_lev Index 1 (Strong Negative) - Standing Biomass 2 (medium)   -> St1-medium
    SteadInd_Baseline[SteadinessIndex == 1 & obj2process_3class == 3] <- 3   # baseline_lev Index 1 (Strong Negative) - Standing Biomass 3 (high)     -> St1-high
    SteadInd_Baseline[SteadinessIndex == 2 & obj2process_3class == 1] <- 4   # baseline_lev Index 2 (Moderate Negative) - Standing Biomass 1 (low)    -> St2-low
    SteadInd_Baseline[SteadinessIndex == 2 & obj2process_3class == 2] <- 5   # baseline_lev Index 2 (Moderate Negative) - Standing Biomass 2 (medium) -> St2-medium
    SteadInd_Baseline[SteadinessIndex == 2 & obj2process_3class == 3] <- 6   # baseline_lev Index 2 (Moderate Negative) - Standing Biomass 3 (high)   -> St2-high
    SteadInd_Baseline[SteadinessIndex == 3 & obj2process_3class == 1] <- 7   # baseline_lev Index 3 (Moderate Positive) - Standing Biomass 1 (low)    -> St3-low
    SteadInd_Baseline[SteadinessIndex == 3 & obj2process_3class == 2] <- 8   # baseline_lev Index 3 (Moderate Positive) - Standing Biomass 2 (medium) -> St3-medium
    SteadInd_Baseline[SteadinessIndex == 3 & obj2process_3class == 3] <- 9   # baseline_lev Index 3 (Moderate Positive) - Standing Biomass 3 (high)   -> St3-high
    SteadInd_Baseline[SteadinessIndex == 4 & obj2process_3class == 1] <- 10   # baseline_lev Index 4 (Strong Positive) - Standing Biomass 1 (low)     -> St4-low
    SteadInd_Baseline[SteadinessIndex == 4 & obj2process_3class == 2] <- 11   # baseline_lev Index 4 (Strong Positive) - Standing Biomass 2 (medium)  -> St4-medium
    SteadInd_Baseline[SteadinessIndex == 4 & obj2process_3class == 3] <- 12   # baseline_lev Index 4 (Strong Positive) - Standing Biomass 3 (high)    -> St4-high

    ## Saving results
    if (filename != "") writeRaster(SteadInd_Baseline, filename = filename, overwrite = TRUE)
    print("Baseline levels calculated and combined with Steadiness Index")
    return(SteadInd_Baseline)

  }
}

