## Land-Productivity Long Term Change Map

#'
#' @author Xavier Rotllan-Puig
#' @title LongTermChange
#' @description LongTermChange combines the Steadiness Index with the baseline levels of land productivity and with the change of
#' state along the time series, resulting in a 22-class object (see details)
#' @details
#' St1-low-No Change  <- 1
#'
#' St1-low-Change 1 categ  <- 2
#'
#' St1-low-Change 2 or more categs  <- 3
#'
#' St1-medium-No Change  <- 4
#'
#' St1-medium-Change 1 categ  <- 5
#'
#' St1-medium-Change 2 or more categs  <- 6
#'
#' St1-high-No Change  <- 7
#'
#' St1-high-Change 1 categ  <- 8
#'
#' St1-high-Change 2 or more categs  <- 9
#'
#' St2-low-No Change  <- 10
#'
#' St2-low-Change 1 categ  <- 10
#'
#' St2-low-Change 2 or more categs  <- 10
#'
#' St2-medium-No Change  <- 11
#'
#' St2-medium-Change 1 categ  <- 11
#'
#' St2-medium-Change 2 or more categs  <- 11
#'
#' St2-high-No Change  <- 12
#'
#' St2-high-Change 1 categ  <- 12
#'
#' St2-high-Change 2 or more categs  <- 12
#'
#' St3-low-No Change  <- 13
#'
#' St3-low-Change 1 categ  <- 13
#'
#' St3-low-Change 2 or more categs  <- 13
#'
#' St3-medium-No Change  <- 14
#'
#' St3-medium-Change 1 categ  <- 14
#'
#' St3-medium-Change 2 or more categs  <- 14
#'
#' St3-high-No Change  <- 15
#'
#' St3-high-Change 1 categ  <- 15
#'
#' St3-high-Change 2 or more categs  <- 15
#'
#' St4-low-No Change  <- 16
#'
#' St4-low-Change 1 categ  <- 17
#'
#' St4-low-Change 2 or more categs  <- 18
#'
#' St4-medium-No Change  <- 19
#'
#' St4-medium-Change 1 categ  <- 20
#'
#' St4-medium-Change 2 or more categs  <- 21
#'
#' St4-high-No Change  <- 22
#'
#' St4-high-Change 1 categ  <- 22
#'
#' St4-high-Change 2 or more categs  <- 22
#'
#' Values = 0 in the final map indicates that there is a scarcity of data in the productivity variable
#' (i.e. only 1 year with data), so that the indicator cannot be calculated
#'
#' @import raster
#' @importFrom data.table as.data.table setkeyv
#' @importFrom dplyr group_by summarise_at
#' @param SteadinessIndex RasterLayer object (or its file name). Steadiness Index (4-class)
#' @param BaselineLevels RasterLayer object (or its file name). Baseline levels of land productivity (beginning of time series; 3-class)
#' @param StateChange RasterLayer object (or its file name). Change of state of land productivity (beginning minus end of time series; 3-class)
#' @param filename Character. Output filename. Optional
#' @return RasterLayer object
#' @name LongTermChange
#' @seealso \code{\link{steadiness}}, \code{\link{baseline_lev}}, \code{\link{state_change}}
#' @export
#' @examples
#' \donttest{
#' sb <- raster::brick(paste0(system.file(package='LPDynR'), "/extdata/sb_cat.tif"))
#' SteadinessIndex_raster <- steadiness(obj2process = sb)
#' BaselineLevels_raster <- baseline_lev(obj2process = sb,
#'                                       yearsBaseline = 3,
#'                                       drylandProp = 0.4)
#' StateChange_raster <- state_change(obj2process = sb,
#'                                    yearsBaseline = 3)
#'
#' LongTermChange(SteadinessIndex = SteadinessIndex_raster,
#'                BaselineLevels = BaselineLevels_raster,
#'                StateChange = StateChange_raster)
#' }
#'



LongTermChange <- function(SteadinessIndex = NULL,
                           BaselineLevels = NULL,
                           StateChange = NULL,
                           filename = ""){

  ## Reading in data
  if(any(is.null(SteadinessIndex), is.null(BaselineLevels), is.null(StateChange))) stop("Please provide objects of classe Raster* (or file names to read in some)")

  if(is.character(SteadinessIndex)){
    SteadinessIndex <- raster(SteadinessIndex)
  }else if(!class(SteadinessIndex) %in% c("RasterLayer")){
    stop("Please provide an object of classe RasterLayer for Steadiness Index (or a file name to read in from)")
  }

  if(is.character(BaselineLevels)){
    BaselineLevels <- raster(BaselineLevels)
  }else if(!class(BaselineLevels) %in% "RasterLayer"){
    stop("Please provide objects of classe RasterLayer for 'BaselineLevels' (or a file name to read in from)")
  }

  if(is.character(StateChange)){
    StateChange <- raster(StateChange)
  }else if(!class(StateChange) %in% "RasterLayer"){
    stop("Please provide objects of classe RasterLayer for 'StateChange' (or a file name to read in from)")
  }


  ## Combining Steadiness Index with baseline levels
  SteadInd_Baseline <- SteadinessIndex
  names(SteadInd_Baseline) <- "SteadInd_Baseline"

  SteadinessIndex_vals <- getValues(SteadinessIndex)
  BaselineLevels_vals <- getValues(BaselineLevels)
  SteadInd_Baseline_vals <- rep(NA, length(SteadinessIndex_vals))


  SteadInd_Baseline_vals[SteadinessIndex_vals == 1 & BaselineLevels_vals == 1] <- 1    # Steadiness Index 1 (Strong Negative) - Base Line Level 1 (low)      -> St1-low
  SteadInd_Baseline_vals[SteadinessIndex_vals == 1 & BaselineLevels_vals == 2] <- 2    # Steadiness Index 1 (Strong Negative) - Base Line Level 2 (medium)   -> St1-medium
  SteadInd_Baseline_vals[SteadinessIndex_vals == 1 & BaselineLevels_vals == 3] <- 3    # Steadiness Index 1 (Strong Negative) - Base Line Level 3 (high)     -> St1-high
  SteadInd_Baseline_vals[SteadinessIndex_vals == 2 & BaselineLevels_vals == 1] <- 4    # Steadiness Index 2 (Moderate Negative) - Base Line Level 1 (low)    -> St2-low
  SteadInd_Baseline_vals[SteadinessIndex_vals == 2 & BaselineLevels_vals == 2] <- 5    # Steadiness Index 2 (Moderate Negative) - Base Line Level 2 (medium) -> St2-medium
  SteadInd_Baseline_vals[SteadinessIndex_vals == 2 & BaselineLevels_vals == 3] <- 6    # Steadiness Index 2 (Moderate Negative) - Base Line Level 3 (high)   -> St2-high
  SteadInd_Baseline_vals[SteadinessIndex_vals == 3 & BaselineLevels_vals == 1] <- 7    # Steadiness Index 3 (Moderate Positive) - Base Line Level 1 (low)    -> St3-low
  SteadInd_Baseline_vals[SteadinessIndex_vals == 3 & BaselineLevels_vals == 2] <- 8    # Steadiness Index 3 (Moderate Positive) - Base Line Level 2 (medium) -> St3-medium
  SteadInd_Baseline_vals[SteadinessIndex_vals == 3 & BaselineLevels_vals == 3] <- 9    # Steadiness Index 3 (Moderate Positive) - Base Line Level 3 (high)   -> St3-high
  SteadInd_Baseline_vals[SteadinessIndex_vals == 4 & BaselineLevels_vals == 1] <- 10   # Steadiness Index 4 (Strong Positive) - Base Line Level 1 (low)     -> St4-low
  SteadInd_Baseline_vals[SteadinessIndex_vals == 4 & BaselineLevels_vals == 2] <- 11   # Steadiness Index 4 (Strong Positive) - Base Line Level 2 (medium)  -> St4-medium
  SteadInd_Baseline_vals[SteadinessIndex_vals == 4 & BaselineLevels_vals == 3] <- 12   # Steadiness Index 4 (Strong Positive) - Base Line Level 3 (high)    -> St4-high
  #SteadInd_Baseline <- setValues(SteadInd_Baseline, SteadInd_Baseline_vals)

  ## Reclassifying into long term change categories (22)
  LandProd_change <- SteadinessIndex
  names(LandProd_change) <- "LandProd_change"

  StateChange_vals <- getValues(StateChange)
  LandProd_change_vals <- rep(NA, length(StateChange_vals))


  LandProd_change_vals[SteadInd_Baseline_vals == 1 & StateChange_vals == 1] <- 1      #St1-low-No Change
  LandProd_change_vals[SteadInd_Baseline_vals == 1 & StateChange_vals == 2] <- 2      #St1-low-Change 1 categ
  LandProd_change_vals[SteadInd_Baseline_vals == 1 & StateChange_vals == 3] <- 3      #St1-low-Change 2 or more categs
  LandProd_change_vals[SteadInd_Baseline_vals == 2 & StateChange_vals == 1] <- 4      #St1-medium-No Change
  LandProd_change_vals[SteadInd_Baseline_vals == 2 & StateChange_vals == 2] <- 5      #St1-medium-Change 1 categ
  LandProd_change_vals[SteadInd_Baseline_vals == 2 & StateChange_vals == 3] <- 6      #St1-medium-Change 2 or more categs
  LandProd_change_vals[SteadInd_Baseline_vals == 3 & StateChange_vals == 1] <- 7      #St1-high-No Change
  LandProd_change_vals[SteadInd_Baseline_vals == 3 & StateChange_vals == 2] <- 8      #St1-high-Change 1 categ
  LandProd_change_vals[SteadInd_Baseline_vals == 3 & StateChange_vals == 3] <- 9      #St1-high-Change 2 or more categs
  LandProd_change_vals[SteadInd_Baseline_vals == 4 & StateChange_vals == 1] <- 10     #St2-low-No Change
  LandProd_change_vals[SteadInd_Baseline_vals == 4 & StateChange_vals == 2] <- 10     #St2-low-Change 1 categ
  LandProd_change_vals[SteadInd_Baseline_vals == 4 & StateChange_vals == 3] <- 10     #St2-low-Change 2 or more categs
  LandProd_change_vals[SteadInd_Baseline_vals == 5 & StateChange_vals == 1] <- 11     #St2-medium-No Change
  LandProd_change_vals[SteadInd_Baseline_vals == 5 & StateChange_vals == 2] <- 11     #St2-medium-Change 1 categ
  LandProd_change_vals[SteadInd_Baseline_vals == 5 & StateChange_vals == 3] <- 11     #St2-medium-Change 2 or more categs
  LandProd_change_vals[SteadInd_Baseline_vals == 6 & StateChange_vals == 1] <- 12     #St2-high-No Change
  LandProd_change_vals[SteadInd_Baseline_vals == 6 & StateChange_vals == 2] <- 12     #St2-high-Change 1 categ
  LandProd_change_vals[SteadInd_Baseline_vals == 6 & StateChange_vals == 3] <- 12     #St2-high-Change 2 or more categs
  LandProd_change_vals[SteadInd_Baseline_vals == 7 & StateChange_vals == 1] <- 13     #St3-low-No Change
  LandProd_change_vals[SteadInd_Baseline_vals == 7 & StateChange_vals == 2] <- 13     #St3-low-Change 1 categ
  LandProd_change_vals[SteadInd_Baseline_vals == 7 & StateChange_vals == 3] <- 13     #St3-low-Change 2 or more categs
  LandProd_change_vals[SteadInd_Baseline_vals == 8 & StateChange_vals == 1] <- 14     #St3-medium-No Change
  LandProd_change_vals[SteadInd_Baseline_vals == 8 & StateChange_vals == 2] <- 14     #St3-medium-Change 1 categ
  LandProd_change_vals[SteadInd_Baseline_vals == 8 & StateChange_vals == 3] <- 14     #St3-medium-Change 2 or more categs
  LandProd_change_vals[SteadInd_Baseline_vals == 9 & StateChange_vals == 1] <- 15     #St3-high-No Change
  LandProd_change_vals[SteadInd_Baseline_vals == 9 & StateChange_vals == 2] <- 15     #St3-high-Change 1 categ
  LandProd_change_vals[SteadInd_Baseline_vals == 9 & StateChange_vals == 3] <- 15     #St3-high-Change 2 or more categs
  LandProd_change_vals[SteadInd_Baseline_vals == 10 & StateChange_vals == 1] <- 16    #St4-low-No Change
  LandProd_change_vals[SteadInd_Baseline_vals == 10 & StateChange_vals == 2] <- 17    #St4-low-Change 1 categ
  LandProd_change_vals[SteadInd_Baseline_vals == 10 & StateChange_vals == 3] <- 18    #St4-low-Change 2 or more categs
  LandProd_change_vals[SteadInd_Baseline_vals == 11 & StateChange_vals == 1] <- 19    #St4-medium-No Change
  LandProd_change_vals[SteadInd_Baseline_vals == 11 & StateChange_vals == 2] <- 20    #St4-medium-Change 1 categ
  LandProd_change_vals[SteadInd_Baseline_vals == 11 & StateChange_vals == 3] <- 21    #St4-medium-Change 2 or more categs
  LandProd_change_vals[SteadInd_Baseline_vals == 12 & StateChange_vals == 1] <- 22    #St4-high-No Change
  LandProd_change_vals[SteadInd_Baseline_vals == 12 & StateChange_vals == 2] <- 22    #St4-high-Change 1 categ
  LandProd_change_vals[SteadInd_Baseline_vals == 12 & StateChange_vals == 3] <- 22    #St4-high-Change 2 or more categs
  #LandProd_change_vals[is.na(StateChange_vals)] <- NA

  LandProd_change <- setValues(LandProd_change, LandProd_change_vals)


  ## Saving results
  if (filename != "") writeRaster(LandProd_change, filename = filename, overwrite = TRUE)
  return(LandProd_change)

}
