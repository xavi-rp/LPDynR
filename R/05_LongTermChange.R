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

  SteadInd_Baseline[SteadinessIndex == 1 & BaselineLevels == 1] <- 1    # Steadiness Index 1 (Strong Negative) - Base Line Level 1 (low)      -> St1-low
  SteadInd_Baseline[SteadinessIndex == 1 & BaselineLevels == 2] <- 2    # Steadiness Index 1 (Strong Negative) - Base Line Level 2 (medium)   -> St1-medium
  SteadInd_Baseline[SteadinessIndex == 1 & BaselineLevels == 3] <- 3    # Steadiness Index 1 (Strong Negative) - Base Line Level 3 (high)     -> St1-high
  SteadInd_Baseline[SteadinessIndex == 2 & BaselineLevels == 1] <- 4    # Steadiness Index 2 (Moderate Negative) - Base Line Level 1 (low)    -> St2-low
  SteadInd_Baseline[SteadinessIndex == 2 & BaselineLevels == 2] <- 5    # Steadiness Index 2 (Moderate Negative) - Base Line Level 2 (medium) -> St2-medium
  SteadInd_Baseline[SteadinessIndex == 2 & BaselineLevels == 3] <- 6    # Steadiness Index 2 (Moderate Negative) - Base Line Level 3 (high)   -> St2-high
  SteadInd_Baseline[SteadinessIndex == 3 & BaselineLevels == 1] <- 7    # Steadiness Index 3 (Moderate Positive) - Base Line Level 1 (low)    -> St3-low
  SteadInd_Baseline[SteadinessIndex == 3 & BaselineLevels == 2] <- 8    # Steadiness Index 3 (Moderate Positive) - Base Line Level 2 (medium) -> St3-medium
  SteadInd_Baseline[SteadinessIndex == 3 & BaselineLevels == 3] <- 9    # Steadiness Index 3 (Moderate Positive) - Base Line Level 3 (high)   -> St3-high
  SteadInd_Baseline[SteadinessIndex == 4 & BaselineLevels == 1] <- 10   # Steadiness Index 4 (Strong Positive) - Base Line Level 1 (low)     -> St4-low
  SteadInd_Baseline[SteadinessIndex == 4 & BaselineLevels == 2] <- 11   # Steadiness Index 4 (Strong Positive) - Base Line Level 2 (medium)  -> St4-medium
  SteadInd_Baseline[SteadinessIndex == 4 & BaselineLevels == 3] <- 12   # Steadiness Index 4 (Strong Positive) - Base Line Level 3 (high)    -> St4-high



  ## Reclassifying into long term change categories (22)
  LandProd_change <- SteadInd_Baseline

  LandProd_change[SteadInd_Baseline == 1 & StateChange == 1] <- 1      #St1-low-No Change
  LandProd_change[SteadInd_Baseline == 1 & StateChange == 2] <- 2      #St1-low-Change 1 categ
  LandProd_change[SteadInd_Baseline == 1 & StateChange == 3] <- 3      #St1-low-Change 2 or more categs
  LandProd_change[SteadInd_Baseline == 2 & StateChange == 1] <- 4      #St1-medium-No Change
  LandProd_change[SteadInd_Baseline == 2 & StateChange == 2] <- 5      #St1-medium-Change 1 categ
  LandProd_change[SteadInd_Baseline == 2 & StateChange == 3] <- 6      #St1-medium-Change 2 or more categs
  LandProd_change[SteadInd_Baseline == 3 & StateChange == 1] <- 7      #St1-high-No Change
  LandProd_change[SteadInd_Baseline == 3 & StateChange == 2] <- 8      #St1-high-Change 1 categ
  LandProd_change[SteadInd_Baseline == 3 & StateChange == 3] <- 9      #St1-high-Change 2 or more categs
  LandProd_change[SteadInd_Baseline == 4 & StateChange == 1] <- 10     #St2-low-No Change
  LandProd_change[SteadInd_Baseline == 4 & StateChange == 2] <- 10     #St2-low-Change 1 categ
  LandProd_change[SteadInd_Baseline == 4 & StateChange == 3] <- 10     #St2-low-Change 2 or more categs
  LandProd_change[SteadInd_Baseline == 5 & StateChange == 1] <- 11     #St2-medium-No Change
  LandProd_change[SteadInd_Baseline == 5 & StateChange == 2] <- 11     #St2-medium-Change 1 categ
  LandProd_change[SteadInd_Baseline == 5 & StateChange == 3] <- 11     #St2-medium-Change 2 or more categs
  LandProd_change[SteadInd_Baseline == 6 & StateChange == 1] <- 12     #St2-high-No Change
  LandProd_change[SteadInd_Baseline == 6 & StateChange == 2] <- 12     #St2-high-Change 1 categ
  LandProd_change[SteadInd_Baseline == 6 & StateChange == 3] <- 12     #St2-high-Change 2 or more categs
  LandProd_change[SteadInd_Baseline == 7 & StateChange == 1] <- 13     #St3-low-No Change
  LandProd_change[SteadInd_Baseline == 7 & StateChange == 2] <- 13     #St3-low-Change 1 categ
  LandProd_change[SteadInd_Baseline == 7 & StateChange == 3] <- 13     #St3-low-Change 2 or more categs
  LandProd_change[SteadInd_Baseline == 8 & StateChange == 1] <- 14     #St3-medium-No Change
  LandProd_change[SteadInd_Baseline == 8 & StateChange == 2] <- 14     #St3-medium-Change 1 categ
  LandProd_change[SteadInd_Baseline == 8 & StateChange == 3] <- 14     #St3-medium-Change 2 or more categs
  LandProd_change[SteadInd_Baseline == 9 & StateChange == 1] <- 15     #St3-high-No Change
  LandProd_change[SteadInd_Baseline == 9 & StateChange == 2] <- 15     #St3-high-Change 1 categ
  LandProd_change[SteadInd_Baseline == 9 & StateChange == 3] <- 15     #St3-high-Change 2 or more categs
  LandProd_change[SteadInd_Baseline == 10 & StateChange == 1] <- 16    #St4-low-No Change
  LandProd_change[SteadInd_Baseline == 10 & StateChange == 2] <- 17    #St4-low-Change 1 categ
  LandProd_change[SteadInd_Baseline == 10 & StateChange == 3] <- 18    #St4-low-Change 2 or more categs
  LandProd_change[SteadInd_Baseline == 11 & StateChange == 1] <- 19    #St4-medium-No Change
  LandProd_change[SteadInd_Baseline == 11 & StateChange == 2] <- 20    #St4-medium-Change 1 categ
  LandProd_change[SteadInd_Baseline == 11 & StateChange == 3] <- 21    #St4-medium-Change 2 or more categs
  LandProd_change[SteadInd_Baseline == 12 & StateChange == 1] <- 22    #St4-high-No Change
  LandProd_change[SteadInd_Baseline == 12 & StateChange == 2] <- 22    #St4-high-Change 1 categ
  LandProd_change[SteadInd_Baseline == 12 & StateChange == 3] <- 22    #St4-high-Change 2 or more categs
  LandProd_change[is.na(StateChange)] <- NA

  ## Saving results
  if (filename != "") writeRaster(LandProd_change, filename = filename, overwrite = TRUE)
  return(LandProd_change)

}
