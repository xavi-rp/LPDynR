
#### Combined Assessment of Land Productivity  ####
## Land Productivity Long Term Change Map + Land Productivity Current Status Map

#'
#' @author Xavier Rotllan-Puig
#' @title Land Productivity Dynamics Indicator (Combined Assessment)
#' @description Land Productivity Long Term Change Map + Land Productivity Current Status Map
#' @details 
#' @import 
#' @importFrom
#' @param LandProd_change RasterLayer object (or its file name). Land Productivity Long Term Change Map
#' @param LandProd_current RasterLayer object (or its file name). Land Productivity Current Status Map
#' @param filename Character. Output filename. Optional
#' @return RasterLayer of the Land Productivity Dynamics (LPD) indicator. Produced by the combined assessment of the Land Productivity Long Term Change Map and the Land Productivity Current Status Map 
#' @name LPD_CombAssess()
#' @references 
#' @examples
#' \dontrun{
#' LPDynR:::LPD_CombAssess(LandProd_change = LandProd_change_raster,
#'                         LandProd_current = LandProd_current_raster)
#' }
#'


LPD_CombAssess <- function(LandProd_change = NULL, LandProd_current = NULL,
                           filename = ""){
  
  ## Reading in rasters ####
  if(any(is.null(LandProd_change), is.null(LandProd_current))) stop("Please provide objects of classe RasterLayer (or file names to read in some)")
  
  if(is.character(LandProd_change)){
    LandProd_change <- raster(LandProd_change)
  }else if(class(LandProd_change) != "RasterLayer"){
    stop("Please provide objects of classe RasterLayer (or file names to read in some)")
  }
    
  if(is.character(LandProd_current)){
    LandProd_current <- raster(LandProd_current)
  }else if(class(LandProd_current) != "RasterLayer"){
    stop("Please provide objects of classe RasterLayer (or file names to read in some)")
  }
  
  ## Checking for same extent/resolution
  if(any(extent(LandProd_change) != extent(LandProd_current), res(LandProd_change) != res(LandProd_current)))
    stop("LandProd_change and LandProd_current must have same extent and resolution")
  

  ## Combined Assessment ####
  LPD_CombAssess <- LandProd_current
  LPD_CombAssess[LandProd_change %in% c(1:6, 8:9)      & LandProd_current  < 50] <- 1      # 1(d): Declining land productivity
  LPD_CombAssess[LandProd_change %in% c(3, 6)          & LandProd_current >= 50] <- 1      # 1(d): Declining land productivity
  LPD_CombAssess[LandProd_change %in% c(7)             & LandProd_current  < 50] <- 2      # 2(ew): Early signs of decline of land productivity
  LPD_CombAssess[LandProd_change %in% c(1:2, 4:5, 8:9) & LandProd_current >= 50] <- 2      # 2(ew): Early signs of decline of land productivity
  LPD_CombAssess[LandProd_change %in% c(7)             & LandProd_current >= 50] <- 3      # 3(nf): Negative fluctuation (stable, but stressed land prod.)
  LPD_CombAssess[LandProd_change %in% c(10:12)                                 ] <- 3      # 3(nf): Negative fluctuation (stable, but stressed land prod.)
  LPD_CombAssess[LandProd_change %in% c(13:15)                                 ] <- 4      # 4(pf): Positive fluctuation (stable, not stressed land prod.)
  LPD_CombAssess[LandProd_change %in% c(16:17, 19)     & LandProd_current  < 50] <- 4      # 4(pf): Positive fluctuation (stable, not stressed land prod.)
  LPD_CombAssess[LandProd_change %in% c(18, 20:22)     & LandProd_current  < 50] <- 5      # 5(i): Increasing land productivity
  LPD_CombAssess[LandProd_change %in% c(16:17, 19:20)  & LandProd_current >= 50] <- 5      # 5(i): Increasing land productivity
  LPD_CombAssess[LandProd_change %in% c(18, 21:22)     & LandProd_current >= 50] <- 6      # 6(si): Strongly increasing land productivity
  LPD_CombAssess[is.na(LandProd_change)] <- NA
  
  ## Saving results ####
  if (filename != "") writeRaster(LPD_CombAssess, filename = filename, overwrite = TRUE)
  return(LPD_CombAssess)

}

