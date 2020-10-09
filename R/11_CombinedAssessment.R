#### Combined Assessment of Land Productivity
## Land Productivity Long Term Change Map + Land Productivity Current Status Map

#'
#' @author Xavier Rotllan-Puig
#' @title LPD_CombAssess
#' @description LPD_CombAssess combines a 'LandProd_change' map (RasterLayer) with a 'LandProd_current'
#' map (RasterLayer), giving a 5-classes map ranging from declining to increasing land
#' productivity. 'LandProd_current' is reclassified into two classes: pixels with less than 50% of the
#' highest local production (within the EFT) and pixels with more or equal to their 50%.
#' If 'LandProd_current' = NULL, 'LandProd_change' is directly reclassified into the same 5-classes map
#' without using 'LandProd_current'. See the ATBD for the way pixels are reclassified
#' @details The Land Productivity Dynamics (LPD) is a qualitative indicator produced by the combined
#' assessment of the Land Productivity Long Term Change Map and the Land Productivity Current
#' Status Map
#' @import raster
#' @param LandProd_change RasterLayer object (or its file name). Land Productivity Long Term Change Map
#' @param LandProd_current RasterLayer object (or its file name). Land Productivity Current Status Map
#' @param filename Character. Output filename. Optional
#' @return RasterLayer
#' @name LPD_CombAssess
#' @seealso \code{\link{LongTermChange}}; \code{\link{LNScaling}}
#' @export
#' @examples
#' \dontrun{
#' LPD_CombAssess(LandProd_change = LandProd_change_raster,
#'                LandProd_current = LandProd_current_raster)
#' }
#'


LPD_CombAssess <- function(LandProd_change = NULL, LandProd_current = NULL,
                           filename = ""){

  ## Reading in rasters ####
  if(is.null(LandProd_change)) stop("Please provide an object of classe RasterLayer (or file names to read in some) for 'LandProd_change'")

  if(is.character(LandProd_change)){
    LandProd_change <- raster(LandProd_change)
  }else if(class(LandProd_change) != "RasterLayer"){
    stop("Please provide objects of classe RasterLayer (or file names to read in some)")
  }

  if(!is.null(LandProd_current)){
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
    LPD_CombAssess[LandProd_change %in% c(16:22)         & LandProd_current >= 50] <- 5      # 5(i): Increasing land productivity
    #LPD_CombAssess[LandProd_change %in% c(16:17, 19:20)  & LandProd_current >= 50] <- 5      # 5(i): Increasing land productivity
    #LPD_CombAssess[LandProd_change %in% c(18, 21:22)     & LandProd_current >= 50] <- 6      # 6(si): Strongly increasing land productivity
    LPD_CombAssess[is.na(LandProd_change)] <- NA


  }else{
    cat("'LandProd_current' is NULL... no combined assessment and proceeding with 'LandProd_change' reclassification\n")

    ## Reclassification of 'LandProd_change' ####
    LPD_CombAssess <- LandProd_change
    LPD_CombAssess[LandProd_change %in% c(1:6, 8:9)      ] <- 1      # 1(d): Declining land productivity
    LPD_CombAssess[LandProd_change %in% c(7)             ] <- 2      # 2(ew): Early signs of decline of land productivity
    LPD_CombAssess[LandProd_change %in% c(10:12)         ] <- 3      # 3(nf): Negative fluctuation (stable, but stressed land prod.)
    LPD_CombAssess[LandProd_change %in% c(13:15)         ] <- 4      # 4(pf): Positive fluctuation (stable, not stressed land prod.)
    LPD_CombAssess[LandProd_change %in% c(16:17, 19)     ] <- 4      # 4(pf): Positive fluctuation (stable, not stressed land prod.)
    LPD_CombAssess[LandProd_change %in% c(18, 20:22)     ] <- 5      # 5(i): Increasing land productivity
    LPD_CombAssess[is.na(LandProd_change)] <- NA
  }


  ## Saving results ####
  if (filename != "") writeRaster(LPD_CombAssess, filename = filename, overwrite = TRUE)
  return(LPD_CombAssess)

}

