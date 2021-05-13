#### Combined Assessment of Land Productivity
## Land Productivity Long Term Change Map + Land Productivity Current Status Map

#'
#' @author Xavier Rotllan-Puig
#' @title LPD_CombAssess
#' @description LPD_CombAssess combines a 'LandProd_change' map (RasterLayer) with a 'LandProd_current'
#' map (RasterLayer), giving a 5-classes map ranging from declining to increasing land
#' productivity. 'LandProd_current' is reclassified into two classes: pixels with less than 'local_prod_threshold'
#' (in percentage; 50 by default) of potential local productivity (within the EFT) and pixels with more or equal to
#' 'local_prod_threshold'.
#'
#' If 'LandProd_current' = NULL, 'LandProd_change' is directly reclassified into the same 5-classes map
#' without using 'LandProd_current'. See the ATBD for the way pixels are reclassified.
#'
#'
#' @details
#' LandProd_change c(1:6, 8:9)       &  LandProd_current  < 'local_prod_threshold'  <- 1 Declining land productivity
#'
#' LandProd_change c(3, 6)           &  LandProd_current >= 'local_prod_threshold'  <- 1 Declining land productivity
#'
#' LandProd_change c(7)              &  LandProd_current  < 'local_prod_threshold'  <- 2 Early signs of decline of land productivity
#'
#' LandProd_change c(1:2, 4:5, 8:9)  &  LandProd_current >= 'local_prod_threshold'  <- 2 Early signs of decline of land productivity
#'
#' LandProd_change c(7)              &  LandProd_current >= 'local_prod_threshold'  <- 3 Negative fluctuation (stable, but stressed land prod.)
#'
#' LandProd_change c(10:12)                                     <- 3 Negative fluctuation (stable, but stressed land prod.)
#
#' LandProd_change c(13:15)                                     <- 4 Positive fluctuation (stable, not stressed land prod.)
#'
#' LandProd_change c(16:17, 19)      &  LandProd_current  < 'local_prod_threshold'  <- 4 Positive fluctuation (stable, not stressed land prod.)
#'
#' LandProd_change c(18, 20:22)      &  LandProd_current  < 'local_prod_threshold'  <- 5 Increasing land productivity
#'
#' LandProd_change c(16:22)          &  LandProd_current >= 'local_prod_threshold'  <- 5 Increasing land productivity
#'
#'
#' Values = 0 in the final map indicates that there is a scarcity of data in the productivity variable
#' (i.e. only 1 year with data), so that the indicator cannot be calculated
#'
#' 'local_prod_threshold' is the threshold used to classify 'LandProd_current' into pixels with low or high
#' potential productivity within its Ecosystem Functional Type
#'
#'
#' @import raster
#' @param LandProd_change RasterLayer object (or its file name). Land Productivity Long Term Change Map
#' @param LandProd_current RasterLayer object (or its file name). Land Productivity Current Status Map
#' @param local_prod_threshold Numeric. Potential local productivity threshold (within the Ecosystem Functional Type) in percentage. Optional. Default = 50
#' @param filename Character. Output filename. Optional
#' @return RasterLayer
#' @name LPD_CombAssess
#' @seealso \code{\link{LongTermChange}}; \code{\link{LNScaling}}
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
#' LandProd_change_raster <- LongTermChange(SteadinessIndex = SteadinessIndex_raster,
#'                                          BaselineLevels = BaselineLevels_raster,
#'                                          StateChange = StateChange_raster)
#'
#' dirctry <- paste0(system.file(package='LPDynR'), "/extdata")
#' variables_noCor <- rm_multicol(dir2process = dirctry,
#'                                multicol_cutoff = 0.7)
#' EFTs_raster <- EFT_clust(obj2clust = variables_noCor,
#'                          n_clust = 10)
#'
#' LandProd_current_raster <- LNScaling(EFTs = EFTs_raster[[1]],
#'                                      ProdVar = sb)
#'
#' LPD_CombAssess(LandProd_change = LandProd_change_raster,
#'                LandProd_current = LandProd_current_raster)
#' }
#'


LPD_CombAssess <- function(LandProd_change = NULL, LandProd_current = NULL,
                           local_prod_threshold = 50,
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

    if(local_prod_threshold < 1 | local_prod_threshold > 100) stop("'local_prod_threshold' has to be in %")

    ## Checking for same extent/resolution
    if(any(extent(LandProd_change) != extent(LandProd_current), res(LandProd_change) != res(LandProd_current)))
      stop("LandProd_change and LandProd_current must have same extent and resolution")


    ## Combined Assessment ####
    LPD_CombAssess <- LandProd_change
    names(LPD_CombAssess) <- "LPD_CombAssess"

    LandProd_change_vals <- getValues(LandProd_change)
    LandProd_current_vals <- getValues(LandProd_current)
    LPD_CombAssess_vals <- rep(NA, length(LandProd_change_vals))


    LPD_CombAssess_vals[LandProd_change_vals %in% c(1:6, 8:9)      & LandProd_current_vals  < local_prod_threshold] <- 1      # 1(d): Declining land productivity
    LPD_CombAssess_vals[LandProd_change_vals %in% c(3, 6)          & LandProd_current_vals >= local_prod_threshold] <- 1      # 1(d): Declining land productivity
    LPD_CombAssess_vals[LandProd_change_vals %in% c(7)             & LandProd_current_vals  < local_prod_threshold] <- 2      # 2(ew): Early signs of decline of land productivity
    LPD_CombAssess_vals[LandProd_change_vals %in% c(1:2, 4:5, 8:9) & LandProd_current_vals >= local_prod_threshold] <- 2      # 2(ew): Early signs of decline of land productivity
    LPD_CombAssess_vals[LandProd_change_vals %in% c(7)             & LandProd_current_vals >= local_prod_threshold] <- 3      # 3(nf): Negative fluctuation (stable, but stressed land prod.)
    LPD_CombAssess_vals[LandProd_change_vals %in% c(10:12)                                                        ] <- 3      # 3(nf): Negative fluctuation (stable, but stressed land prod.)
    LPD_CombAssess_vals[LandProd_change_vals %in% c(13:15)                                                        ] <- 4      # 4(pf): Positive fluctuation (stable, not stressed land prod.)
    LPD_CombAssess_vals[LandProd_change_vals %in% c(16:17, 19)     & LandProd_current_vals  < local_prod_threshold] <- 4      # 4(pf): Positive fluctuation (stable, not stressed land prod.)
    LPD_CombAssess_vals[LandProd_change_vals %in% c(18, 20:22)     & LandProd_current_vals  < local_prod_threshold] <- 5      # 5(i): Increasing land productivity
    LPD_CombAssess_vals[LandProd_change_vals %in% c(16:22)         & LandProd_current_vals >= local_prod_threshold] <- 5      # 5(i): Increasing land productivity
    #LPD_CombAssess_vals[LandProd_change_vals %in% c(16:17, 19:20)  & LandProd_current_vals >= local_prod_threshold] <- 5      # 5(i): Increasing land productivity
    #LPD_CombAssess_vals[LandProd_change_vals %in% c(18, 21:22)     & LandProd_current_vals >= local_prod_threshold] <- 6      # 6(si): Strongly increasing land productivity
    #LPD_CombAssess_vals[is.na(LandProd_change_vals)] <- NA


  }else{
    message("'LandProd_current' is NULL... no combined assessment and proceeding with 'LandProd_change' reclassification\n")

    ## Reclassification of 'LandProd_change' ####
    LPD_CombAssess <- LandProd_change
    names(LPD_CombAssess) <- "LPD_LandProdChange_reclass"

    LandProd_change_vals <- getValues(LandProd_change)
    LPD_CombAssess_vals <- rep(NA, length(LandProd_change_vals))


    LPD_CombAssess_vals[LandProd_change_vals %in% c(1:6, 8:9)      ] <- 1      # 1(d): Declining land productivity
    LPD_CombAssess_vals[LandProd_change_vals %in% c(7)             ] <- 2      # 2(ew): Early signs of decline of land productivity
    LPD_CombAssess_vals[LandProd_change_vals %in% c(10:12)         ] <- 3      # 3(nf): Negative fluctuation (stable, but stressed land prod.)
    LPD_CombAssess_vals[LandProd_change_vals %in% c(13:15)         ] <- 4      # 4(pf): Positive fluctuation (stable, not stressed land prod.)
    LPD_CombAssess_vals[LandProd_change_vals %in% c(16:17, 19)     ] <- 4      # 4(pf): Positive fluctuation (stable, not stressed land prod.)
    LPD_CombAssess_vals[LandProd_change_vals %in% c(18, 20:22)     ] <- 5      # 5(i): Increasing land productivity
    #LPD_CombAssess_vals[is.na(LandProd_change_vals)] <- NA
  }

  LPD_CombAssess <- setValues(LPD_CombAssess, LPD_CombAssess_vals)


  ## Saving results ####
  if (filename != "") writeRaster(LPD_CombAssess, filename = filename, overwrite = TRUE)
  return(LPD_CombAssess)

}

