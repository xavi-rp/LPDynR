
#### Combined Assessment of Land Productivity  ####
## Land Productivity Long Term Change Map + Land Productivity Current Status Map

#'
#' @author Xavier Rotllan-Puig
#' @title Combined Assessment of Land Productivity
#' @description 
#' @details 
#' @import 
#' @importFrom stats quantile sd
#' @importFrom utils read.csv write.csv
#' @param occ Data set with presences (occurrences). A data frame with 3 columns: long, lat and species name (in this order)
#' @param varbles A raster brick of the independent variables, or a directory where the rasters are. It will use all the rasters in the folder. Supported: .tif and .bil
#' @param wd A directory to save the results
#' @param prj Coordinates system (e.g. "4326" is WGS84; check \url{http://spatialreference.org/} )
#' @param num_bands Number of buffers
#' @param n_rep Number of replicates
#' @param occ_prop_test Proportion of presences (occurrences) set aside for testing
#' @param maxent_tool Either "dismo" or "maxnet"
#' @param BI_part Maximum Boyce Index Partial to stop the process if reached
#' @param BI_tot Maximum Boyce Index Total to stop the process if reached
#' @param SD_BI_part Minimum SD of the Boyce Index Partial to stop the process if reached (last 3 buffers)
#' @param SD_BI_tot Minimum SD of the Boyce Index Total to stop the process if reached (last 3 buffers)
#' @return \code{selfinfo_mod_}, \code{info_mod_} and \code{info_mod_means_} (all followed by the name of the species). The first two tables are merely informative about how the modelling process has been developed and the results of each model. Whereas \code{info_mod_means_} shows the means of the n models run for each buffer
#' @name minba()
#' @references Rotllan-Puig, X. & Traveset, A. 2019. Determining the Minimal Background Area for Species Distribution Models: MinBAR Package. bioRxiv. 571182. DOI: 10.1101/571182
#' @examples
#' \dontrun{
#' LPDynR:::LPD_CombAssess(occ = sprecords, varbles = bioscrop,
#' wd = tempdir(), prj = 4326, num_bands = 3, n_rep = 3,
#' maxent_tool = "maxnet")
#' }
#'
#



## Reading in data sets (Step 05 and Step10) ####


LPD_CombAssess <- function(LandProd_change = NULL, LocalNetProductivity = NULL){
  
  ## Reading in rasters ####
  if(any(is.null(LandProd_change), is.null(LocalNetProductivity))) stop("Please provide objects of classe RasterLayer or file names to read in some")
  
  if(is.character(LandProd_change)){
    LandProd_change <- raster(LandProd_change)
  }else if(class(LandProd_change) != "RasterLayer"){
    stop("Please provide objects of classe RasterLayer or file names to read in some")
  }
    
  if(is.character(LocalNetProductivity)){
    LocalNetProductivity <- raster(LocalNetProductivity)
  }else if(class(LocalNetProductivity) != "RasterLayer"){
    stop("Please provide objects of classe RasterLayer or file names to read in some")
  }
    

  ## Combined Assessment ####
  LPD_CombAssess <- LocalNetProductivity
  LPD_CombAssess[LandProd_change %in% c(1:6, 8:9)      & LocalNetProductivity  < 50] <- 1      # 1(d): Declining land productivity
  LPD_CombAssess[LandProd_change %in% c(3, 6)          & LocalNetProductivity >= 50] <- 1      # 1(d): Declining land productivity
  LPD_CombAssess[LandProd_change %in% c(7)             & LocalNetProductivity  < 50] <- 2      # 2(ew): Early signs of decline of land productivity
  LPD_CombAssess[LandProd_change %in% c(1:2, 4:5, 8:9) & LocalNetProductivity >= 50] <- 2      # 2(ew): Early signs of decline of land productivity
  LPD_CombAssess[LandProd_change %in% c(7)             & LocalNetProductivity >= 50] <- 3      # 3(nf): Negative fluctuation (stable, but stressed land prod.)
  LPD_CombAssess[LandProd_change %in% c(10:12)                                     ] <- 3      # 3(nf): Negative fluctuation (stable, but stressed land prod.)
  LPD_CombAssess[LandProd_change %in% c(13:15)                                     ] <- 4      # 4(pf): Positive fluctuation (stable, not stressed land prod.)
  LPD_CombAssess[LandProd_change %in% c(16:17, 19)     & LocalNetProductivity  < 50] <- 4      # 4(pf): Positive fluctuation (stable, not stressed land prod.)
  LPD_CombAssess[LandProd_change %in% c(18, 20:22)     & LocalNetProductivity  < 50] <- 5      # 5(i): Increasing land productivity
  LPD_CombAssess[LandProd_change %in% c(16:17, 19:20)  & LocalNetProductivity >= 50] <- 5      # 5(i): Increasing land productivity
  LPD_CombAssess[LandProd_change %in% c(18, 21:22)     & LocalNetProductivity >= 50] <- 6      # 6(si): Strongly increasing land productivity
  LPD_CombAssess[is.na(LandProd_change)] <- NA
  
  ## Saving results ####
  writeRaster(LPD_CombAssess, "LPD_CombinedAssessment.tif", overwrite = TRUE)
  return(LPD_CombAssess)

}

