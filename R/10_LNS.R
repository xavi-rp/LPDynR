## Local Net Productivity Scaling (LNS)

#'
#' @author Xavier Rotllan-Puig
#' @title LNScaling
#' @description LNScaling (Local Net Productivity Scaling) uses a productivity variable
#' (SpatRaster), e.g. season growth, to calculate the actual status of land productivity
#' relative to its potential in homogeneous land areas or Ecosystem Functional Types
#' (SpatRaster). If the productivity variable 'ProdVar' is a SpatRaster
#' object with time series, it is calculated the average of the last 5 years
#' @details The Local Net Primary Production Scaling (LNS) method (Prince, 2009) calculates
#'  the difference between the potential and actual Net Primary Production for each pixel
#'  in homogeneous land areas. The current land production related to the local potential
#'  reflects the current level of productivity efficiency and, therefore, it is useful for
#'  the delineation of a land productivity status map
#' @rawNamespace import(terra, except = na.omit)
#' @importFrom data.table as.data.table setkeyv
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by summarise_at
#' @param EFTs SpatRaster object (or its file name). Ecosystem Functional Types. Its first variable has the number of EFT (cluster) each pixel belongs to
#' @param ProdVar SpatRaster object (or its file name). Productivity variable (e.g. Cyclic fraction -season growth-)
#' @param filename Character. Output filename. Optional
#' @param cores2use Numeric. Number of cores to use for parallelization. Optional. Default is 1 (no parallelization)
#' @return SpatRaster object
#' @name LNScaling
#' @seealso \code{\link{EFT_clust}}
#' @references Prince, S.D., Becker-Reshef, I. and Rishmawi, K. 2009. “Detection and Mapping of Long-Term Land Degradation Using Local Net Production Scaling: Application to Zimbabwe.” REMOTE SENSING OF ENVIRONMENT 113 (5): 1046–57
#' @export
#' @examples
#' \donttest{
#' dirctry <- paste0(system.file(package='LPDynR'), "/extdata")
#' variables_noCor <- rm_multicol(dir2process = dirctry,
#'                                multicol_cutoff = 0.7)
#' EFTs_raster <- EFT_clust(obj2clust = variables_noCor,
#'                          n_clust = 10)
#' sb <- terra::rast(paste0(system.file(package='LPDynR'), "/extdata/sb_cat.tif"))
#'
#' LNScaling(EFTs = EFTs_raster[[1]],
#'           ProdVar = sb)
#' }
#'



LNScaling <- function(EFTs = NULL, ProdVar = NULL,
                      cores2use = 1,
                      filename = ""){

  ## Reading in EFTs data (Step 09)
  if(any(is.null(EFTs), is.null(ProdVar))) stop("Please provide objects of classe SpatRaster (or file names to read in some)")

  if(is.character(EFTs)){
    EFTs <- rast(EFTs)
  }else if(!class(EFTs) %in% c("SpatRaster")){
    stop("Please provide objects of classe SpatRaster for EFTs (or a file name to read in from)")
  }


  ## Reading in productivity data (e.g. Cyclic Fraction) and averaging
  if(is.character(ProdVar)){
    ProdVar <- rast(ProdVar)
  }else if(!class(ProdVar) %in% c("SpatRaster")){
    stop("Please provide objects of classe SpatRaster (or a file name to read in from)")
  }

  ## Checking for same extent/resolution
  if(any(ext(EFTs) != ext(ProdVar), round(res(EFTs), 8) != round(res(ProdVar), 8)))
    stop("EFTs and ProdVar must have same extent and resolution")


  ## Averaging (if ProdVar has > 1 layer)
  if(nlyr(ProdVar) == 1) {
    ProdVar_average <- ProdVar
  }else if(nlyr(ProdVar) <= 4){
    yrs <- (1:nlyr(ProdVar))
    ProdVar_average <- app(ProdVar, fun = mean_years_function, cores = cores2use, yrs = yrs)

  }else if(nlyr(ProdVar) > 4){
    yrs <- (nlyr(ProdVar) - 4):nlyr(ProdVar)
    ProdVar_average <- app(ProdVar, fun = mean_years_function, cores = cores2use, yrs = yrs)

  }

  ## Merging productivity variable data with new clusters
  ProdVar_average_df <- as.data.frame(ProdVar_average, na.rm = FALSE)
  EFTs_df <- as.data.frame(EFTs)
  ProdVar_average_df$EFT <- EFTs_df[[1]]
  ProdVar_average_df$rwnms <- as.numeric(rownames(EFTs_df))
  names(ProdVar_average_df)[1] <- "ProductivityVariable"

  ## Calculating 90-percentile by EFT ####
  ## EFT = 0 is NoData in the raster
  ProdVar_90perc <- as.data.table(ProdVar_average_df %>% group_by(EFT) %>% summarise_at(.vars = "ProductivityVariable", .funs = c("ProductivityVariable_90perc" = quantile), prob = 0.9, na.rm = TRUE))


  ## Assigning maximum (potential) productivity to outliers ####
  ## outliers:  value > percentile 90 intra-cluster

  ProdVar_average_df <- merge(as.data.table(ProdVar_average_df), ProdVar_90perc, by = "EFT", all.x = TRUE)
  setkeyv(ProdVar_average_df, "rwnms")

  cond <- ProdVar_average_df$ProductivityVariable > ProdVar_average_df$ProductivityVariable_90perc & !is.na(ProdVar_average_df$ProductivityVariable)
  ProdVar_average_df$ProductivityVariable[cond] <- ProdVar_average_df$ProductivityVariable_90perc[cond]
  #rm(cond)
  ProdVar_average_df$ProductivityVariable_90perc[is.na(ProdVar_average_df$ProductivityVariable)] <- NA

  ProdVar_average_df <- as.data.frame(ProdVar_average_df)
  ProdVar_average_df <- ProdVar_average_df[, !names(ProdVar_average_df) %in% "rwnms"]



  ## Calculating Local Scaled Productivity (LSP) ####
  ## Current status of efficiency of productivity: % over "potential", per pixel

  ProdVar_average_df$LSP <- round(((ProdVar_average_df$ProductivityVariable / ProdVar_average_df$ProductivityVariable_90perc) * 100), 1)
  names(ProdVar_average_df)[names(ProdVar_average_df) %in% "ProductivityVariable_90perc"] <- "PotentialProduction"


  ## Saving results ####
  gc()
  ProdVar <- ProdVar[[1]]

  LocalNetProductivity_rstr <- terra::setValues(ProdVar, ProdVar_average_df$LSP)
  names(LocalNetProductivity_rstr) <- "CurrentScaledNetProd"

  if (filename != "") writeRaster(LocalNetProductivity_rstr, filename = filename, overwrite = TRUE)
  return(LocalNetProductivity_rstr)

  #cellStats(LocalNetProductivity_rstr, max)
}
