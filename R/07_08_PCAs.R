## PCAs (Two-steps process) to prepare data for clustering

#'
#' @author Xavier Rotllan-Puig
#' @title PCAs4clust
#' @description PCAs4clust runs a two-steps process to prepare the data to be clustered
#' @details Firstly, a Principal Component Analysis ('screening PCA') with all the variables in
#' 'obj2process' is run in order to know the optimal number of variables to be used in a subsequent
#' PCA, as well as the most associated variable to those Principal Components (PCs). A threshold
#' of cumulative variance (cumul_var_threshold; default = 0.9) is needed. Secondly, a 'final PCA'
#' is run with the results of the 'screening PCA' (i.e. number of PC axes and their most associated
#' variables). PCAs4clust uses \code{\link[stats]{prcomp}} to run PCAs
#' @import raster
#' @importFrom stats na.omit varimax prcomp
#' @importFrom data.table rbindlist setorderv :=
#' @param obj2process Raster* object (or its file name). Each layer is one variable
#' @param cumul_var_threshold Numeric. Optional (default = 0.9). Threshold of cumulative variance
#' to select the number of useful PCs
#' @param filename Character. Output filename. Optional
#' @param ... Optional. Arguments for \code{\link[stats]{prcomp}}
#' @return RasterBrick object
#' @name PCAs4clust
#' @seealso \code{\link{rm_multicol}}; \code{\link[stats]{prcomp}}
#' @export
#' @examples
#' \donttest{
#' dirctry <- paste0(system.file(package='LPDynR'), "/extdata")
#' variables_noCor <- rm_multicol(dir2process = dirctry,
#'                                multicol_cutoff = 0.7)
#' PCAs4clust(obj2process = variables_noCor,
#'             cumul_var_threshold = 0.9)
#' }
#'


PCAs4clust <- function(obj2process = NULL,
                       cumul_var_threshold = 0.9,
                       filename = "",
                       ...){

  ## Reading in data (not correlated Phenolo variables)

  if(is.null(obj2process)) stop("Please provide an objects of classe Raster* (or a file names to read in from)")

  if(is.character(obj2process)){
    obj2process <- stack(obj2process)
  }else if(!class(obj2process) %in% c("RasterLayer", "RasterStack", "RasterBrick")){
    stop("Please provide objects of classe Raster* (or a file name to read in from)")
  }

  if(!is.numeric(cumul_var_threshold) |
     is.na(cumul_var_threshold) | is.null(cumul_var_threshold) |
     cumul_var_threshold < 0 | cumul_var_threshold > 1)
    stop("Please provide a threshold of cumulative variance (cumul_var_threshold) between 0 and 1")

  ## Performing the 'screening PCA'

  obj2process_df <- as.data.frame(obj2process)

  dts <- list(...)
  if(is.null(dts$retx)) dts$retx <- TRUE
  if(is.null(dts$center)) dts$center <- TRUE
  if(is.null(dts$scale.)) dts$scale. <- TRUE

  pca <- prcomp(na.omit(obj2process_df),
                retx   = dts$retx,
                center = dts$center, # variables are zero centered
                scale. = dts$scale.  # scaling variables to have unit variance
  )


  ## Rotating
  pca.rotated <- varimax(pca$rotation, normalize = TRUE)

  pca_importance <- summary(pca)
  pca_importance <- as.data.frame(pca_importance$importance)
  usefulPCs <- which(pca_importance[3 ,] >= cumul_var_threshold)[1]
  pca_importance <- pca_importance[, 1:usefulPCs]
  nPCs <- ncol(pca_importance)
  #if(nPCs > ncol(pca_importance)) nPCs <- ncol(pca_importance)

  screeningPCA_variables_df <- pca.rotated$loadings
  screeningPCA_variables_df <- as.data.frame(unclass(screeningPCA_variables_df))
  screeningPCA_variables_df <- round(screeningPCA_variables_df, 0)[, c(1:nPCs)]
  screeningPCA_variables <- names(which(apply(screeningPCA_variables_df, 1, sum) != 0))
  if(length(screeningPCA_variables) != nPCs) stop("inconsistency among number of variables selected for the next step ('final PCA') and number of PCs")



  ## Performing the 'Final PCA'

  obj2process_df <- obj2process_df[, names(obj2process_df) %in% screeningPCA_variables]

  pca_final <- prcomp(na.omit(obj2process_df),
                      retx   = dts$retx,
                      center = dts$center, # variables are zero centered
                      scale. = dts$scale.  # scaling variables to have unit variance
  )

  ## Spatial Patterns of PCs
  #if(exists("pca")) rm(pca)
  #if(exists("obj2process_df")) rm(obj2process_df)
  #gc()

  pca_final_rottd_varbles <- as.data.frame(pca_final$x)
  #if(exists("pca_final")) rm(pca_final)
  #gc()

  pca_final_rottd_varbles$rn <- as.integer(rownames(pca_final_rottd_varbles))
  num_pix <- obj2process@ncols * obj2process@nrows

  num_pix1 <- c(1:num_pix)
  num_pix1 <- num_pix1[!num_pix1 %in% as.integer(rownames(pca_final_rottd_varbles))]

  df2fill <- as.data.frame(matrix(NA, nrow = length(num_pix1), ncol = ncol(pca_final_rottd_varbles)))
  names(df2fill) <- c(paste0("PC", seq(1:(ncol(pca_final_rottd_varbles) - 1))), "rn")
  df2fill$rn <- num_pix1
  rownames(df2fill) <- num_pix1
  #rm(num_pix1)

  pca_final_raster1 <- rbindlist(list(pca_final_rottd_varbles, df2fill))
  #rm(df2fill)

  setorderv(pca_final_raster1, "rn")
  pca_final_raster1 <- pca_final_raster1[, rn := NULL]

  xtnt <- extent(obj2process)
  #pca_final_brick <- brick(nrows = obj2process@nrows, ncols = obj2process@ncols,
  #                         xmn = xtnt[1], xmx = xtnt[2], ymn = xtnt[3], ymx = xtnt[4],
  #                         crs = crs(obj2process),
  #                         nl = (ncol(pca_final_rottd_varbles) - 1)
  #)
  #rm(pca_final_rottd_varbles)
  pca_final_brick <- stack()
  for(i in 1:ncol(pca_final_raster1)){
    rastr_tmp <- raster(nrows = obj2process@nrows, ncols = obj2process@ncols,
                        xmn = xtnt[1], xmx = xtnt[2], ymn = xtnt[3], ymx = xtnt[4],
                        crs = crs(obj2process),
                        #ext,
                        #resolution,
                        vals = pca_final_raster1[[i]])
    #pca_final_brick[[i]] <- rastr_tmp
    pca_final_brick <- stack(pca_final_brick, rastr_tmp)
    names(pca_final_brick[[i]]) <- names(pca_final_raster1)[i]
  }
  pca_final_brick <- brick(pca_final_brick)

  ## Saving results
  if (filename != "") writeRaster(pca_final_brick, filename = filename, options = "INTERLEAVE=BAND", overwrite = TRUE)
  return(pca_final_brick)

}








