#### Ecosystem Functional Types: Clustering

#'
#' @author Xavier Rotllan-Puig
#' @title EFT_clust
#' @description EFT_clust derives the Ecosystem Functional Types using K-means to perform a clustering on the
#' pixels of the Raster* object
#' @details kmeans() does not optimize the final number of clusters. It needs to be set by means of n_clust
#' (default = 20). There are several methods and statistics to determine the optimal number. clust_optim()
#' produces a scree plot to help the user to decide the optimal number of clusters. In this case, kmeans()
#' uses as default iter.max = 500 and algorithm = "MacQueen", but these can be modified passing the arguments
#' throuh ...
#' @import raster
#' @importFrom dplyr bind_rows
#' @importFrom stats kmeans
#' @param obj2clust RasterStack or RasterBrick object (or its file name). Each layer is one variable
#' @param n_clust Numeric. Number of total clusters. Optional. Default = 20
#' @param filename Character. Output filename. Optional
#' @param ... Arguments for kmeans(). Optional
#' @return RasterLayer object
#' @name EFT_clust()
#' @export
#' @examples
#' \dontrun{
#' LPDynR:::EFT_clust(obj2clust = EFTs_raster,
#'                    n_clust = 20)
#' }
#'



EFT_clust <- function(obj2clust = NULL,
                      n_clust = 20,
                      filename = "",
                      ...){

  ## Reading in data

  if(is.null(obj2clust)) stop("Please provide objects of classe Raster* (or file names to read in some)")

  if(is.character(obj2clust)){
    obj2clust <- stack(obj2clust)
  }else if(!class(obj2clust) %in% c("RasterLayer", "RasterStack", "RasterBrick")){
    stop("Please provide objects of classe Raster* (or a file name to read in from)")
  }

  if(!is.numeric(n_clust) | is.na(n_clust) | is.null(n_clust))
    stop("Please provide a number of clusters")


  obj2clust_ini <- as.data.frame(obj2clust)
  obj2clust_ini$rn <- 1:nrow(obj2clust_ini)

  obj2clust_ini_NA <- obj2clust_ini[!complete.cases(obj2clust_ini), ]   #to be used at the end to fill the raster
  obj2clust_ini_NA$clstr <- NA
  obj2clust_ini_NA$clstr <- as.integer(obj2clust_ini_NA$clstr)
  obj2clust_ini_NA <- obj2clust_ini_NA[, names(obj2clust_ini_NA) %in% c("clstr", "rn")]

  obj2clust_ini <- obj2clust_ini[complete.cases(obj2clust_ini), ]


  ## Clustering using optimal number of clusters
  dts <- list(...)
  if(is.null(dts$nstart)) dts$nstart <- 1
  if(is.null(dts$iter.max)) dts$iter.max <- 500
  if(is.null(dts$algorithm)) dts$algorithm <- "MacQueen"

  kmeans_clustring <- kmeans(obj2clust_ini[, - c(length(obj2clust_ini))],
                             centers = n_clust,
                             nstart = dts$nstart,
                             iter.max = dts$iter.max,
                             algorithm = dts$algorithm
                             )


  ## Binding NA data and adding back spatial information ####
  obj2clust_ini$clstr <- kmeans_clustring$cluster
  obj2clust_ini <- obj2clust_ini[, names(obj2clust_ini) %in% c("clstr", "rn")]

  all_data <- bind_rows(obj2clust_ini, obj2clust_ini_NA)
  rm(obj2clust_ini, obj2clust_ini_NA)
  #gc()

  all_data <- all_data[order(all_data$rn), ]

  #obj2clust1 <- obj2clust[[1]]
  EFTs_raster <- raster(nrows = obj2clust@nrows, ncols = obj2clust@ncols,
                        crs = crs(obj2clust),
                        ext = extent(obj2clust),
                        #resolution,
                        vals = all_data$clstr)

  names(EFTs_raster) <- "clusterNum"

  ## Saving results
  if (filename != "") writeRaster(EFTs_raster, filename = filename, overwrite = TRUE)
  return(EFTs_raster)

}


