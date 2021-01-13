## Optimal number of clusters

#'
#' @author Xavier Rotllan-Puig
#' @title clust_optim
#' @description clust_optim produces a scree plot with number of cluster at x-axis and
#' total within-cluster sum of squares at y-axis
#' @details The 'scree plot method' allows the user to assess how the quality of the
#' K-means clustering improves when increasing the number of clusters. An elbow in the curve
#' indicates the optimal number of clusters. K-means are run with \code{\link[stats]{kmeans}}
#' @import raster
#' @importFrom stats kmeans complete.cases var
#' @param obj2clust RasterStack or RasterBrick object (or its file name). Each layer is one variable
#' @param num_clstrs Numeric. Optional. Vector with a sequence of number of clusters to check for optimal
#' @param ... Optional. Arguments for \code{\link[stats]{kmeans}}
#' @return A scree plot
#' @name clust_optim
#' @seealso \code{\link{PCAs4clust}}; \code{\link{EFT_clust}}; \code{\link[stats]{kmeans}}
#' @export
#' @examples
#' \donttest{
#' dirctry <- paste0(system.file(package='LPDynR'), "/extdata")
#' variables_noCor <- rm_multicol(dir2process = dirctry,
#'                                multicol_cutoff = 0.7)
#' clust_optim(obj2clust = variables_noCor,
#'             num_clstrs = seq(5, 50, 5))
#'}



clust_optim <- function(obj2clust = NULL,
                        num_clstrs = seq(5, 50, 5),
                        ...){

  if(is.null(obj2clust))
    stop("Please provide an objects of classe RasterStack or RasterBrick (or a file name to read in from)")

  if(is.character(obj2clust)){
    obj2clust <- stack(obj2clust)
  }else if(!class(obj2clust) %in% c("RasterLayer", "RasterStack", "RasterBrick")){
    stop("Please provide objects of classe Raster* (or a file name to read in from)")
  }

  if(!is.numeric(num_clstrs) | any(is.na(num_clstrs)) | is.null(num_clstrs))
    stop("Please provide a sequence of number of clusters to check for optimal")


  obj2clust <- as.data.frame(obj2clust)
  obj2clust <- obj2clust[complete.cases(obj2clust), ]


  ## K-means
  wss <- (nrow(obj2clust) - 1) * sum(apply(as.data.frame(obj2clust[, - c(length(obj2clust))]), 2, var))

  dts <- list(...)
  if(is.null(dts$nstart)) dts$nstart <- 1
  if(is.null(dts$iter.max)) dts$iter.max <- 10
  if(is.null(dts$algorithm)) dts$algorithm <- "MacQueen"

  for (i in 2:(length(num_clstrs) + 1)) wss[i] <- kmeans(obj2clust[, - c(length(obj2clust))],
                                                         centers = num_clstrs[i - 1],
                                                         nstart = dts$nstart,
                                                         iter.max = dts$iter.max,
                                                         algorithm = dts$algorithm)$tot.withinss

  wss_plot <- plot(c(1, num_clstrs), wss, type = "b",
                   xlab = "Number of Clusters",
                   ylab = "Total within-cluster sum of squares")

  return(wss_plot)


}

