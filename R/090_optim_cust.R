## Optimal number of clusters

#'
#' @author Xavier Rotllan-Puig
#' @title clust_optim
#' @description clust_optim produces a scree plot with number of cluster at x-axis and
#' total within-cluster sum of squares at y-axis
#' @details The 'scree plot method' allows the user to assess how the quality of the
#' K-means clustering improves when increasing the number of clusters. An elbow in the curve
#' indicates the optimal number of clusters
#' @import raster
#' @importFrom stats kmeans
#' @param obj2clust RasterStack or RasterBrick object (or its file name). Each layer is one variable
#' @param num_clstrs Numeric. Optional. Vector with a sequence of number of clusters to check for optimal
#' @param ... Optional. Arguments for kmeans()
#' @return A scree plot
#' @name clust_optim()
#' @export
#' @examples
#' \dontrun{
#' LPDynR:::clust_optim(obj2clust = obj2clust_raster,
#'                      num_clstrs = seq(5, 50, 5))
#' }
#'



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

  if(!is.numeric(num_clstrs) | is.na(num_clstrs) | is.null(num_clstrs))
    stop("Please provide a sequence of number of clusters to check for optimal")


  obj2clust <- as.data.frame(obj2clust)
  obj2clust <- obj2clust[complete.cases(obj2clust), ]


  ## K-means
  wss <- (nrow(obj2clust) - 1) * sum(apply(obj2clust[, - c(length(obj2clust))], 2, var))

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

