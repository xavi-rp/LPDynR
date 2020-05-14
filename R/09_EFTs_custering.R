#### Ecosystem Functional Types: Clustering

#'
#' @author Xavier Rotllan-Puig
#' @title EFT_clust
#' @description EFT_clust
#' @details
#' @import raster
#' @importFrom
#' @importFrom
#' @param ProdVar Raster* object (or its file name). Productivity variable (e.g. Cyclic fraction -season growth-)
#' @param filename Character. Output filename. Optional
#' @param cores2use Numeric. Number of cores to use for parallelization. Optional. Default is 1 (no parallelization)
#' @return RasterLayer object
#' @name EFT_clust()
#' @export
#' @examples
#' \dontrun{
#' LPDynR:::EFT_clust(EFTs = EFTs_raster,
#'                    ProdVar = ProdVar_brick)
#' }
#'



EFT_clust <- function(obj2clust = NULL,
                      n_clust = 20){


  ## Reading in data from 'final PCA' (Step 08) ####

  if(any(is.null(obj2clust), is.null())) stop("Please provide objects of classe Raster* (or file names to read in some)")

  if(is.character(obj2clust)){
    obj2clust <- stack(obj2clust)
  }else if(!class(obj2clust) %in% c("RasterLayer", "RasterStack", "RasterBrick")){
    stop("Please provide objects of classe Raster* (or a file name to read in from)")
  }


  #obj2clust <- stack(paste0(path2tempResults, "/pca_final_raster.tif"))
  obj2clust_ini <- as.data.frame(obj2clust)
  obj2clust_ini$rn <- 1:nrow(obj2clust_ini)

  obj2clust_ini_NA <- obj2clust_ini[!complete.cases(obj2clust_ini), ]   #to be used at the end to fill the raster
  obj2clust_ini_NA$clstr <- NA
  obj2clust_ini_NA$clstr <- as.integer(obj2clust_ini_NA$clstr)
  obj2clust_ini_NA <- obj2clust_ini_NA[, names(obj2clust_ini_NA) %in% c("clstr", "rn")]


  obj2clust_ini <- obj2clust_ini[complete.cases(obj2clust_ini), ]



  ## Clustering using optimal number of clusters

  t0 <- Sys.time()
  kmeans_clustring <- kmeans(obj2clust_ini[, - c(length(obj2clust_ini))],
                             centers = 20,
                             #iter.max = 10,  # Warning: did not converge in 10 iterations
                             #iter.max = 50,   # Warning: did not converge in 50 iterations
                             #iter.max = 100,  # Warning: did not converge in 100 iterations (~ 15 minutes)
                             iter.max = 500,
                             nstart = 1,
                             algorithm = "MacQueen"
  )
  t1 <- Sys.time() - t0
  save(list = c("kmeans_clustring", "t1"), file = paste0(path2tempResults, "/results_Step9_kmeans.RData"))
  #load(paste0(path2tempResults, "/results_Step9_kmeans.RData"), verbose = TRUE)




  ## Binding NA data and adding back spatial information ####

  #pca_final_raster1 <- pca_final_raster

  obj2clust_ini$clstr <- kmeans_clustring$cluster
  #obj2clust_ini$rn <- as.numeric(rownames(obj2clust_ini))
  obj2clust_ini <- obj2clust_ini[, names(obj2clust_ini) %in% c("clstr", "rn")]



  all_data <- bind_rows(obj2clust_ini, obj2clust_ini_NA)#, data_ini_4later)
  rm(obj2clust_ini, obj2clust_ini_NA)
  #gc()


  #all_data <- all_data %>%
  #             as_tibble() %>%
  #             arrange(rn) %>%
  #             column_to_rownames(var = "rn")

  all_data <- all_data[order(all_data$rn), ]
  #rownames(all_data) <- all_data$rn



  obj2clust <- raster(paste0(path2tempResults, "/pca_final_raster.tif"))

  xtnt <- extent(obj2clust)
  pca_final_clstrs_raster <- raster(nrows = obj2clust@nrows, ncols = obj2clust@ncols,
                                    crs = crs(obj2clust),
                                    ext = xtnt,
                                    #resolution,
                                    vals = all_data$clstr)

  names(pca_final_clstrs_raster) <- "clusterNum"






  ## Plotting clusters  ####
  #pca_final_clstrs_raster <- raster(paste0(path2tempResults, "/SpatialPatternsPCs_clstrs.tif"))

  #rning_plts <- "y"
  #rning_plts <- "n"
  if(rning_plts == "y"){
    jpeg(paste0(path2saveTests, "/clusters_EFTs.jpg"), width = 28, height = 20, units = "cm",
         #res = 600, pointsize = 8)
         res = 300)

    par(mar = c(3, 3, 0, 0), bty = "n")

    pal <- colorRampPalette(c("mistyrose1", "pink1", "violet", "blue", "skyblue2", "green", "darkolivegreen2", "yellow", "orange", "tomato3", "brown4", "coral4"))
    par(xpd = FALSE)
    plot(pca_final_clstrs_raster,
         col = pal(pca_final_clstrs_raster@data@max),
         legend = FALSE#,
         #main = paste0((length(unique(all_data$clstr)) - 1), " clusters")
    )
    legend("bottom",
           #x = - 30, y = 30,
           title = paste0( "Number of clusters = ", pca_final_clstrs_raster@data@max),
           ncol = ceiling(pca_final_clstrs_raster@data@max / 2),
           legend = c(1:pca_final_clstrs_raster@data@max),
           fill = pal(pca_final_clstrs_raster@data@max),
           inset = 0.02
    )

    title(main = "Ecosystem Functional Types (EFTs)",
          outer = TRUE,
          #adj = 0,
          line = - 4,
          cex.main = 2)

    #mtext(paste0( "Number of clusters = ", pca_final_clstrs_raster@data@max),
    #      side = 1, line = - ,
    #      #at = 5,
    #      adj = 0,
    #      cex = 0.8)

    dev.off()

  }






  ## Saving results ####


  #quick plot to check
  #jpeg(paste0(path2saveTests, "\\SpatialPatternsPCs_clstrs.jpg"))
  #plot(pca_final_clstrs_raster[["new_clst"]])
  #dev.off()


  #stuff2save <- c("data_centroids", "obj2clust_ini_clusters", "pca_final_clstrs_raster")
  pca_final_clstrs <- all_data
  stuff2save <- c("pca_final_clstrs", "pca_final_clstrs_raster")
  save(list = stuff2save, file = paste0(path2tempResults, "/results_Step9.RData"))
  writeRaster(pca_final_clstrs_raster, paste0(path2tempResults, "/SpatialPatternsPCs_clstrs.tif"), overwrite = TRUE)
  #load(file = paste0(path2tempResults, "/results_Step9.RData"), verbose = TRUE)
  #all_data <- pca_final_clstrs
  #pca_final_clstrs_raster <- raster(paste0(path2tempResults, "/SpatialPatternsPCs_clstrs.tif"))


}


