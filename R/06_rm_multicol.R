## Removing Multicollinearity Of Variables

#'
#' @author Xavier Rotllan-Puig
#' @title rm_multicol
#' @description rm_multicol calculates the average of each variable and removes those variables highly correlated
#' @details Firstly, after reading in all .tif files in 'dir2process', if they are multiband (time series), averages are
#' calculated. Secondly, it creates a RasterBrick object with those (averaged) variables which have a Pearson's
#' correlation coefficient below 'multicol_cutoff'. See \code{\link[virtualspecies]{removeCollinearity}} for further arguments
#' and functionalities
#' @import raster parallel virtualspecies
#' @param dir2process Character. Directory where the Raster* objects are stored. All the .tif
#' files in the directory will be read in to be used
#' @param multicol_cutoff Numeric. Cutoff value of (Pearson's) correlation. Optional. Default is 0.70
#' @param cores2use Numeric. Number of cores to use for parallelization. Optional. Default is 1 (no parallelization)
#' @param filename Character. Output filename. Optional
#' @param ... Optional. Arguments for removeCollinearity()
#' @return RasterBrick object
#' @name rm_multicol
#' @seealso \code{\link[virtualspecies]{removeCollinearity}}
#' @references Leroy B, Meynard CN, Bellard C, Courchamp F (2015). “virtualspecies, an R package to generate virtual species distributions”. Ecography. doi: 10.1111/ecog.01388
#' @export
#' @examples
#' \donttest{
#' dirctry <- paste0(system.file(package='LPDynR'), "/extdata")  # directory with variables to process
#' rm_multicol(dir2process = dirctry,
#'             multicol_cutoff = 0.7,
#'             plot = TRUE)
#' }

rm_multicol <- function(dir2process = NULL,
                        multicol_cutoff = 0.70,
                        cores2use = 1,
                        filename = "",
                        ...){

  ## Reading in data (Phenologivcal/productivity variables) and averaging
  if(!is.character(dir2process) | is.na(dir2process) | is.null(dir2process) |
     !dir.exists(dir2process))
    stop("Please provide a character vector where the .tif files can be read in from")

  varbles <- dir(path = dir2process,
                 pattern = ".tif$", full.names = TRUE)

  if(length(varbles) == 0) stop(paste0("No .tif files in ", dir2process))
  #if(length(varbles) > 0) cat(paste0(varbles, "... being processed.", "\n"))


  vrbles <- c()
  stack_rstrs_avg <- stack()

  for (v in varbles) {
    var2process <- brick(v)
    rstr_name <- unlist(strsplit(v, "/"))
    rstr_name <- rstr_name[length(rstr_name)]
    rstr_name <- sub(".tif", "", rstr_name)

    ## Calculating averages
    # Average is calculated over ALL the available years, but this might be included as an argument
    beginCluster(cores2use)   # it uses n - 1 clusters
    yrs <- c()
    yrs <<- 1:nlayers(var2process)
    rstr_average <- clusterR(var2process, calc, args = list(fun = mean_years_function), export = "yrs")
    endCluster()

    names(rstr_average) <- paste0(rstr_name, "_avrge")
    vrbles <- c(vrbles, rstr_name)

    stack_rstrs_avg <- stack(stack_rstrs_avg, rstr_average)
  }

  ## Multicollinearity
  dts <- list(...)
  if(is.null(dts$select.variables)) dts$select.variables <- TRUE
  if(is.null(dts$sample.points)) dts$sample.points <- TRUE
  #if(is.null(dts$nb.points)) dts$nb.points <- 1000000
  if(is.null(dts$nb.points)) dts$nb.points <- ceiling((stack_rstrs_avg@nrows * stack_rstrs_avg@ncols) * 10 / 100)
  if(is.null(dts$plot)) dts$plot <- FALSE

  vrbles_NoC <- removeCollinearity(stack_rstrs_avg,
                                   multicollinearity.cutoff = multicol_cutoff,  # it uses Pearson's R
                                   select.variables = dts$select.variables,  # if TRUE, randomly select one variable of the group. If FALSE, returns a list with the groups
                                   sample.points = dts$sample.points,  # using nb.points to calculate multicollinearity
                                   nb.points = dts$nb.points,
                                   plot = dts$plot)

  # Removing correlated variables
  variables_avg_noC <- brick(stack_rstrs_avg@layers[names(stack_rstrs_avg) %in% vrbles_NoC])


  ## Saving results
  #rm(list = c("yrs"), envir = globalenv())
  if (filename != "") writeRaster(variables_avg_noC, filename = filename, options = "INTERLEAVE=BAND", overwrite = TRUE)
  return(variables_avg_noC)

}

