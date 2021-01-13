#' Standing Biomass
#'
#' RasterBrick object containing time series of land productivity data (400 cells; 10 layers).
#' Years 2000-2009. The variable has been derived from MODIS using Timesat
#'
#' Downloaded from www.eea.europa.eu (20/08/2020)
#'
#' Coord. ref. : +proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs
#'
#' Resolution  : 500, 500  (x, y)
#'
#' Extent      : 3640000, 3650000, 2140000, 2150000  (xmin, xmax, ymin, ymax)
#'
#'
#' @docType data
#' @examples raster::brick(paste0(system.file(package='LPDynR'), "/extdata/sb_cat.tif"))
#'
#' @format GeoTIFF. RasterBrick object with dimensions: 20, 20, 400, 10  (nrow, ncol, ncell, nlayers)
#' \describe{
#'   \item{sb_cat}{Standing biomass}
#' }
#'
#' @references
#' \url{https://www.eea.europa.eu/data-and-maps/data/annual-above-ground-vegetation-productivity}
#'
#' @source \url{https://www.eea.europa.eu/data-and-maps}
"sb_cat"


#' Season Beginning Day
#'
#' RasterBrick object containing time series of phenological data (400 cells; 10 layers).
#' Years 2000-2009. The variable has been derived from MODIS using Timesat
#'
#' Downloaded from www.eea.europa.eu (20/08/2020)
#'
#' Coord. ref. : +proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs
#'
#' Resolution  : 500, 500  (x, y)
#'
#' Extent      : 3640000, 3650000, 2140000, 2150000  (xmin, xmax, ymin, ymax)
#'
#'
#' @docType data
#' @examples raster::brick(paste0(system.file(package='LPDynR'), "/extdata/sbd_cat.tif"))
#'
#' @format GeoTIFF. RasterBrick object with dimensions: 20, 20, 400, 10  (nrow, ncol, ncell, nlayers)
#' \describe{
#'   \item{sbd_cat}{Season beginning day}
#' }
#' @references
#' \url{https://www.eea.europa.eu/data-and-maps/data/annual-start-of-vegetation-growing}
#'
#' @source \url{https://www.eea.europa.eu/data-and-maps}
"sbd_cat"


#' Season Length
#'
#' RasterBrick object containing time series of phenological data (400 cells; 10 layers).
#' Years 2000-2009. The variable has been derived from MODIS using Timesat
#'
#' Downloaded from www.eea.europa.eu (20/08/2020)
#'
#' Coord. ref. : +proj=laea +lat_0=52 +lon_0=10 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +units=m +no_defs
#'
#' Resolution  : 500, 500  (x, y)
#'
#' Extent      : 3640000, 3650000, 2140000, 2150000  (xmin, xmax, ymin, ymax)
#'
#'
#' @docType data
#' @examples raster::brick(paste0(system.file(package='LPDynR'), "/extdata/sl_cat.tif"))
#'
#' @format GeoTIFF. RasterBrick object with dimensions: 20, 20, 400, 10  (nrow, ncol, ncell, nlayers)
#' \describe{
#'   \item{sl_cat}{Season Length}
#' }
#' @references
#' \url{https://www.eea.europa.eu/data-and-maps/data/annual-above-ground-vegetation-season}
#'
#' @source \url{https://www.eea.europa.eu/data-and-maps}
"sl_cat"

