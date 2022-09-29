#' Define land mask used in prob.GLS.algorithm
#'
#' Extends land mask found in NOAA OI SST V2 file. Land masks mark the boundaries where position estimates can not be made.
#'
#' @param med.sea if T classify mediterranean sea as land
#' @param black.sea if T classify black sea as land
#' @param baltic.sea if T classify baltic sea as land
#' @param caspian.sea if T classify caspian sea as land
#' @param arctic.ocean if T classify arctic ocean as land
#' @param north.atlantic.ocean if T classify north atlantic ocean as land
#' @param south.atlantic.ocean if T classify south atlantic ocean as land
#' @param north.pacific.ocean if T classify north pacific ocean as land
#' @param south.pacific.ocean if T classify south pacific ocean as land
#' @param southern.ocean if T classify southern ocean as land
#' @param custom.land.mask List containing vectors of length four that define custom areas to include in land mask. The format is minimum.longitude, maximum.longitude, minimum latitude, maximum latitude.
#' @param NOAA.OI.location The location of the NOAA OI SST V2 land mask file relative to the working directory.
#'
#'
#' @export

modify.land.mask <- function (med.sea = F,
                              black.sea = F,
                              baltic.sea = F,
                              caspian.sea = F,
                              arctic.ocean = F,
                              north.atlantic.ocean = F,
                              south.atlantic.ocean = F,
                              north.pacific.ocean = F,
                              south.pacific.ocean = F,
                              southern.ocean = F,
                              custom.land.mask = NULL
)
{
  if (!is.logical(med.sea) | !length(med.sea) == 1) stop(paste("med.sea must be either TRUE or FALSE."))
  if (!is.logical(black.sea) | !length(black.sea) == 1) stop(paste("black.sea must be either TRUE or FALSE."))
  if (!is.logical(baltic.sea) | !length(baltic.sea) == 1) stop(paste("baltic.sea must be either TRUE or FALSE."))
  if (!is.logical(caspian.sea) | !length(caspian.sea) == 1) stop(paste("caspian.sea must be either TRUE or FALSE."))
  if (!is.logical(arctic.ocean) | !length(arctic.ocean) == 1) stop(paste("arctic.ocean must be either TRUE or FALSE."))
  if (!is.logical(north.atlantic.ocean) | !length(north.atlantic.ocean) == 1) stop(paste("north.atlantic.ocean must be either TRUE or FALSE."))
  if (!is.logical(south.atlantic.ocean) | !length(south.atlantic.ocean) == 1) stop(paste("south.atlantic.ocean must be either TRUE or FALSE."))
  if (!is.logical(north.pacific.ocean) | !length(north.pacific.ocean) == 1) stop(paste("north.pacific.ocean must be either TRUE or FALSE."))
  if (!is.logical(south.pacific.ocean) | !length(south.pacific.ocean) == 1) stop(paste("south.pacific.ocean must be either TRUE or FALSE."))
  if (!is.logical(southern.ocean) | !length(southern.ocean) == 1) stop(paste("southern.ocean must be either TRUE or FALSE."))
  if (!(is.null(custom.land.mask) | is.list(custom.land.mask))) stop(paste("custom.land.mask must be a list of vectors. e.g. list(c(9,8,7,6),c(1,2,3,4)."))
  if (!is.null(custom.land.mask) & !all(lengths(custom.land.mask) == 4)) stop(paste("custom.land.mask must be a list of vectors of length 4 or NULL"))

  landmaskdf <- data.frame(matrix(ncol = 4, nrow = 0))

  if (baltic.sea == T)
    landmaskdf <- rbind(landmaskdf, list(14, 33.5, 51.4, 66.2))
  if (med.sea == T) {
    landmaskdf <- rbind(landmaskdf, list(0, 27, 30, 48))
    landmaskdf <- rbind(landmaskdf, list( 0,27, 30 , 48))
    landmaskdf <- rbind(landmaskdf, list( 27, 40, 30 , 40))
    landmaskdf <- rbind(landmaskdf, list(355,360, 30 , 42))
  }
  if (black.sea == T)
    landmaskdf <- rbind(landmaskdf, list(27, 45, 40 , 48))
  if (caspian.sea == T)
    landmaskdf <- rbind(landmaskdf, list(45, 62,  35 , 48))
  if (arctic.ocean == T) {
    landmaskdf <- rbind(landmaskdf, list(-180, 180, 51, 90))
  }
  if (north.atlantic.ocean == T) {
    landmaskdf <- rbind(landmaskdf, list(-77, 20, 0, 8))
    landmaskdf <- rbind(landmaskdf, list(-83, 20, 8, 14))
    landmaskdf <- rbind(landmaskdf, list(-92, 20, 14, 17))
    landmaskdf <- rbind(landmaskdf, list(-100, 20, 17, 60))
  }
  if (south.atlantic.ocean == T) {
    landmaskdf <- rbind(landmaskdf, list(-70, 20, -60, 0))
  }
  if (north.pacific.ocean == T) {
    landmaskdf <- rbind(landmaskdf, list(117, -67, 0, 9))
    landmaskdf <- rbind(landmaskdf, list(117, -83.6, 9, 15))
    landmaskdf <- rbind(landmaskdf, list(117, -92, 15, 18))
    landmaskdf <- rbind(landmaskdf, list(117, -100, 18, 66.6))
  }
  if (south.pacific.ocean == T)
    landmaskdf <- rbind(landmaskdf, list(130, -67, -60, 0))
  if (southern.ocean == T)
    landmaskdf <- rbind(landmaskdf, list(-180, 180, -90, -60))
  if (!is.null(custom.land.mask)) {
    for(i in 1:length(custom.land.mask)) {
      landmaskdf <- rbind(landmaskdf, unlist(custom.land.mask[i]))
    }
  }

  colnames(landmaskdf) <- c('min.lon', 'max.lon', 'min.lat', 'max.lat')

  return(landmaskdf)
}
