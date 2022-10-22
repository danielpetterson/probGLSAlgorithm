#' Read in data from .tem or .sst files from MK7, MK15 or C65-SUPER loggers
#'
#' Transforms .tem files into a data frame with
#'
#' @param temfile The name of a temperature log file downloaded from a geolocator device
#' @param logger.type The model of light-level geolocator. BAS MK7, MK15 and Migrate Technology C65-SUPER are supported.
#' @param temp.range Vector containing to lowest and highest temperature values. Migrate technology loggers provide min and max temperatures for each eight hour interval.
#'
#' @import dplyr
#'
#' @export

read.sensor <- function(temfile, logger.type = "MK15", temp.range = c(-2,19)) {

  if (!grepl(".tem$", temfile) & !grepl(".sst$", temfile)) stop(paste("Auxillary temperature data file should be of .tem or .sst file type."))

  if (logger.type %in% c("MK7", "MK15")) {
    td <- read.csv(temfile, header = FALSE)
    colnames(td) <- c("X1", "dtime", "X3", "IntTemp")
    td <- td[!is.na(td$IntTemp),]
    td$dtime <- as.POSIXct(td$dtime, format = "%d/%m/%y %H:%M:%OS")
    sst <- sst_deduction(datetime = td$dtime, temp = td$IntTemp, temp.range)
  } else if (logger.type == "C65-SUPER") {
    td <- read.csv(temfile, header = TRUE, skip = 19, sep = "\t")
    td$dtime <- as.POSIXct(td[, 1], format = "%d/%m/%Y %H:%M:%OS")
    sst <- sst_deduction(datetime = td[, 1], temp = td[, 2], temp.range = c(td[, 2], td[, 3]))
  } else {
    stop(paste("logger.type not supported."))
  }

  return(sst[sst$SST.remove==FALSE,])
}
