#' Probabilistic algorithm for geolocation using light-level loggers
#'
#' Algorithm for more accurately tracking the migration of species that cross the equator relative to the probGLS algorithm
#'
#' @param particle.number number of particles for each location cloud used in the model
#' @param iteration.number number of iterations
#' @param loess.quartile quartiles for loessFilter (GeoLight), if NULL loess filter is not used
#' @param tagging.location tagging location longitude and latitude
#' @param tagging.date deployment data as POSIXct or Date object
#' @param retrieval.date  retrieval date as POSIXct or Date object
#' @param sunrise.sd output vector from twilight_error_estimation
#' @param sunset.sd output vector from twilight_error_estimation
#' @param range.solar min and max of solar angle range in degree
#' @param speed.dry optimal speed, speed standard deviation and max speed allowed if logger is dry in m/s
#' @param speed.wet optimal speed, speed standard deviation and max speed allowed if logger is wet in m/s
#' @param sst.sd SST standard deviation in degree C
#' @param max.sst.diff max difference in SST allowed in degree C
#' @param days.around.spring.equinox days before the Spring equinox and days after the Spring equinox. The Spring equinox is assumed constant at 20 March.
#' @param days.around.fall.equinox days before the Fall equinox, days after the Fall equinox. The Fall equinox is assumed constant at 22 September.
#' @param ice.conc.cutoff max percentage of sea ice in which the animal is believed to be
#' @param boundary.box min lon, max lon, min lat and max lat of extrem boundary where you expect an animal to be
#' @param land.mask if T animal is only using ocean areas, if F animal is only using land areas, if NULL no land mask used
#' @param east.west.comp if T apply biotrack east west movement compensation (Biotrack manual v11 page 31pp.)
#' @param sensor data.frame with daily SST data deduced from tag temperature readings (sst_deduction ouput), NULL if no SST data is available (SST will not be used)
#' @param trn data.frame containing twilights and at least tFirst, tSecond and type (same as computed by trn_to_dataframe, ipe_to_dataframe or lotek_to_dataframe)
#' @param act data.frame containing wet dry data (e.g. .act file from Biotrack loggers or .deg file from migrate tech loggers), NULL if no wetdry data is available (algorithm will assume that the logger was always dry)
#' @param wetdry.resolution sampling rate of conductivity switch in sec (e.g. MK15 & MK3006 sample every 3 sec).
#' @param backward run algorithm in reverse.
#' @param NOAA.OI.location directory location of NOAA OI V2 NCDF files as well as land mask file 'lsmask.oisst.v2.nc' (downloadable from http://www.esrl.noaa.gov/psd/data/gridded/data.noaa.oisst.v2.highres.html).
#' @param land.mask.mod dataframe containing the geographic bounds of any modifications to the land mask.
#' @return A list with: [1] all positions, [2] geographic median positions, [3] all possible particles, [4] input parameters, [5] model run time, [6] list of location estimates plots per timestep. List items 1 to 3 are returned as SpatialPointsDataframe.
#'
#' @import ncdf4
#' @import geosphere
#' @import GeoLight
#' @import probGLS
#' @import sp
#' @import ggplot2
#' @importFrom raster raster
#' @importFrom terra rast rotate
#' @importFrom tidyterra geom_spatraster
#' @importFrom stats rlnorm dnorm step median
#'
#' @export

GLS.prob.algorithm <- function (particle.number = 2000,
                                iteration.number = 60,
                                loess.quartile = NULL,
                                tagging.location = c(0, 0),
                                tagging.date,
                                retrieval.date,
                                sunrise.sd = twilight_error_estimation(2.49, 0.94, 4.98),
                                sunset.sd = twilight_error_estimation(2.49, 0.94,4.98),
                                range.solar = c(-7,-1),
                                speed.wet = c(20, 0.2, 25),
                                speed.dry = c(20, 0.2, 25),
                                sst.sd = 0.5,
                                max.sst.diff = 3,
                                days.around.spring.equinox = c(10, 10),
                                days.around.fall.equinox = c(10,10),
                                ice.conc.cutoff = 1,
                                boundary.box = c(-180, 180,-90, 90),
                                east.west.comp = T,
                                land.mask = T,
                                sensor,
                                trn,
                                act,
                                wetdry.resolution = 30,
                                backward = F,
                                NOAA.OI.location = NULL,
                                land.mask.mod = NULL)
{

  if (!length(particle.number) == 1 | particle.number <= 0 | !is.numeric(particle.number)) stop(paste("particle.number should be an int greater than zero."))
  if (!length(iteration.number) == 1 | iteration.number <= 0 | !is.numeric(iteration.number)) stop(paste("iteration.number should be an int greater than zero."))
  if (!length(tagging.location) == 2 | tagging.location[1] < -180 | tagging.location[1] > 180 | tagging.location[2] < -90 | tagging.location[2] > 90 | !is.numeric(tagging.location)) stop(paste("tagging.location needs to be (longitude, latitude) representing the initial location."))
  if (!length(sunrise.sd) == 5 | !is.numeric(sunrise.sd)) stop(paste("sunrise.sd should be an output vector from twilight_error_estimation"))
  if (!length(sunset.sd) == 5 | !is.numeric(sunset.sd)) stop(paste("sunset.sd should be an output vector from twilight_error_estimation."))
  if (!length(range.solar) == 2 | !is.numeric(range.solar)) stop(paste("range.solar should be a vector representing the min and max of solar angle range in degrees."))
  if (!length(speed.wet) == 3 | !is.numeric(speed.wet)) stop(paste("speed.wet should be a vector representing optimal speed, speed standard deviation and max speed allowed if logger is wet in m/s"))
  if (!length(speed.dry) == 3 | !is.numeric(speed.dry)) stop(paste("speed.dry should be a vector representing optimal speed, speed standard deviation and max speed allowed if logger is dry in m/s"))
  if (!length(sst.sd) == 1 | !is.numeric(sst.sd) | sst.sd < 0) stop(paste("sst.sd should be a non-negative number of length one."))
  if (!length(max.sst.diff) == 1 | !is.numeric(max.sst.diff) | max.sst.diff < 0) stop(paste("max.sst.diff should be a number equal to or greater than zero."))
  if (!length(days.around.spring.equinox) == 2 | !is.numeric(days.around.spring.equinox) | min(days.around.spring.equinox) < 0) stop(paste("days.around.spring.equinox should be a numeric vector of length two with all elements equal to or greater than zero."))
  if (!length(days.around.fall.equinox) == 2 | !is.numeric(days.around.fall.equinox) | min(days.around.fall.equinox) < 0) stop(paste("days.around.fall.equinox should be a numeric vector of length two with all elements equal to or greater than zero."))
  if (!length(ice.conc.cutoff) == 1 | ice.conc.cutoff < 0 | ice.conc.cutoff > 1 | !is.numeric(ice.conc.cutoff)) stop(paste("ice.conc.cutoff should be an int greater than zero and less than one."))
  if (!length(boundary.box) == 4 | !is.numeric(boundary.box)) stop(paste("boundary.box should be a numeric vector of length four i.e.(min lon, max lon, min lat, max lat)"))
  if (!length(wetdry.resolution) == 1 | wetdry.resolution <= 0 | !is.numeric(wetdry.resolution)) stop(paste("wetdry.resolution should be be an int greater than zero"))
  if (!length(NOAA.OI.location) == 1 | !is.character(NOAA.OI.location)) stop(paste("NOAA.OI.location should be be an string containing the file path to NOAA OI V2."))

  if (!speed.wet[3] > speed.wet[1]) stop(paste("Maximum speed when wet should be higher than optimal speed."))
  if (!speed.dry[3] > speed.dry[1]) stop(paste("Maximum speed when dry should be higher than optimal speed."))
  if (min(boundary.box[1:2]) < -180 | max(boundary.box[1:2]) > 180) stop(paste("Longitude values of boundary box should be between -180 and 180."))
  if (min(boundary.box[3:4]) < -90 | max(boundary.box[3:4]) > 90) stop(paste("Latitude values of boundary box should be between -90 and 90."))

  if (!all(class(tagging.date) == c("POSIXct", "POSIXt"))) stop(paste("tagging.date is not a datetime."))
  if (!all(class(retrieval.date) == c("POSIXct", "POSIXt"))) stop(paste("retrieval.date is not a datetime."))
  if (tagging.date > retrieval.date) stop(paste("retrieval.date is before tagging.date"))

  if(methods::is(trn,"list")) trn <- as.data.frame(lapply(trn, cbind))
  if(!is.null(sensor) & methods::is(sensor,"list")) sensor <- as.data.frame(lapply(sensor, cbind))
  if(!is.null(act) & methods::is(act,"list")) act <- as.data.frame(lapply(act, cbind))

  land.mask.mod <- as.data.frame(unclass(land.mask.mod))
  if(!methods::is(trn,"data.frame")) stop(paste("trn argument is not a dataframe or coercible list."))
  if(!is.null(sensor) & (!methods::is(sensor,"data.frame"))) stop(paste("sensor argument is not a dataframe or coercible list."))
  if(!is.null(act) & (!methods::is(act,"data.frame"))) stop(paste("act argument is not a dataframe or coercible list."))
  if(!is.null(land.mask.mod) & (!methods::is(land.mask.mod,"data.frame"))) stop(paste("land.mask.mod argument is not coercible to a dataframe."))

  if (!all(c("tFirst","tSecond","type") == names(trn))) stop(paste("trn column names are incorrect. Try reading in data using appropriate function e.g. read_trn."))
  if (!all(c("date","SST","SST.remove") == names(sensor))) stop(paste("sensor column names are incorrect. Check input dataframe."))
  if (!all(c("dtime","duration","wet.dry","wetdry") == names(act))) stop(paste("act column names are incorrect. Check input dataframe."))
  if (!is.null(land.mask.mod) & !all(c("min.lon","max.lon","min.lat","max.lat") == names(land.mask.mod))) stop(paste("land.mask.mod column names are incorrect. Check input dataframe."))

  if (!length(names(trn)) == 3) stop(paste("Incorrect number of columns in trn. Try reading in data using appropriate function e.g. read_trn."))
  if (!is.null(sensor) & !length(names(sensor)) == 3) stop(paste("Incorrect number of columns in sensor."))
  if (!length(names(act)) == 4) stop(paste("Incorrect number of columns in act."))
  if (!is.null(land.mask.mod) & !length(names(land.mask.mod)) == 4) stop(paste("Incorrect number of columns in land.mask.mod. "))

  if (!is.logical(east.west.comp)) stop(paste("east.west.comp must be either TRUE or FALSE."))
  if (!is.logical(land.mask)) stop(paste("land.mask must be either TRUE or FALSE."))
  if (!is.logical(backward)) stop(paste("backward must be either TRUE or FALSE."))

  start.time <- Sys.time()
  min.lat <- max.lat <- min.lon <- max.lon <- Land.Sea.Mask <- NULL
  tFirst <- tSecond <- type <- dtime <- doy <- jday <- year <- month <- NULL
  oldw <- getOption("warn")
  options(warn = -1)
  if (is.null(sensor))
    sst.used = F
  else sst.used = T
  model.input <- data.frame(parameter = c("particle.number",
                                          "iteration.number",
                                          "loess.quartile",
                                          "tagging.location",
                                          "tagging.date",
                                          "retrieval.date",
                                          "sunrise.sd",
                                          "sunset.sd",
                                          "range.solar",
                                          "speed.wet",
                                          "speed.dry",
                                          "sst.sd",
                                          "max.sst.diff",
                                          "days.around.spring.equinox",
                                          "days.around.fall.equinox",
                                          "ice.conc.cutoff",
                                          "boundary.box",
                                          "east.west.comp",
                                          "wetdry.resolution",
                                          "NOAA.OI.location",
                                          "land.mask.mod",
                                          "backward",
                                          "sensor.data"),
                            chosen = c(
                              paste(particle.number, collapse = " "),
                              paste(iteration.number, collapse = " "),
                              paste(loess.quartile, collapse = " "),
                              paste(tagging.location,  collapse = " "),
                              paste(tagging.date, collapse = " "),
                              paste(retrieval.date, collapse = " "),
                              paste(sunrise.sd, collapse = " "),
                              paste(sunset.sd, collapse = " "),
                              paste(range.solar, collapse = " "),
                              paste(speed.wet, collapse = " "),
                              paste(speed.dry, collapse = " "),
                              paste(sst.sd, collapse = " "),
                              paste(max.sst.diff, collapse = " "),
                              paste(days.around.spring.equinox, collapse = " "),
                              paste(days.around.fall.equinox, collapse = " "),
                              paste(ice.conc.cutoff, collapse = " "),
                              paste(boundary.box, collapse = " "),
                              paste(east.west.comp, collapse = " "),
                              paste(wetdry.resolution, collapse = " "),
                              paste(NOAA.OI.location, collapse = " "),
                              paste(land.mask.mod, collapse = " "),
                              paste(backward, collapse = " "),
                              sst.used
                                          ))
  if (!is.null(land.mask)) {
    landmask.location <- list.files(path = NOAA.OI.location,
                                    pattern = "lsmask.oisst.v2.nc", recursive = T)
    if (length(landmask.location) == 0) {
      stop(paste("no land mask file found in folder", NOAA.OI.location,
                 sep = " "), call. = F)
    }
    landmask.location <- paste(NOAA.OI.location, landmask.location,
                               sep = "/")[1]
  }
  trn$dtime <- trn$tFirst + as.numeric(difftime(trn$tSecond,
                                                trn$tFirst, units = "sec"))/2
  trn$doy <- as.numeric(strftime(trn$dtime, format = "%j"))
  trn$month <- as.numeric(strftime(trn$dtime, format = "%m"))
  trn$year <- as.numeric(strftime(trn$dtime, format = "%Y"))
  trn$jday <- as.numeric(julian(trn$dtime))
  trn <- trn[trn$tFirst >= as.POSIXct(tagging.date) & trn$tSecond <=
               as.POSIXct(retrieval.date), ]
  trn <- trn[!is.na(trn$tFirst), ]
  trn <- trn[!is.na(trn$tSecond), ]
  if (nrow(trn) == 0)
    stop("no data points in trn file between selected tagging and retrieval date",
         call. = F)
  proj.latlon <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  f = function(x) function(i) sapply(x, `[[`, i)
  col <- as.data.frame(cbind(as.numeric(tagging.location[1]),
                             as.numeric(tagging.location[2])))
  colnames(col) <- c("lon", "lat")
  coordinates(col) <- cbind(col$lon, col$lat)
  proj4string(col) <- proj.latlon
  col$dtime <- as.POSIXct(tagging.date, tz = "UTC")
  col$doy <- NA
  col$jday <- NA
  col$year <- NA
  col$type <- NA
  col$tFirst <- as.POSIXct(tagging.date, tz = "UTC")
  col$tSecond <- as.POSIXct(tagging.date, tz = "UTC")
  col$tFirst.err <- NA
  col$tSecond.err <- NA
  col$sun.elev <- NA
  col$step <- NA
  col$iteration <- NA
  col$bearing <- NA
  col$distance <- NA
  col$frac.timedry <- NA
  col$speed <- NA
  col$timediff <- NA
  col$sat.ice <- NA
  col$sat.sst <- NA
  col$sat.sst.err <- NA
  col$tag.sst <- NA
  col$sst.diff <- NA
  col$wsst <- NA
  col$wspeed <- NA
  col$wrel <- 1
  empty.spdf <- col
  empty.spdf$wrel <- 2
  if (!is.null(loess.quartile)) {
    trn$loes <- loessFilter(trn, plot = T, k = loess.quartile)
    trn <- trn[trn$loes == T, ]
  }
  if (east.west.comp == T) {
    datetime <- trn$dtime
    temp.lon <- coord(trn, degElevation = -6, note = F)[,
                                                        2]
    longitude2 <- -temp.lon
    longitude1 <- -c(NA, temp.lon[1:(length(temp.lon) - 1)])
    timedate2 <- as.numeric(datetime)
    timedate1 <- as.numeric(c(NA, datetime[1:(length(datetime) -
                                                1)]))
    length <- abs(as.numeric(difftime(trn$tSecond, trn$tFirst,
                                      units = "sec")))
    m <- (longitude2 - longitude1)/((timedate2 - timedate1)) *
      length/15/2
    ewc <- data.frame(datetime, longitude2, longitude1, timedate2,
                      timedate1, length, m)
    ewc$tFirst.corrected <- trn$tFirst + (ewc$m)
    ewc$tSecond.corrected <- trn$tSecond - (ewc$m)
    ewc$tFirst.corrected[is.na(ewc$tFirst.corrected)] <- trn$tFirst[is.na(ewc$tFirst.corrected)]
    ewc$tSecond.corrected[is.na(ewc$tSecond.corrected)] <- trn$tSecond[is.na(ewc$tSecond.corrected)]
    trn$tFirst <- ewc$tFirst.corrected
    trn$tSecond <- ewc$tSecond.corrected
  }
  ho3 <- data.frame(subset(trn, select = c(tFirst, tSecond,
                                           type, dtime, doy, jday, year, month)))
  ho4 <- data.frame(mapply(rep, ho3, particle.number))
  ho4[, 1] <- as.POSIXct(as.numeric(as.character(ho4[, 1])),
                         origin = "1970-01-01", tz = "UTC")
  ho4[, 2] <- as.POSIXct(as.numeric(as.character(ho4[, 2])),
                         origin = "1970-01-01", tz = "UTC")
  ho4[, 4] <- as.POSIXct(as.numeric(as.character(ho4[, 4])),
                         origin = "1970-01-01", tz = "UTC")
  ho4$loop.step <- paste(as.numeric(julian(ho4$tFirst)), as.numeric(julian(ho4$tSecond)),
                         ho4$type, sep = "-")
  ho4$step <- as.numeric(as.factor(ho4$loop.step))
  sun.elev.steps <- seq(range.solar[1], range.solar[2], 0.01)
  ho4$sun.elev <- sample(sun.elev.steps, size = nrow(ho4),
                         replace = T)
  ho4$tFirst.er[ho4$type == 1] <- 60 * (rlnorm(length(ho4[ho4$type == 1, 1]), meanlog = sunrise.sd[1], sdlog = sunrise.sd[2]) + sunrise.sd[3])
  ho4$tSecond.er[ho4$type == 1] <- -60 * (rlnorm(length(ho4[ho4$type == 1, 1]), meanlog = sunset.sd[1], sdlog = sunset.sd[2]) + sunset.sd[3])
  ho4$tFirst.er[ho4$type == 2] <- -60 * (rlnorm(length(ho4[ho4$type == 2, 1]), meanlog = sunset.sd[1], sdlog = sunset.sd[2]) + sunset.sd[3])
  ho4$tSecond.er[ho4$type == 2] <- 60 * (rlnorm(length(ho4[ho4$type == 2, 1]), meanlog = sunrise.sd[1], sdlog = sunrise.sd[2]) +  sunrise.sd[3])
  ho4$tFirst <- ho4$tFirst + ho4$tFirst.er
  ho4$tSecond <- ho4$tSecond + ho4$tSecond.er
  new.pos <- as.data.frame(coord(ho4, degElevation = ho4$sun.elev, note = F, method = "NOAA"))
  colnames(new.pos) <- c("lon", "lat")
  ho4$lon <- new.pos$lon
  ho4$lat <- new.pos$lat
  spring.equinox <- c((79 - days.around.spring.equinox[1]):(79 + days.around.spring.equinox[2]))
  fall.equinox <- c((265 - days.around.fall.equinox[1]):(265 + days.around.fall.equinox[2]))
  ho4$lat[ho4$doy %in% c(spring.equinox, fall.equinox)] <- sample(seq(boundary.box[3],
                                                                      boundary.box[4], by = 1e-04), size = length(ho4$lat[ho4$doy %in%
                                                                                                                            c(spring.equinox, fall.equinox)]), replace = T)
  ho4$sun.elev[ho4$doy %in% c(spring.equinox, fall.equinox)] <- NA
  if (boundary.box[1] > boundary.box[2]) {
    ho4 <- ho4[(ho4$lon > boundary.box[1] | ho4$lon < boundary.box[2]) &
                 ho4$lat > boundary.box[3] & ho4$lat < boundary.box[4],
    ]
  }
  else {
    ho4 <- ho4[ho4$lon > boundary.box[1] & ho4$lon < boundary.box[2] &
                 ho4$lat > boundary.box[3] & ho4$lat < boundary.box[4],
    ]
  }
  sp6 <- ho4[!is.na(ho4$lat), ]
  jt <- data.frame(table(sp6$step))
  sp6 <- sp6[sp6$step %in% as.numeric(as.character(jt$Var1[jt$Freq >=
                                                             c(particle.number/5)])), ]
  rm.lat <- data.frame(max.lat = tapply(sp6$lat, sp6$step,
                                        max))
  rm.lat$step <- rownames(rm.lat)
  if (nrow(rm.lat) == 0) {
    stop(paste("No data points inside boundary box. increase boundary box"),
         call. = F)
  }
  rm.lat$rm <- 1
  rm.lat$rm[rm.lat$max.lat < boundary.box[3]] <- 0
  sp7 <- sp6[sp6$step %in% rm.lat$step[rm.lat$rm == 1], ]
  rm.lat <- data.frame(min.lat = tapply(sp7$lat, sp7$step,
                                        min))
  rm.lat$step <- rownames(rm.lat)
  if (nrow(rm.lat) == 0) {
    stop(paste("No data points inside boundary box. increase boundary box"),
         call. = F)
  }
  rm.lat$rm <- 1
  rm.lat$rm[rm.lat$min.lat > boundary.box[4]] <- 0
  sp7 <- sp7[sp7$step %in% rm.lat$step[rm.lat$rm == 1], ]
  all.particles <- data.frame(sp7)
  coordinates(all.particles) <- cbind(all.particles$lon, all.particles$lat)
  proj4string(all.particles) <- CRS(proj.latlon)
  if (!is.null(land.mask)) {
    landms <- rotate(raster(landmask.location))
    landmsSP <- rast(landms)
  }
  coordinates(sp7) <- cbind(sp7$lon, sp7$lat)
  proj4string(sp7) <- CRS(proj.latlon)
  sp7$landmask <- raster::extract(landms, sp7)
  possible.area <- NULL
  if (!is.null(land.mask) && !is.null(land.mask.mod)) {
    for (i in 1:nrow(land.mask.mod)) {
      sp7$landmask[sp7$lon > land.mask.mod[i, 1] &
                     sp7$lon < land.mask.mod[i, 2] &
                     sp7$lat > land.mask.mod[i, 3] &
                     sp7$lat < land.mask.mod[i, 4]] <- 0
    }
    suppressMessages({
      possible.area <- ggplot() +
                     geom_spatraster(data = landmsSP, mapping = aes(fill = Land.Sea.Mask)) +
                     scale_fill_gradient(low = "black", high = "white") +
                     geom_rect(aes(xmin = min.lon, xmax = max.lon, ymin = min.lat, ymax = max.lat,fill = 0), data = land.mask.mod) +
                     geom_path(aes(
                            x = c(boundary.box[1], boundary.box[1], boundary.box[2], boundary.box[2]),
                            y = c(boundary.box[3], boundary.box[4], boundary.box[4], boundary.box[3]),
                            col = "red"
                          )) +
                    labs(
                        x = "",
                        y = "",
                        title = "Land mask with bounding box",
                        subtitle = "White area within bounding box represents possible locations given current parameters") +
                     scale_x_continuous(breaks = seq(-180, 180, by = 10)) +
                     guides(fill="none", color="none")
    })
  }

  if (!is.null(land.mask)) {
    if (land.mask == T)
      sp7 <- sp7[sp7$landmask == 1, ]
    if (land.mask == F)
      sp7 <- sp7[sp7$landmask == 0, ]
  }
  jt <- data.frame(table(sp7$step))
  grr <- sp7[sp7$step %in% jt$Var1[jt$Freq >= c(particle.number *
                                                  0.1)], ]
  grr$dtime <- as.POSIXct((as.numeric(grr$tSecond) - as.numeric(grr$tFirst))/2,
                          origin = grr$tFirst, tmz = "UTC")
  grr$jday2 <- floor(grr$jday)
  grr$date <- grr$dtime
  grr <- grr[order(grr$dtime), ]
  col2 <- col
  if (backward == F)
    col2$dtime <- as.POSIXct(tagging.date)
  if (backward == T)
    col2$dtime <- as.POSIXct(retrieval.date)
  col2$jday <- as.numeric(julian(col2$dtime))
  colt <- vector("list", length = iteration.number)
  colt[1:iteration.number] <- col2
  iter = 0
  if (backward == F)
    steps <- sort(unique(grr$step))
  if (backward == T)
    steps <- sort(unique(grr$step), decreasing = T)
  for (ts in steps) {
    step.start <- Sys.time()
    if (length(grr$dtime[grr$step == ts]) > 0) {
      gr3 <- grr[grr$step == ts, ]
      gbear <- lapply(colt, FUN = function(x) bearing(x,
                                                      gr3))
      gdist <- lapply(colt, FUN = function(x) spDists(gr3,
                                                      x, longlat = T) * 1000)
      fun.time.dry <- function(x) {
        if (!is.null(act)) {
          slo2 <- act$wetdry[act$dtime >= min(x$tFirst) &
                               act$dtime <= max(gr3$tSecond)]
          slo2.time <- abs(as.numeric(difftime(min(x$tFirst),
                                               max(gr3$tSecond), units = "secs")))
          sumact <- (1 - sum(slo2) * wetdry.resolution/slo2.time)
          if (sumact > 1)
            sumact <- 1
          if (sumact < 0)
            sumact <- 0
        }
        else {
          sumact <- 1
        }
        return(sumact)
      }
      gtime.dry <- lapply(colt, fun.time.dry)
      gr2 <- vector("list", length = iteration.number)
      gr2[1:iteration.number] <- gr3
      gr2 <- lapply(gr2, function(x) data.frame(x))
      gr2 <- mapply(cbind, gr2, gbear = gbear, gdist = gdist,
                    time.dry = gtime.dry, SIMPLIFY = FALSE)
      prev.dtime <- lapply(colt, function(x) x$dtime)
      gr2 <- mapply(cbind, gr2, prev.dtime = prev.dtime,
                    SIMPLIFY = FALSE)
      gspeed <- lapply(gr2, function(x) x$gdist/abs(as.numeric(difftime(x$dtime,
                                                                        x$prev.dtime, units = "secs"))))
      time.diff <- lapply(gr2, function(x) difftime(x$dtime,
                                                    x$prev.dtime, units = "mins"))
      gr2 <- mapply(cbind, gr2, gspeed = gspeed, time.diff = time.diff,
                    SIMPLIFY = FALSE)
      if (!is.null(sensor)) {
        sensor$jday <- as.numeric(julian(sensor$date))
        track2 <- data.frame(day = c(as.numeric(as.character(substr(gr3$dtime[1],
                                                                    9, 10)))), month = c(as.numeric(as.character(substr(gr3$dtime[1],
                                                                                                                        6, 7)))), year = c(gr3$year[1]), lon = c(floor(min(gr3$lon)),
                                                                                                                                                                 ceiling(max(gr3$lon))), lat = c(floor(min(gr3$lat)),
                                                                                                                                                                                                 ceiling(max(gr3$lat))))
        track2.origin <- track2
        ls <- list.files(NOAA.OI.location)
        fname.sst <- paste0(NOAA.OI.location, "/", ls[grep(paste0("sst.day.mean.",
                                                                  track2$year[1]), ls)])[1]
        fname.err <- paste0(NOAA.OI.location, "/", ls[grep(paste0("sst.day.err.",
                                                                  track2$year[1]), ls)])[1]
        fname.ice <- paste0(NOAA.OI.location, "/", ls[grep(paste0("icec.day.mean.",
                                                                  track2$year[1]), ls)])[1]
        if ((min(track2$lon) * max(track2$lon)) >= 0) {
          if (min(track2$lon) < 0)
            track2$lon[track2$lon == 0] <- 360
          track2$lon[track2$lon < 0] <- 360 + track2$lon[track2$lon <
                                                           0]
          eoi <- load.NOAA.OISST.V2(fname = fname.sst,
                                    lsmask = landmask.location, lonW = min(track2$lon),
                                    lonE = max(track2$lon), latS = min(track2$lat),
                                    latN = max(track2$lat), date1 = as.Date(paste(track2$year[1],
                                                                                  track2$month[1], track2$day[1], sep = "-")),
                                    date2 = as.Date(paste(track2$year[1], track2$month[1],
                                                          track2$day[1], sep = "-")), extract.value = "sst")
          eii <- load.NOAA.OISST.V2(fname = fname.ice,
                                    lsmask = landmask.location, lonW = min(track2$lon),
                                    lonE = max(track2$lon), latS = min(track2$lat),
                                    latN = max(track2$lat), date1 = as.Date(paste(track2$year[1],
                                                                                  track2$month[1], track2$day[1], sep = "-")),
                                    date2 = as.Date(paste(track2$year[1], track2$month[1],
                                                          track2$day[1], sep = "-")), extract.value = "icec")
          eri <- load.NOAA.OISST.V2(fname = fname.err,
                                    lsmask = landmask.location, lonW = min(track2$lon),
                                    lonE = max(track2$lon), latS = min(track2$lat),
                                    latN = max(track2$lat), date1 = as.Date(paste(track2$year[1],
                                                                                  track2$month[1], track2$day[1], sep = "-")),
                                    date2 = as.Date(paste(track2$year[1], track2$month[1],
                                                          track2$day[1], sep = "-")), extract.value = "err")
          sstd <- raster(as.matrix(eoi[, , 1]), xmn = min(track2.origin$lon) -
                           0.125, xmx = max(track2.origin$lon) + 0.125,
                         ymn = min(track2.origin$lat) - 0.125, ymx = max(track2.origin$lat) +
                           0.125, crs = CRS(proj.latlon))
          errd <- raster(as.matrix(eri[, , 1]), xmn = min(track2.origin$lon) -
                           0.125, xmx = max(track2.origin$lon) + 0.125,
                         ymn = min(track2.origin$lat) - 0.125, ymx = max(track2.origin$lat) +
                           0.125, crs = CRS(proj.latlon))
          iced <- raster(as.matrix(eii[, , 1]), xmn = min(track2.origin$lon) -
                           0.125, xmx = max(track2.origin$lon) + 0.125,
                         ymn = min(track2.origin$lat) - 0.125, ymx = max(track2.origin$lat) +
                           0.125, crs = CRS(proj.latlon))
        }
        else {
          track2$lon[track2$lon < 0] <- 360 + track2$lon[track2$lon <
                                                           0]
          eoi1 <- load.NOAA.OISST.V2(fname = fname.sst,
                                     lsmask = landmask.location, lonW = 0, lonE = min(track2$lon),
                                     latS = min(track2$lat), latN = max(track2$lat),
                                     date1 = as.Date(paste(track2$year[1], track2$month[1],
                                                           track2$day[1], sep = "-")), date2 = as.Date(paste(track2$year[1],
                                                                                                             track2$month[1], track2$day[1], sep = "-")))
          eii1 <- load.NOAA.OISST.V2(fname = fname.ice,
                                     lsmask = landmask.location, lonW = 0, lonE = min(track2$lon),
                                     latS = min(track2$lat), latN = max(track2$lat),
                                     date1 = as.Date(paste(track2$year[1], track2$month[1],
                                                           track2$day[1], sep = "-")), date2 = as.Date(paste(track2$year[1],
                                                                                                             track2$month[1], track2$day[1], sep = "-")),
                                     extract.value = "icec")
          eri1 <- load.NOAA.OISST.V2(fname = fname.err,
                                     lsmask = landmask.location, lonW = 0, lonE = min(track2$lon),
                                     latS = min(track2$lat), latN = max(track2$lat),
                                     date1 = as.Date(paste(track2$year[1], track2$month[1],
                                                           track2$day[1], sep = "-")), date2 = as.Date(paste(track2$year[1],
                                                                                                             track2$month[1], track2$day[1], sep = "-")),
                                     extract.value = "err")
          eoi2 <- load.NOAA.OISST.V2(fname = fname.sst,
                                     lsmask = landmask.location, lonW = max(track2$lon),
                                     lonE = 360, latS = min(track2$lat), latN = max(track2$lat),
                                     date1 = as.Date(paste(track2$year[1], track2$month[1],
                                                           track2$day[1], sep = "-")), date2 = as.Date(paste(track2$year[1],
                                                                                                             track2$month[1], track2$day[1], sep = "-")))
          eii2 <- load.NOAA.OISST.V2(fname = fname.ice,
                                     lsmask = landmask.location, lonW = max(track2$lon),
                                     lonE = 360, latS = min(track2$lat), latN = max(track2$lat),
                                     date1 = as.Date(paste(track2$year[1], track2$month[1],
                                                           track2$day[1], sep = "-")), date2 = as.Date(paste(track2$year[1],
                                                                                                             track2$month[1], track2$day[1], sep = "-")),
                                     extract.value = "icec")
          eri2 <- load.NOAA.OISST.V2(fname = fname.err,
                                     lsmask = landmask.location, lonW = max(track2$lon),
                                     lonE = 360, latS = min(track2$lat), latN = max(track2$lat),
                                     date1 = as.Date(paste(track2$year[1], track2$month[1],
                                                           track2$day[1], sep = "-")), date2 = as.Date(paste(track2$year[1],
                                                                                                             track2$month[1], track2$day[1], sep = "-")),
                                     extract.value = "err")
          sstd <- raster(cbind(as.matrix(eoi2[, , 1]),
                               as.matrix(eoi1[, , 1])), xmn = min(track2.origin$lon) -
                           0.125, xmx = max(track2.origin$lon) + 0.125,
                         ymn = min(track2.origin$lat) - 0.125, ymx = max(track2.origin$lat) +
                           0.125, crs = CRS(proj.latlon))
          errd <- raster(cbind(as.matrix(eri2[, , 1]),
                               as.matrix(eri1[, , 1])), xmn = min(track2.origin$lon) -
                           0.125, xmx = max(track2.origin$lon) + 0.125,
                         ymn = min(track2.origin$lat) - 0.125, ymx = max(track2.origin$lat) +
                           0.125, crs = CRS(proj.latlon))
          iced <- raster(cbind(as.matrix(eii2[, , 1]),
                               as.matrix(eii1[, , 1])), xmn = min(track2.origin$lon) -
                           0.125, xmx = max(track2.origin$lon) + 0.125,
                         ymn = min(track2.origin$lat) - 0.125, ymx = max(track2.origin$lat) +
                           0.125, crs = CRS(proj.latlon))
        }
        gr3$sat.ice <- raster::extract(iced, gr3)
        gr3$sat.sst <- raster::extract(sstd, gr3)
        gr3$sat.sst.err <- raster::extract(errd, gr3)
        gr3$sat.ice[is.na(gr3$sat.ice)] <- 0
        if (as.Date(gr3$date[1]) < as.Date("2012-08-11") |
            as.Date(gr3$date[1]) > as.Date("2012-08-16"))
          gr3$sat.sst[gr3$sat.ice > ice.conc.cutoff] <- NA
        if (length(sensor$SST[sensor$jday == gr3$jday2[1]]) >
            0)
          gr3$tag.sst <- sensor$SST[sensor$jday == gr3$jday2[1]]
        if (length(sensor$SST[sensor$jday == gr3$jday2[1]]) ==
            0)
          gr3$tag.sst <- NA
      }
      if (is.null(sensor)) {
        gr3$sat.ice <- 0
        gr3$sat.sst <- 0
        gr3$sat.sst.err <- 0
        gr3$tag.sst <- NA
      }
      gr3$sst.diff <- gr3$sat.sst - gr3$tag.sst
      gr2 <- lapply(gr2, function(x) cbind(x, sat.sst = gr3$sat.sst,
                                           tag.sst = gr3$tag.sst, sst.diff = gr3$sst.diff,
                                           sat.sst.err = gr3$sat.sst.err, sat.ice = gr3$sat.ice))
      if (!is.null(land.mask))
        if (land.mask == T)
          gr2 <- lapply(gr2, function(x) x[!is.na(x$sat.sst),
          ])
      fun.gspeed <- function(x) {
        dnorm(x$gspeed, mean = c((speed.dry[1] * x$time.dry) +
                                   (speed.wet[1] * (1 - x$time.dry))), sd = c((speed.dry[2] *
                                                                                 x$time.dry) + (speed.wet[2] * (1 - x$time.dry))))/max(dnorm(c((speed.dry[1] *
                                                                                                                                                  x$time.dry) + (speed.wet[1] * (1 - x$time.dry))),
                                                                                                                                             mean = c((speed.dry[1] * x$time.dry) + (speed.wet[1] *
                                                                                                                                                                                       (1 - x$time.dry))), sd = c((speed.dry[2] *
                                                                                                                                                                                                                     x$time.dry) + (speed.wet[2] * (1 - x$time.dry)))),
                                                                                                                                       na.rm = T)
      }
      wspeed <- lapply(gr2, fun.gspeed)
      fun.gsst <- function(x) {
        dnorm(x$sst.diff, mean = 0, sd = sst.sd + x$sat.sst.err)/max(dnorm(0,
                                                                           mean = 0, sd = sst.sd + x$sat.sst.err), na.rm = T)
      }
      wsst <- lapply(gr2, fun.gsst)
      gr2 <- mapply(cbind, gr2, wspeed = wspeed, wsst = wsst,
                    SIMPLIFY = FALSE)
      gr2 <- lapply(gr2, function(x) {
        x$wspeed[x$gspeed <= c(speed.dry[1] * x$time.dry +
                                 speed.wet[1] * (1 - x$time.dry))] <- 1
        x$wspeed[x$gspeed < 0] <- 0
        x$wspeed[x$gspeed > c(speed.dry[3] * x$time.dry +
                                speed.wet[3] * (1 - x$time.dry))] <- 0
        x$wspeed[x$sat.ice > ice.conc.cutoff] <- 0
        x$wsst[x$sst.diff > max.sst.diff] <- 0
        x$wsst[x$sst.diff < (-max.sst.diff)] <- 0
        x$wsst[is.na(x$wsst)] <- 0
        return(x)
      })
      if (!is.na(gr3$tag.sst[1]))
        gselect <- lapply(gr2, function(x) x$wspeed *
                            x$wsst)
      if (is.na(gr3$tag.sst[1]))
        gselect <- lapply(gr2, function(x) x$wspeed)
      gr2 <- mapply(cbind, gr2, grel = gselect, SIMPLIFY = FALSE)
      new.r <- vector("list", length = iteration.number)
      new.r2 <- vector("list", length = iteration.number)
      random.point <- vector("list", length = iteration.number)
      for (botts in 1:iteration.number) {
        if (length(gr2[[botts]]$grel[gr2[[botts]]$grel >
                                     0 & !is.na(gr2[[botts]]$grel)]) > 0) {
          random.point[[botts]] <- sample(nrow(gr2[[botts]]),
                                          size = 1, prob = gr2[[botts]]$grel)
          new.r[[botts]] <- data.frame(destPoint(colt[[botts]],
                                                 gr2[[botts]]$gbear[random.point[[botts]]],
                                                 gr2[[botts]]$gdist[random.point[[botts]]]))
          coordinates(new.r[[botts]]) <- new.r[[botts]]
          proj4string(new.r[[botts]]) <- CRS(proj.latlon)
          new.r[[botts]]$dtime <- gr2[[botts]]$dtime[random.point[[botts]]]
          new.r[[botts]]$doy <- gr2[[botts]]$doy[random.point[[botts]]]
          new.r[[botts]]$jday <- gr2[[botts]]$jday[random.point[[botts]]]
          new.r[[botts]]$year <- gr2[[botts]]$year[random.point[[botts]]]
          new.r[[botts]]$type <- gr2[[botts]]$type[random.point[[botts]]]
          new.r[[botts]]$tFirst <- gr2[[botts]]$tFirst[random.point[[botts]]]
          new.r[[botts]]$tSecond <- gr2[[botts]]$tSecond[random.point[[botts]]]
          new.r[[botts]]$tFirst.err <- gr2[[botts]]$tFirst.er[random.point[[botts]]]
          new.r[[botts]]$tSecond.err <- gr2[[botts]]$tSecond.er[random.point[[botts]]]
          new.r[[botts]]$sun.elev <- gr2[[botts]]$sun.elev[random.point[[botts]]]
          new.r[[botts]]$sex <- gr2[[botts]]$sex[random.point[[botts]]]
          new.r[[botts]]$morph <- gr2[[botts]]$morp[random.point[[botts]]]
          new.r[[botts]]$step <- ts
          new.r[[botts]]$iteration <- botts
          new.r[[botts]]$bearing <- gr2[[botts]]$gbear[random.point[[botts]]]
          new.r[[botts]]$distance <- gr2[[botts]]$gdist[random.point[[botts]]]
          new.r[[botts]]$frac.timedry <- gr2[[botts]]$time.dry[random.point[[botts]]]
          new.r[[botts]]$speed <- gr2[[botts]]$gspeed[random.point[[botts]]]
          new.r[[botts]]$timediff <- gr2[[botts]]$time.diff[random.point[[botts]]]
          new.r[[botts]]$sat.ice <- gr2[[botts]]$sat.ice[random.point[[botts]]]
          new.r[[botts]]$sat.sst <- gr2[[botts]]$sat.sst[random.point[[botts]]]
          new.r[[botts]]$sat.sst.err <- gr2[[botts]]$sat.sst.err[random.point[[botts]]]
          new.r[[botts]]$tag.sst <- gr2[[botts]]$tag.sst[random.point[[botts]]]
          new.r[[botts]]$sst.diff <- gr2[[botts]]$sst.diff[random.point[[botts]]]
          new.r[[botts]]$wsst <- gr2[[botts]]$wsst[random.point[[botts]]]
          new.r[[botts]]$wspeed <- gr2[[botts]]$wspeed[random.point[[botts]]]
          new.r[[botts]]$wrel <- gr2[[botts]]$grel[random.point[[botts]]]
          new.r2[[botts]] <- subset(data.frame(new.r[[botts]]),
                                    select = names(empty.spdf))
        }
        else {
          new.r[[botts]] <- empty.spdf
          new.r2[[botts]] <- subset(data.frame(new.r[[botts]]),
                                    select = names(empty.spdf))
          new.r[[botts]] <- colt[[botts]]
        }
      }
      colt <- new.r
      iter = iter + 1
      new.r2 <- as.data.frame(Map(f(new.r2), names(new.r2[[1]])))
      if (iter == 1)
        newt2 <- new.r2
      else newt2 <- rbind(newt2, new.r2)
    }
    step.end <- Sys.time()
    step.time <- step.end - step.start
    cat("\r", paste(as.Date(gr3$dtime)[1], "  -  ", iter,
                    " of ", length(unique(grr$step)), " steps      ",
                    sep = ""))
  }
  newt2 <- newt2[newt2$wrel <= 1, ]
  newt2 <- as.data.frame(newt2)
  newt2$lon <- as.numeric(newt2$lon)
  newt2$lat <- as.numeric(newt2$lat)
  newt2 <- newt2[!is.na(newt2$lon), ]
  newt2 <- newt2[!is.na(newt2$lat), ]
  if (is.null(sensor)) {
    newt2$sat.ice <- NA
    newt2$sat.sst <- NA
    newt2$sat.sst.err <- NA
    newt2$sst.diff <- NA
  }
  coordinates(newt2) <- c("lon", "lat")
  proj4string(newt2) <- CRS(proj.latlon)
  newt2$dtime <- as.POSIXct(newt2$dtime, origin = "1970-01-01",
                            tz = "UTC")
  newt2$tFirst <- as.POSIXct(newt2$tFirst, origin = "1970-01-01",
                             tz = "UTC")
  newt2$tSecond <- as.POSIXct(newt2$tSecond, origin = "1970-01-01",
                              tz = "UTC")
  newt2$jday2 <- as.numeric(julian(newt2$dtime))
  newt2 <- newt2[order(newt2$step), ]
  newt2$month <- as.numeric(strftime(newt2$dtime, "%m"))
  for (i in unique(newt2$step)) {
    sf <- data.frame(spDists(coordinates(newt2[newt2$step ==
                                                 i, ]), longlat = T), ncol = length(newt2$step[newt2$step ==
                                                                                                 i]))
    sa <- data.frame(sum.dist = rowMeans(sf), bot = seq(1,
                                                        length(newt2$step[newt2$step == i]), 1))
    gmp <- newt2[newt2$step == i, ][sa$sum.dist == min(sa$sum.dist),
    ][1, ]
    gmp$median.sat.sst <- median(newt2$sat.sst[newt2$step ==
                                                 i])
    gmp$median.sun.elev <- median(newt2$sun.elev[newt2$step ==
                                                   i])
    gmp$median.wrel <- median(newt2$wrel[newt2$step == i])
    if (i == unique(newt2$step)[1])
      newg <- gmp
    else newg <- rbind(newg, gmp)
  }
  end.time <- Sys.time()
  time.taken <- abs(difftime(end.time, start.time, units = "mins"))
  cat("\r", paste("algorithm run time:", round(as.numeric(time.taken),
                                               1), "min      ", sep = " "))
  list.all <- list(newt2, newg, all.particles, model.input,
                   time.taken, possible.area)
  names(list.all) <- c("all tracks", "most probable track",
                       "all possible particles", "input parameters", "model run time", "possible position area")
  class(list.all) <- "glsTracks"
  options(warn = oldw)
  return(list.all)
}


#' Summary method for glsTracks object
#'
#' @param obj object of class "glsTracks"
#' @param ... additional arguments to be passed to methods
#'
#' @export summary_glsTracks
#' @export
summary_glsTracks <- function(obj,...) {
  if (!methods::is(obj,"glsTracks")) stop(paste("obj is not a glsTracks object."))
  p <- obj[[4]]
  cat(paste("The following input parameters were used:\n" ))
  cat(paste(p[,1], " = ", p[,2], "\n"))
  cat(paste("\n The algorithm took", round(as.numeric(obj[[5]]),4), "minutes to process the data."))
  cat(paste("\n There are", nrow(obj[[2]])), "location estimates in the most probable track.")
  if (!is.null(p[28,2])) cat("\n A land mask modification was used. To view affected areas use the plot function.")
}

#' Generate series of plots displaying iteration location estimates at each timestep overlaid with most probable track.
#'
#' @param obj object of class "glsTracks"
#' @param zoom If T plot limits are min/max of all iteration estimates. If F plot limits are bounding box.
#' @param ... additional arguments to be passed to method.
#'
#' @import ggplot2
#'
#' @export plot_glsTracks
#' @export

plot_glsTracks <- function(obj, zoom=TRUE, ...) {
  if (!methods::is(obj,"glsTracks")) stop(paste("obj is not a glsTracks object."))
  lat <- lon <- NULL
  boundary.box <- unlist(lapply(strsplit(obj$`input parameters`$chosen[17], " "), as.numeric))
  iter.total <- as.numeric(obj$`input parameters`$chosen[2])
  tag.loc <- as.numeric(unlist(strsplit(as.character(obj[[4]][4, 2]), "[ ]")))
  median.path <- as.data.frame(unclass(obj$`most probable track`)@coords)
  iteration.positions <- as.data.frame(unclass(obj$`all tracks`)@coords)
  plot.init <- obj$`possible position area` +
    geom_path(data=median.path, aes(x=lon, y=lat), size=0.3) +
    geom_path(data=data.frame(lon=c(median.path$lon[length(median.path$lon)],
                                       tag.loc[1],
                                       median.path$lon[1]),
                                 lat=c(median.path$lat[length(median.path$lat)],
                                       tag.loc[2],
                                       median.path$lat[1])),
              aes(x=lon,y=lat)) +
    geom_point(data=median.path, aes(x=lon, y=lat), col="orange") +
    geom_point(aes(x=tag.loc[1], y=tag.loc[2], col="red"))
  plot.list <- list()
  plot.list <- lapply(1:length(unique(obj[[1]]$step)), function(i) {
    iter.end <- i * iter.total
    iter.start <- iter.end - iter.total + 1
    plot.list[[i]] <- plot.init +
      geom_point(data = iteration.positions[iter.start:iter.end, ], aes(x=lon, y=lat, alpha=0.02), size=0.01) +
      geom_point(data=median.path, aes(x=lon[i], y=lat[i])) +
      ggtitle(paste("Step:", unique(obj[[1]]$step)[i], " Datetime:", obj[[1]]$dtime)[i]) +
      xlab("Longitude") + ylab("Latitude") +
      theme(legend.position = "none")
      if (zoom) {
      plot.list[[i]] <- plot.list[[i]] +
        lims(x=c(min(iteration.positions$lon), max(iteration.positions$lon)), y=c(min(iteration.positions$lat), max(iteration.positions$lat)))
    } else {
      plot.list[[i]] <- plot.list[[i]] +
        lims(x=c(boundary.box[1], boundary.box[2]), y=c(boundary.box[3], boundary.box[4]))
    }
  })
   return(plot.list)
}
