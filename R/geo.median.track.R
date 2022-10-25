#' Function to recalculate track with possibilities in non-active hemisphere removed.
#'
#' Due to the difficulty in deriving location estimates during periods of equinox, the algorithm samples randomly from the range of possible latitude values that lie within the bounding box with no regard for the speed distribution. This function is to be used if the bounding box crosses the equator and it is known which hemisphere the subject would be in during a given time period. Estimates prior to the earlier of either the cross.north or cross.south dates are the original median.track values. From the first until the later date, the median track will attempt to be estimated from all possible tracks/iterations from the initial hemisphere. i.e. cross.north < cross.south then between these dates only tracks in the northern hemisphere will be considered. Following the latter date, if it is set, the reverse will occur. In the event that there are no tracks in a given hemisphere over either of these periods, the original most probable track values will be used. For recordings spanning a long time/ multiple crossings, the data will need to be subset to smaller timeframes.
#'
#'
#' @param pr The output from GLS.prob.algorithm or prob_algorithm functions.
#' @param cross.north The expected date that a subject would cross the equator to the northern hemisphere. Character string in the form of "2022-04-15". NULL if a northward crossing isn't expected.
#' @param cross.south The expected date that a subject would cross the equator to the southern hemisphere. Character string in the form of "2022-04-15". NULL if a southward crossing isn't expected.
#'
#' @import sf
#' @import sp
#' @import dplyr
#'
#' @export

geo.median.track <- function(pr, cross.north = NULL, cross.south = NULL){

  pr1 <- as.data.frame(pr[[1]])
  pr2 <- as.data.frame(pr[[2]][,1:27])
  max.step <- max(pr1$step)

  if (is.null(cross.north)) cross.north <- as.Date("2999-12-31") else cross.north <- as.Date(cross.north)
  if (is.null(cross.south)) cross.south <- as.Date("2999-12-31") else cross.south <- as.Date(cross.south)

  if (cross.north == cross.south) {
    stop(paste('cross.north and cross.south must be different dates represented by a character string in the format of "2999-12-31"'))
  } else if (cross.north < cross.south) {
  filtered.tracks <- pr1 |>
    dplyr::filter(!(as.Date(pr1$dtime) >= cross.north & as.Date(pr1$dtime) <= cross.south & pr1$lat > 0))
  } else if (cross.north > cross.south) {
    filtered.tracks <- pr1 |>
      dplyr::filter(!(as.Date(pr1$dtime) <= cross.north & as.Date(pr1$dtime) >= cross.south & pr1$lat < 0))
  }

  sp::coordinates(filtered.tracks)<-~lon+lat

  for(i in unique(filtered.tracks$step)){
    sf                  <- data.frame(sp::spDists(sp::coordinates(filtered.tracks[filtered.tracks$step==i,]),longlat=T),
                                      ncol=length(filtered.tracks$step[filtered.tracks$step==i]))
    sa                  <- data.frame(sum.dist=rowMeans(sf),bot=seq(1,length(filtered.tracks$step[filtered.tracks$step==i]),1))
    gmp                 <- filtered.tracks[filtered.tracks$step==i,] [sa$sum.dist==min(sa$sum.dist),] [1,]

    if(i == unique(filtered.tracks$step)[1]) data <- gmp else data <- rbind(data,gmp)
  }

  data <- as.data.frame(data)
  data$mod <- TRUE
  prev.med.track <- anti_join(pr2, data, by = "step")
  if (nrow(prev.med.track) > 0) {
    prev.med.track$mod <- FALSE
    median.track <- rbind(data, prev.med.track) |> arrange(step)
  } else {
    median.track <- pr2
    cat("There is no change from the previous most probable path.")
  }
  sp::coordinates(median.track) <-  ~ lon + lat
  class(median.track) <- "modifiedTrack"
  return(median.track)
}

#' Summary method for modifiedTrack object
#'
#' @param obj object of class "modifiedTrack"
#' @param ... additional arguments to be passed to methods
#'
#' @export
summary_modifiedTrack <- function(obj,...) {
  if (!methods::is(obj,"modifiedTrack")) stop(paste("obj is not a modifiedTrack object."))
  mTrackD <- unclass(obj)@data
  mTrackCoord <- unclass(obj)@coords
  if (nrow(mTrackD) > 0) {
    cat("The modified track contains location estimates between", as.character(as.POSIXct(min(mTrackD[,6]))), "and", as.character(as.POSIXct(max(mTrackD[,7]))))
    cat(paste("\nAll location estimates lie within the following box: \n min.lon =", min(mTrackCoord[,1]), "\n max.lon =", max(mTrackCoord[,1]), "\n min.lat =", min(mTrackCoord[,2]), "\n max.lat =", max(mTrackCoord[,2])))
  }
}
