#' Function to recalculate track with possibilities in non-active hemisphere removed
#'
#'
#'
#' @param all.tracks First element of output from GLS.prob.algorithm function. All possible tracks.
#' @param cross.north The expected date that a subject would cross the equator to the northern hemisphere. Character string in the form of "2022-04-15". NULL if a northward crossing isn't expected.
#' @param cross.southThe The expected date that a subject would cross the equator to the southern hemisphere. Character string in the form of "2022-04-15". NULL if a southward crossing isn't expected.
#'
#' @import sf
#' @import dplyr
#'
#' @export

geo.median.track <- function(pr, cross.north = NULL, cross.south = NULL){

  pr1 <- as.data.frame(pr[[1]])
  max.step <- max(pr1$step)

  # if (!max(pr1$lat) > 0) {
  #   stop(paste("There are no location estimates in the northern hemisphere"))
  # } else if (min(pr1$lat) < 0) {
  #   stop(paste("There are no location estimates in the southern hemisphere"))
  # }

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

  if (!all(seq(2, max.step, by = 1) %in% unique(filtered.tracks$step))) stop(paste("There were no possible paths for at least one of the days. Check expected crossing dates based on pr$all.tracks data."))

  coordinates(filtered.tracks)<-~lon+lat

  for(i in unique(filtered.tracks$step)){
    sf                  <- data.frame(spDists(coordinates(filtered.tracks[filtered.tracks$step==i,]),longlat=T),
                                      ncol=length(filtered.tracks$step[filtered.tracks$step==i]))
    sa                  <- data.frame(sum.dist=rowMeans(sf),bot=seq(1,length(filtered.tracks$step[filtered.tracks$step==i]),1))
    gmp                 <- filtered.tracks[filtered.tracks$step==i,] [sa$sum.dist==min(sa$sum.dist),] [1,]

    if(i == unique(filtered.tracks$step)[1]) data <- gmp else data <- rbind(data,gmp)
  }


  # other <- anti_join(as.data.frame(all.tracks), data, by = "dtime")
  # median.track <- rbind(data, other) |> arrange(dtime)
  # coordinates(data)<-~lon+lat
  return(data)

}
