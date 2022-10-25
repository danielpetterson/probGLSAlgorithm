#' Read in data from .act files
#'
#' Transforms .act files into a data frame with datetime and wetdry values. Wetdry value represents the total number of times that the sensor is recorded as submerged in salt water within a time interval.
#'
#' @param actfile The name of an activity file downloaded from a geolocator device.
#' @param sampling.interval The interval at which the device samples state changes in seconds (e.g. MK15 & MK3006 sample every 3 sec).
#' @param summarise.interval The number of minutes to sum all the wet state changes. Defaults to 10 minutes.
#'
#' @import dplyr
#' @importFrom utils read.csv
#'
#' @export

read.act <- function(actfile, sampling.interval = 3, summarise.interval = 10) {

  if (!length(sampling.interval) == 1 | !is.numeric(sampling.interval)) stop(paste("sampling.interval should be the number of seconds between each conductivity measurement. This is device specific."))
  if (!length(summarise.interval) == 1 | !is.numeric(summarise.interval)) stop(paste("summarise.interval should be the number of minutes over which the total number of wet recordings are aggregated."))

  state <- dtime <- is_wet <- wetdry <- NULL

  act_data <- utils::read.csv(actfile, header = FALSE)
  if (NCOL(act_data == 5)) {
    colnames(act_data) <- c("status", "DateTime", "V3", "V4", "state")

    err <- grep("^ERROR:*", act_data$status)
    errors <- act_data[err,]
    if (!nrow(errors) == 0) {
      for (i in 1:nrow(errors)) {
        cat(paste0(errors$status[i], "\n"))
        cat(paste0(errors$DateTime[i], "\n"))
      }
      act_data <- act_data[-err,]
    }

    act_data$DateTime <- as.POSIXct(act_data$DateTime, format = "%d/%m/%y %H:%M:%OS")
    act_time_skeleton <-
      seq(min(act_data$DateTime), max(act_data$DateTime), by = paste(sampling.interval, "secs"))
    act_time_grouping <-
      rep(seq(min(act_data$DateTime), max(act_data$DateTime), by = paste(summarise.interval, "mins")), each = summarise.interval*60/sampling.interval)
    length(act_time_grouping) <- length(act_time_skeleton)

    act <- tibble(dtime = act_time_grouping, DateTime = act_time_skeleton) |>
      left_join(act_data[,c(2,5)],
                by = c("DateTime" = "DateTime")) |>
      fill(state, .direction = "down") |>
      group_by(dtime) |>
      mutate(is_wet = ifelse(state == "wet", TRUE, FALSE)) |>
      summarise(wetdry = sum(is_wet)) |>
      ungroup()

  } else if (NCOL(act_data == 4)) {
    colnames(act_data) <- c("status", "dtime", "V3", "wetdry")

    err <- grep("^ERROR:*", act_data$status)
    errors <- act_data[err,]
    if (!nrow(errors) == 0) {
      for (i in 1:nrow(errors)) {
        cat(paste0(errors$status[i], "\n"))
        cat(paste0(errors$dtime[i], "\n"))
      }
      act_data <- act_data[-err,]
    }

    act_data$dtime <- as.POSIXct(act_data$dtime, format = "%d/%m/%y %H:%M:%OS")
    act <- subset(act_data, select = c(dtime, wetdry))
  }

  return(act)
}
