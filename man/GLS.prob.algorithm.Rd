% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GLS.prob.algorithm.R
\name{GLS.prob.algorithm}
\alias{GLS.prob.algorithm}
\title{Probabilistic algorithm for geolocation using light-level loggers}
\usage{
GLS.prob.algorithm(
  particle.number = 2000,
  iteration.number = 60,
  loess.quartile = NULL,
  tagging.location = c(0, 0),
  tagging.date,
  retrieval.date,
  sunrise.sd = twilight_error_estimation(2.49, 0.94, 4.98),
  sunset.sd = twilight_error_estimation(2.49, 0.94, 4.98),
  range.solar = c(-7, -1),
  speed.wet = c(20, 0.2, 25),
  speed.dry = c(20, 0.2, 25),
  sst.sd = 0.5,
  max.sst.diff = 3,
  days.around.spring.equinox = c(10, 10),
  days.around.fall.equinox = c(10, 10),
  ice.conc.cutoff = 1,
  boundary.box = c(-180, 180, -90, 90),
  east.west.comp = T,
  land.mask = T,
  sensor,
  trn,
  act,
  wetdry.resolution = 30,
  backward = F,
  NOAA.OI.location = NULL,
  land.mask.mod = NULL
)
}
\arguments{
\item{particle.number}{number of particles for each location cloud used in the model}

\item{iteration.number}{number of iterations}

\item{loess.quartile}{quartiles for loessFilter (GeoLight), if NULL loess filter is not used}

\item{tagging.location}{tagging location longitude and latitude}

\item{tagging.date}{deployment data as POSIXct or Date object}

\item{retrieval.date}{retrieval date as POSIXct or Date object}

\item{sunrise.sd}{output vector from twilight_error_estimation}

\item{sunset.sd}{output vector from twilight_error_estimation}

\item{range.solar}{min and max of solar angle range in degree}

\item{speed.wet}{optimal speed, speed standard deviation and max speed allowed if logger is wet in m/s}

\item{speed.dry}{optimal speed, speed standard deviation and max speed allowed if logger is dry in m/s}

\item{sst.sd}{SST standard deviation in degree C}

\item{max.sst.diff}{max difference in SST allowed in degree C}

\item{days.around.spring.equinox}{days before the Spring equinox and days after the Spring equinox. The Spring equinox is assumed constant at 20 March.}

\item{days.around.fall.equinox}{days before the Fall equinox, days after the Fall equinox. The Fall equinox is assumed constant at 22 September.}

\item{ice.conc.cutoff}{max percentage of sea ice in which the animal is believed to be}

\item{boundary.box}{min lon, max lon, min lat and max lat of extrem boundary where you expect an animal to be}

\item{east.west.comp}{if T apply biotrack east west movement compensation (Biotrack manual v11 page 31pp.)}

\item{land.mask}{if T animal is only using ocean areas, if F animal is only using land areas, if NULL no land mask used}

\item{sensor}{data.frame with daily SST data deduced from tag temperature readings (sst_deduction ouput), NULL if no SST data is available (SST will not be used)}

\item{trn}{data.frame containing twilights and at least tFirst, tSecond and type (same as computed by trn_to_dataframe, ipe_to_dataframe or lotek_to_dataframe)}

\item{act}{data.frame containing wet dry data (e.g. .act file from Biotrack loggers or .deg file from migrate tech loggers), NULL if no wetdry data is available (algorithm will assume that the logger was always dry)}

\item{wetdry.resolution}{sampling rate of conductivity switch in sec (e.g. MK15 & MK3006 sample every 3 sec).}

\item{backward}{run algorithm in reverse.}

\item{NOAA.OI.location}{directory location of NOAA OI V2 NCDF files as well as land mask file 'lsmask.oisst.v2.nc' (downloadable from http://www.esrl.noaa.gov/psd/data/gridded/data.noaa.oisst.v2.highres.html).}

\item{land.mask.mod}{dataframe containing the geographic bounds of any modifications to the land mask.}
}
\value{
A list with: [1] all positions, [2] geographic median positions, [3] all possible particles, [4] input parameters, [5] model run time, [6] list of location estimates plots per timestep. List items 1 to 3 are returned as SpatialPointsDataframe.
}
\description{
Algorithm for more accurately tracking the migration of species that cross the equator relative to the probGLS algorithm
}
