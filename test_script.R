######################################
# example black browed albatross track
######################################
library(probGLS)
library(GeoLight)

# define start and end datetimes ----
start <- as.POSIXct("2014-12-13 17:55", tz="UTC")
end   <- as.POSIXct("2014-12-22 08:55", tz="UTC")

# light data ----
trn           <- twilightCalc(BBA_lux$dtime, BBA_lux$lig, ask = FALSE, LightThreshold = 2)

# sst data ----
sen           <- sst_deduction(datetime = BBA_sst$dtime, temp = BBA_sst$temp, temp.range = c(-2,30))

# wet dry data ----
act           <- BBA_deg[BBA_deg$wet.dry=="wet",]
act$wetdry    <- act$duration

# twilight error distribution estimation ----
tw            <- twilight_error_estimation()

land.mask.mod <- modify.land.mask(north.pacific.ocean = T, south.pacific.ocean = T)

# download environmental data ----

# download yearly NetCDF files for (replace YEAR with appropriate number):
# daily mean SST                   -> 'sst.day.mean.YEAR.v2.nc'
# daily SST error                  -> 'sst.day.err.YEAR.v2.nc'
# daily mean sea ice concentration -> 'icec.day.mean.YEAR.v2.nc'

# from:
# https://www.esrl.noaa.gov/psd/data/gridded/data.noaa.oisst.v2.highres.html
# and place all into the same folder

# Also, download the land mask file: 'lsmask.oisst.v2.nc' from the same directory
# and place it in the same folder as all the other NetCDF files

# run algorithm ----
pr   <- prob_algorithm(trn                         = trn,
                      sensor                      = sen,
                      act                         = act,
                      tagging.date                = start,
                      retrieval.date              = end,
                      loess.quartile              = NULL,
                      tagging.location            = c(-36.816,-54.316),
                      particle.number             = 1000,
                      iteration.number            = 100,
                      sunrise.sd                  = tw,
                      sunset.sd                   = tw,
                      range.solar                 = c(-7,-1),
                      boundary.box                = c(-120,40,-90,0),
                      days.around.spring.equinox  = c(0,0),
                      days.around.fall.equinox    = c(0,0),
                      speed.dry                   = c(12,6,45),
                      speed.wet                   = c(1,1.3,5),
                      sst.sd                      = 0.5,
                      max.sst.diff                = 3,
                      east.west.comp              = T,
                      land.mask                   = T,
                      ice.conc.cutoff             = 1,
                      wetdry.resolution           = 1,
                      NOAA.OI.location            = "EnvironmentalData_BAS")

plot_map(pr)

library(probGLSAlgorithm)

land.mask.mod <- modify.land.mask(custom.land.mask = list(c(-120,40,-90,0)))
land.mask.mod <- modify.land.mask(arctic.ocean = T)

# run algorithm ----
obj   <- GLS.prob.algorithm(trn                         = trn,
                       sensor                      = sen,
                       act                         = act,
                       tagging.date                = start,
                       retrieval.date              = end,
                       loess.quartile              = NULL,
                       tagging.location            = c(-36.816,-54.316),
                       particle.number             = 1000,
                       iteration.number            = 100,
                       sunrise.sd                  = tw,
                       sunset.sd                   = tw,
                       range.solar                 = c(-7,-1),
                       boundary.box                = c(-120,40,-90,0),
                       days.around.spring.equinox  = c(0,0),
                       days.around.fall.equinox    = c(0,0),
                       speed.dry                   = c(12,6,45),
                       speed.wet                   = c(1,1.3,5),
                       sst.sd                      = 0.5,
                       max.sst.diff                = 3,
                       east.west.comp              = T,
                       land.mask                   = T,
                       ice.conc.cutoff             = 1,
                       wetdry.resolution           = 1,
                       NOAA.OI.location            = "EnvironmentalData_BAS",
                       land.mask.mod               = land.mask.mod)


