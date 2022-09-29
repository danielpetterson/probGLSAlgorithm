library(probGLSAlgorithm)

# light data
trn           <- GeoLight::twilightCalc(probGLS::BBA_lux$dtime, probGLS::BBA_lux$lig, ask = FALSE, LightThreshold = 2)
# sst data
sensor           <- probGLS::sst_deduction(datetime = probGLS::BBA_sst$dtime, temp = probGLS::BBA_sst$temp, temp.range = c(-2,30))
# wet dry data
act           <- probGLS::BBA_deg[probGLS::BBA_deg$wet.dry=="wet",]
act$wetdry    <- act$duration
# land mask mod
land.mask.mod <- modify.land.mask(med.sea = T)

test_that("Input Type", {

  #check that function completes and generates time to complete message
  expect_output(GLS.prob.algorithm(particle.number = 2,
                                   iteration.number = 3,
                                   loess.quartile = NULL,
                                   tagging.location = c(-36.816,-54.316), #dataset-specific
                                   tagging.date = as.POSIXct("2014-12-13 17:55", tz="UTC"), #dataset-specific
                                   retrieval.date = as.POSIXct("2014-12-22 08:55", tz="UTC"), #dataset-specific
                                   sunrise.sd = probGLS::twilight_error_estimation(2.49, 0.94, 4.98), #generic for open-habitiat species like seabirds
                                   sunset.sd = probGLS::twilight_error_estimation(2.49, 0.94,4.98),#generic for open-habitiat species like seabirds
                                   range.solar = c(-7,-1),
                                   speed.wet = c(20, 0.2, 25), #species-specific
                                   speed.dry = c(20, 0.2, 25), #species-specific
                                   sst.sd = 0.5,
                                   max.sst.diff = 3,
                                   days.around.spring.equinox = c(10, 10),
                                   days.around.fall.equinox = c(10,10),
                                   ice.conc.cutoff = 1,
                                   boundary.box = c(-120,40,-90,0), #species-specific
                                   east.west.comp = T, #related to tagging location
                                   land.mask = T, #function fails if F due to data being from seabirds
                                   sensor = sensor,
                                   trn = trn,
                                   act = act,
                                   wetdry.resolution = 30, #device/manufacturer dependent
                                   backward = F,
                                   NOAA.OI.location = "EnvironmentalData_BAS", #location must be within testing folder due to issues with relative path
                                   land.mask.mod = land.mask.mod,
                                   cross.hemisphere.migration.dates = NULL),
                "algorithm run time:")

  # Test that particle.number is greater than zero
  expect_error(GLS.prob.algorithm(particle.number = 0,
                                  iteration.number = 3,
                                  loess.quartile = NULL,
                                  tagging.location = c(-36.816,-54.316), #dataset-specific
                                  tagging.date = as.POSIXct("2014-12-13 17:55", tz="UTC"), #dataset-specific
                                  retrieval.date = as.POSIXct("2014-12-22 08:55", tz="UTC"), #dataset-specific
                                  sunrise.sd = probGLS::twilight_error_estimation(2.49, 0.94, 4.98), #generic for open-habitiat species like seabirds
                                  sunset.sd = probGLS::twilight_error_estimation(2.49, 0.94,4.98),#generic for open-habitiat species like seabirds
                                  range.solar = c(-7,-1),
                                  speed.wet = c(20, 0.2, 25), #species-specific
                                  speed.dry = c(20, 0.2, 25), #species-specific
                                  sst.sd = 0.5,
                                  max.sst.diff = 3,
                                  days.around.spring.equinox = c(10, 10),
                                  days.around.fall.equinox = c(10,10),
                                  ice.conc.cutoff = 1,
                                  boundary.box = c(-120,40,-90,0), #species-specific
                                  east.west.comp = T, #related to tagging location
                                  land.mask = T, #function fails if F due to data being from seabirds
                                  sensor = sensor,
                                  trn = trn,
                                  act = act,
                                  wetdry.resolution = 30, #device/manufacturer dependent
                                  backward = F,
                                  NOAA.OI.location = "EnvironmentalData_BAS", #location must be within testing folder due to issues with relative path
                                  land.mask.mod = land.mask.mod,
                                  cross.hemisphere.migration.dates = NULL), "particle.number should be an int greater than zero.")



  # # Test that tagging loc is within bounds
  # expect_error(GLS.prob.algorithm(particle.number = 2,
  #                                 iteration.number = 3,
  #                                 loess.quartile = NULL,
  #                                 tagging.location = c(-181,100), #dataset-specific
  #                                 tagging.date = as.POSIXct("2014-12-13 17:55", tz="UTC"), #dataset-specific
  #                                 retrieval.date = as.POSIXct("2014-12-22 08:55", tz="UTC"), #dataset-specific
  #                                 sunrise.sd = probGLS::twilight_error_estimation(2.49, 0.94, 4.98), #generic for open-habitiat species like seabirds
  #                                 sunset.sd = probGLS::twilight_error_estimation(2.49, 0.94,4.98),#generic for open-habitiat species like seabirds
  #                                 range.solar = c(-7,-1),
  #                                 speed.wet = c(20, 0.2, 25), #species-specific
  #                                 speed.dry = c(20, 0.2, 25), #species-specific
  #                                 sst.sd = 0.5,
  #                                 max.sst.diff = 3,
  #                                 days.around.spring.equinox = c(10, 10),
  #                                 days.around.fall.equinox = c(10,10),
  #                                 ice.conc.cutoff = 1,
  #                                 boundary.box = c(-120,40,-90,0), #species-specific
  #                                 east.west.comp = T, #related to tagging location
  #                                 land.mask = T, #function fails if F due to data being from seabirds
  #                                 sensor = sensor,
  #                                 trn = trn,
  #                                 act = act,
  #                                 wetdry.resolution = 30, #device/manufacturer dependent
  #                                 backward = F,
  #                                 NOAA.OI.location = "EnvironmentalData_BAS", #location must be within testing folder due to issues with relative path
  #                                 land.mask.mod = land.mask.mod,
  #                                 cross.hemisphere.migration.dates = NULL), "tagging.location needs to be (longitude, latitude) representing the initial location.")

  # Test that tagging.date won't accept non-POSIXct dates
  expect_error(GLS.prob.algorithm(particle.number = 2,
                                  iteration.number = 3,
                                  loess.quartile = NULL,
                                  tagging.location = c(-36.816,-54.316), #dataset-specific
                                  tagging.date = 2014-12-13, #dataset-specific
                                  retrieval.date = as.POSIXct("2014-12-22 08:55", tz="UTC"), #dataset-specific
                                  sunrise.sd = probGLS::twilight_error_estimation(2.49, 0.94, 4.98), #generic for open-habitiat species like seabirds
                                  sunset.sd = probGLS::twilight_error_estimation(2.49, 0.94,4.98),#generic for open-habitiat species like seabirds
                                  range.solar = c(-7,-1),
                                  speed.wet = c(20, 0.2, 25), #species-specific
                                  speed.dry = c(20, 0.2, 25), #species-specific
                                  sst.sd = 0.5,
                                  max.sst.diff = 3,
                                  days.around.spring.equinox = c(10, 10),
                                  days.around.fall.equinox = c(10,10),
                                  ice.conc.cutoff = 1,
                                  boundary.box = c(-120,40,-90,0), #species-specific
                                  east.west.comp = T, #related to tagging location
                                  land.mask = T, #function fails if F due to data being from seabirds
                                  sensor = sensor,
                                  trn = trn,
                                  act = act,
                                  wetdry.resolution = 30, #device/manufacturer dependent
                                  backward = F,
                                  NOAA.OI.location = "EnvironmentalData_BAS", #location must be within testing folder due to issues with relative path
                                  land.mask.mod = land.mask.mod,
                                  cross.hemisphere.migration.dates = NULL), "tagging.date is not a datetime.")

  # Test that retrieval.date won't accept non-POSIXct dates
  expect_error(GLS.prob.algorithm(particle.number = 2,
                                  iteration.number = 3,
                                  loess.quartile = NULL,
                                  tagging.location = c(-36.816,-54.316), #dataset-specific
                                  tagging.date = as.POSIXct("2014-12-13 17:55", tz="UTC"), #dataset-specific
                                  retrieval.date = 2014/12/22, #dataset-specific
                                  sunrise.sd = probGLS::twilight_error_estimation(2.49, 0.94, 4.98), #generic for open-habitiat species like seabirds
                                  sunset.sd = probGLS::twilight_error_estimation(2.49, 0.94,4.98),#generic for open-habitiat species like seabirds
                                  range.solar = c(-7,-1),
                                  speed.wet = c(20, 0.2, 25), #species-specific
                                  speed.dry = c(20, 0.2, 25), #species-specific
                                  sst.sd = 0.5,
                                  max.sst.diff = 3,
                                  days.around.spring.equinox = c(10, 10),
                                  days.around.fall.equinox = c(10,10),
                                  ice.conc.cutoff = 1,
                                  boundary.box = c(-120,40,-90,0), #species-specific
                                  east.west.comp = T, #related to tagging location
                                  land.mask = T, #function fails if F due to data being from seabirds
                                  sensor = sensor,
                                  trn = trn,
                                  act = act,
                                  wetdry.resolution = 30, #device/manufacturer dependent
                                  backward = F,
                                  NOAA.OI.location = "EnvironmentalData_BAS", #location must be within testing folder due to issues with relative path
                                  land.mask.mod = land.mask.mod,
                                  cross.hemisphere.migration.dates = NULL), "retrieval.date is not a datetime.")

  # Test that range.solar requires a numeric vector of length 2
  expect_error(GLS.prob.algorithm(particle.number = 2,
                                  iteration.number = 3,
                                  loess.quartile = NULL,
                                  tagging.location = c(-36.816,-54.316), #dataset-specific
                                  tagging.date = as.POSIXct("2014-12-13 17:55", tz="UTC"), #dataset-specific
                                  retrieval.date = as.POSIXct("2014-12-22 08:55", tz="UTC"), #dataset-specific
                                  sunrise.sd = probGLS::twilight_error_estimation(2.49, 0.94, 4.98), #generic for open-habitiat species like seabirds
                                  sunset.sd = probGLS::twilight_error_estimation(2.49, 0.94,4.98),#generic for open-habitiat species like seabirds
                                  range.solar = c(-7,-1, "hi"),
                                  speed.wet = c(20, 0.2, 25), #species-specific
                                  speed.dry = c(20, 0.2, 25), #species-specific
                                  sst.sd = 0.5,
                                  max.sst.diff = 3,
                                  days.around.spring.equinox = c(10, 10),
                                  days.around.fall.equinox = c(10,10),
                                  ice.conc.cutoff = 1,
                                  boundary.box = c(-120,40,-90,0), #species-specific
                                  east.west.comp = T, #related to tagging location
                                  land.mask = T, #function fails if F due to data being from seabirds
                                  sensor = sensor,
                                  trn = trn,
                                  act = act,
                                  wetdry.resolution = 30, #device/manufacturer dependent
                                  backward = F,
                                  NOAA.OI.location = "EnvironmentalData_BAS", #location must be within testing folder due to issues with relative path
                                  land.mask.mod = land.mask.mod,
                                  cross.hemisphere.migration.dates = NULL), "range.solar should be a vector representing the min and max of solar angle range in degrees.")

  # Test that speed.wet is a numeric vector of length 3
  expect_error(GLS.prob.algorithm(particle.number = 2,
                                  iteration.number = 3,
                                  loess.quartile = NULL,
                                  tagging.location = c(-36.816,-54.316), #dataset-specific
                                  tagging.date = as.POSIXct("2014-12-13 17:55", tz="UTC"), #dataset-specific
                                  retrieval.date = as.POSIXct("2014-12-22 08:55", tz="UTC"), #dataset-specific
                                  sunrise.sd = probGLS::twilight_error_estimation(2.49, 0.94, 4.98), #generic for open-habitiat species like seabirds
                                  sunset.sd = probGLS::twilight_error_estimation(2.49, 0.94,4.98),#generic for open-habitiat species like seabirds
                                  range.solar = c(-7,-1),
                                  speed.wet = c(20, 0.2, 25, 10), #species-specific
                                  speed.dry = c(20, 0.2, 25), #species-specific
                                  sst.sd = 0.5,
                                  max.sst.diff = 3,
                                  days.around.spring.equinox = c(10, 10),
                                  days.around.fall.equinox = c(10,10),
                                  ice.conc.cutoff = 1,
                                  boundary.box = c(-120,40,-90,0), #species-specific
                                  east.west.comp = T, #related to tagging location
                                  land.mask = T, #function fails if F due to data being from seabirds
                                  sensor = sensor,
                                  trn = trn,
                                  act = act,
                                  wetdry.resolution = 30, #device/manufacturer dependent
                                  backward = F,
                                  NOAA.OI.location = "EnvironmentalData_BAS", #location must be within testing folder due to issues with relative path
                                  land.mask.mod = land.mask.mod,
                                  cross.hemisphere.migration.dates = NULL), "speed.wet should be a vector representing optimal speed, speed standard deviation and max speed allowed if logger is wet in m/s")

    # Test that speed.dry is a numeric vector of length 3
    expect_error(GLS.prob.algorithm(particle.number = 2,
                                    iteration.number = 3,
                                    loess.quartile = NULL,
                                    tagging.location = c(-36.816,-54.316), #dataset-specific
                                    tagging.date = as.POSIXct("2014-12-13 17:55", tz="UTC"), #dataset-specific
                                    retrieval.date = as.POSIXct("2014-12-22 08:55", tz="UTC"), #dataset-specific
                                    sunrise.sd = probGLS::twilight_error_estimation(2.49, 0.94, 4.98), #generic for open-habitiat species like seabirds
                                    sunset.sd = probGLS::twilight_error_estimation(2.49, 0.94,4.98),#generic for open-habitiat species like seabirds
                                    range.solar = c(-7,-1),
                                    speed.wet = c(20, 0.2, 25), #species-specific
                                    speed.dry = c(20, "hi", 25), #species-specific
                                    sst.sd = 0.5,
                                    max.sst.diff = 3,
                                    days.around.spring.equinox = c(10, 10),
                                    days.around.fall.equinox = c(10,10),
                                    ice.conc.cutoff = 1,
                                    boundary.box = c(-120,40,-90,0), #species-specific
                                    east.west.comp = T, #related to tagging location
                                    land.mask = T, #function fails if F due to data being from seabirds
                                    sensor = sensor,
                                    trn = trn,
                                    act = act,
                                    wetdry.resolution = 30, #device/manufacturer dependent
                                    backward = F,
                                    NOAA.OI.location = "EnvironmentalData_BAS", #location must be within testing folder due to issues with relative path
                                    land.mask.mod = land.mask.mod,
                                    cross.hemisphere.migration.dates = NULL), "speed.dry should be a vector representing optimal speed, speed standard deviation and max speed allowed if logger is dry in m/s")

  # Test that sst.sd is a positive number
  expect_error(GLS.prob.algorithm(particle.number = 2,
                                  iteration.number = 3,
                                  loess.quartile = NULL,
                                  tagging.location = c(-36.816,-54.316), #dataset-specific
                                  tagging.date = as.POSIXct("2014-12-13 17:55", tz="UTC"), #dataset-specific
                                  retrieval.date = as.POSIXct("2014-12-22 08:55", tz="UTC"), #dataset-specific
                                  sunrise.sd = probGLS::twilight_error_estimation(2.49, 0.94, 4.98), #generic for open-habitiat species like seabirds
                                  sunset.sd = probGLS::twilight_error_estimation(2.49, 0.94,4.98),#generic for open-habitiat species like seabirds
                                  range.solar = c(-7,-1),
                                  speed.wet = c(20, 0.2, 25), #species-specific
                                  speed.dry = c(20, 0.2, 25), #species-specific
                                  sst.sd = -0.5,
                                  max.sst.diff = 3,
                                  days.around.spring.equinox = c(10, 10),
                                  days.around.fall.equinox = c(10,10),
                                  ice.conc.cutoff = 1,
                                  boundary.box = c(-120,40,-90,0), #species-specific
                                  east.west.comp = T, #related to tagging location
                                  land.mask = T, #function fails if F due to data being from seabirds
                                  sensor = sensor,
                                  trn = trn,
                                  act = act,
                                  wetdry.resolution = 30, #device/manufacturer dependent
                                  backward = F,
                                  NOAA.OI.location = "EnvironmentalData_BAS", #location must be within testing folder due to issues with relative path
                                  land.mask.mod = land.mask.mod,
                                  cross.hemisphere.migration.dates = NULL), "sst.sd should be a non-negative number of length one.")

  # Test that max.sst.diff is a non-negative number
  expect_error(GLS.prob.algorithm(particle.number = 2,
                                  iteration.number = 3,
                                  loess.quartile = NULL,
                                  tagging.location = c(-36.816,-54.316), #dataset-specific
                                  tagging.date = as.POSIXct("2014-12-13 17:55", tz="UTC"), #dataset-specific
                                  retrieval.date = as.POSIXct("2014-12-22 08:55", tz="UTC"), #dataset-specific
                                  sunrise.sd = probGLS::twilight_error_estimation(2.49, 0.94, 4.98), #generic for open-habitiat species like seabirds
                                  sunset.sd = probGLS::twilight_error_estimation(2.49, 0.94,4.98),#generic for open-habitiat species like seabirds
                                  range.solar = c(-7,-1),
                                  speed.wet = c(20, 0.2, 25), #species-specific
                                  speed.dry = c(20, 0.2, 25), #species-specific
                                  sst.sd = 0.5,
                                  max.sst.diff = -0.1,
                                  days.around.spring.equinox = c(10, 10),
                                  days.around.fall.equinox = c(10,10),
                                  ice.conc.cutoff = 1,
                                  boundary.box = c(-120,40,-90,0), #species-specific
                                  east.west.comp = T, #related to tagging location
                                  land.mask = T, #function fails if F due to data being from seabirds
                                  sensor = sensor,
                                  trn = trn,
                                  act = act,
                                  wetdry.resolution = 30, #device/manufacturer dependent
                                  backward = F,
                                  NOAA.OI.location = "EnvironmentalData_BAS", #location must be within testing folder due to issues with relative path
                                  land.mask.mod = land.mask.mod,
                                  cross.hemisphere.migration.dates = NULL), "max.sst.diff should be a number equal to or greater than zero.")


})

test_that("The structure of the output of GLS.prob.algorithm", {

  output <- GLS.prob.algorithm(particle.number = 2,
                               iteration.number = 3,
                               loess.quartile = NULL,
                               tagging.location = c(-36.816,-54.316), #dataset-specific
                               tagging.date = as.POSIXct("2014-12-13 17:55", tz="UTC"), #dataset-specific
                               retrieval.date = as.POSIXct("2014-12-22 08:55", tz="UTC"), #dataset-specific
                               sunrise.sd = probGLS::twilight_error_estimation(2.49, 0.94, 4.98), #generic for open-habitiat species like seabirds
                               sunset.sd = probGLS::twilight_error_estimation(2.49, 0.94,4.98),#generic for open-habitiat species like seabirds
                               range.solar = c(-7,-1),
                               speed.wet = c(20, 0.2, 25), #species-specific
                               speed.dry = c(20, 0.2, 25), #species-specific
                               sst.sd = 0.5,
                               max.sst.diff = 3,
                               days.around.spring.equinox = c(10, 10),
                               days.around.fall.equinox = c(10,10),
                               ice.conc.cutoff = 1,
                               boundary.box = c(-120,40,-90,0), #species-specific
                               east.west.comp = T, #related to tagging location
                               land.mask = T, #function fails if F due to data being from seabirds
                               sensor = sensor,
                               trn = trn,
                               act = act,
                               wetdry.resolution = 30, #device/manufacturer dependent
                               backward = F,
                               NOAA.OI.location = "EnvironmentalData_BAS", #location must be within testing folder due to issues with relative path
                               land.mask.mod = land.mask.mod,
                               cross.hemisphere.migration.dates = NULL)

  # Check length of output object and types of each element of the object
  expect_length(output, 6)
  expect_true(is(output,"glsTracks"))
  expect_true(is(output[[1]][1],"SpatialPointsDataFrame"))
  expect_true(is(output[[2]][1],"SpatialPointsDataFrame"))
  expect_true(is(output[[3]][1],"SpatialPointsDataFrame"))
  expect_true(is(output[[4]],"data.frame"))
  expect_true(is(output[[5]],"difftime"))
  expect_true(is(output[[6]][1],"list"))
  expect_true(is(output[[6]][2],"list"))

})


test_that("Checks the output of GLS.prob.algorithm for a specific example dataset.", {

  set.seed(123)
  reference   <- probGLS::prob_algorithm(particle.number = 2,
                                         iteration.number = 3,
                                         loess.quartile = NULL,
                                         tagging.location = c(-36.816,-54.316), #dataset-specific
                                         tagging.date = as.POSIXct("2014-12-13 17:55", tz="UTC"), #dataset-specific
                                         retrieval.date = as.POSIXct("2014-12-22 08:55", tz="UTC"), #dataset-specific
                                         sunrise.sd = probGLS::twilight_error_estimation(2.49, 0.94, 4.98), #generic for open-habitiat species like seabirds
                                         sunset.sd = probGLS::twilight_error_estimation(2.49, 0.94,4.98),#generic for open-habitiat species like seabirds
                                         range.solar = c(-7,-1),
                                         speed.wet = c(20, 0.2, 25), #species-specific
                                         speed.dry = c(20, 0.2, 25), #species-specific
                                         sst.sd = 0.5,
                                         max.sst.diff = 3,
                                         days.around.spring.equinox = c(10, 10),
                                         days.around.fall.equinox = c(10,10),
                                         ice.conc.cutoff = 1,
                                         boundary.box = c(-120,40,-90,0), #species-specific
                                         east.west.comp = T, #related to tagging location
                                         land.mask = T, #function fails if F due to data being from seabirds
                                         sensor = sensor,
                                         trn = trn,
                                         act = act,
                                         wetdry.resolution = 30, #device/manufacturer dependent
                                         backward = F,
                                         NOAA.OI.location = "EnvironmentalData_BAS", #location must be within testing folder due to issues with relative path
                                         med.sea = T)

  set.seed(123)
  actual <- GLS.prob.algorithm(particle.number = 2,
                               iteration.number = 3,
                               loess.quartile = NULL,
                               tagging.location = c(-36.816,-54.316), #dataset-specific
                               tagging.date = as.POSIXct("2014-12-13 17:55", tz="UTC"), #dataset-specific
                               retrieval.date = as.POSIXct("2014-12-22 08:55", tz="UTC"), #dataset-specific
                               sunrise.sd = probGLS::twilight_error_estimation(2.49, 0.94, 4.98), #generic for open-habitiat species like seabirds
                               sunset.sd = probGLS::twilight_error_estimation(2.49, 0.94,4.98),#generic for open-habitiat species like seabirds
                               range.solar = c(-7,-1),
                               speed.wet = c(20, 0.2, 25), #species-specific
                               speed.dry = c(20, 0.2, 25), #species-specific
                               sst.sd = 0.5,
                               max.sst.diff = 3,
                               days.around.spring.equinox = c(10, 10),
                               days.around.fall.equinox = c(10,10),
                               ice.conc.cutoff = 1,
                               boundary.box = c(-120,40,-90,0), #species-specific
                               east.west.comp = T, #related to tagging location
                               land.mask = T, #function fails if F due to data being from seabirds
                               sensor = sensor,
                               trn = trn,
                               act = act,
                               wetdry.resolution = 30, #device/manufacturer dependent
                               backward = F,
                               NOAA.OI.location = "EnvironmentalData_BAS", #location must be within testing folder due to issues with relative path
                               land.mask.mod = modify.land.mask(med.sea = T))

  # check same output values given matching parameters
  expect_identical(reference[[1]], actual[[1]], "all tracks values are the same as probGLS values given identical parameters.")
  expect_identical(reference[[2]], actual[[2]], "most probable track values are the same as probGLS values given identical parameters.")
  expect_identical(reference[[3]], actual[[3]], "all possible particles values are the same as probGLS values given identical parameters.")

})

