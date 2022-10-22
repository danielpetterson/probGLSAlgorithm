library(probGLSAlgorithm)

# output <- read.sensor("tests/testthat/testdata/testfileSEN.tem", "tests/testthat/testdata/testfileSEN.act")

# test_that("Input type", {
#
#   expect_error(read.act("testdata/testfileERROR.act", sampling.interval = "test"), "sampling.interval should be the number of seconds between each conductivity measurement. This is device specific.")
#   expect_error(read.act("testdata/testfileERROR.act", summarise.interval = "test"), "summarise.interval should be the number of minutes over which the total number of wet recordings are aggregated.")
#
# })
#
# test_that("Output error when present", {
#
#   #check that function reads in file and generates error message
#   expect_output(read.act("testdata/testfileERROR.act"),
#                 "ERROR: 27/03/11 05:52:45\nlast two days data invalid; 1 missing samples\nERROR: 29/03/11 05:52:45\nlast two days data invalid; 1 extra samples")
#
#   #check for silence if no errors found
#   expect_silent(read.act("testdata/testfile.act"))
#
# })
#
# test_that("The structure of the output is as expected", {
#
#   output.error <- read.act("testdata/testfileERROR.act")
#
#   #check length of datetime vector
#   expect_length(output.error$dtime, 855)
#   #check that output is data.frame
#   expect_true(is(output.error,"data.frame"))
#   #check that datetime vector is correct type
#   expect_true(is(output.error$dtime[1], "POSIXct"))
#   #check that numeric vector is correct type
#   expect_true(is(output.error$wetdry, "integer"))
#   #check that first datetime value is correct
#   expect_true(output.error$dtime[1] == "2011-03-24 21:14:00 CET")
#
#   output <- read.act("testdata/testfile.act")
#
#   expect_length(output$dtime, 996)
#   expect_true(is(output,"data.frame"))
#   expect_true(is(output$dtime[1], "POSIXct"))
#   expect_true(is(output$wetdry, "integer"))
#   expect_true(output$dtime[1] == "2011-01-06 05:52:45 CET")
#
# })
