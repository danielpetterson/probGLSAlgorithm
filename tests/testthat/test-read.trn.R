# output <- read.trn("tests/testthat/testdata/testfile.trn")
# output.error <- read.trn("tests/testthat/testdata/testfileERROR.trn")

test_that("Output error when present", {

  #check that function reads in file and generates error message
  expect_output(read.trn("testdata/testfileERROR.trn"),
                "ERROR: Device Failure 01 occurred after 08/10/11 17:33:00")

  #check for silence if no errors found
  expect_silent(read.trn("testdata/testfile.trn"))

})

test_that("The structure of the output is as expected", {

  output.error <- read.trn("testdata/testfileERROR.trn")

  #check length of datetime vector
  expect_length(output.error$tFirst, 19)
  #check that output is data.frame
  expect_true(is(output.error,"data.frame"))
  #check that datetime vector is correct type
  expect_true(is(output.error$tFirst[1], "POSIXct"))
  #check that numeric vector is correct type
  expect_true(is(output.error$type, "numeric"))
  #check that first tFirst datetime value is correct
  expect_identical(output.error$tFirst[1], as.POSIXct("2011-09-29 06:55:00", tz = "UTC"))
  #check that first tSecond datetime value is correct
  expect_identical(output.error$tSecond[1], as.POSIXct("2011-09-29 17:46:00", tz = "UTC"))

  output <- read.trn("testdata/testfile.trn")

  expect_length(output$tFirst, 19)
  expect_true(is(output,"data.frame"))
  expect_true(is(output$tFirst[1], "POSIXct"))
  expect_true(is(output$type, "numeric"))
  expect_identical(output$tFirst[1], as.POSIXct("2011-09-29 06:55:00", tz = "UTC"))
  expect_identical(output$tSecond[1], as.POSIXct("2011-09-29 17:46:00", tz = "UTC"))

})
