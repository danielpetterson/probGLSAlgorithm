test_that("Input Type", {

  # Test that input is of logical class and length 1
  expect_error(modify.land.mask(med.sea = 5), "med.sea must be either TRUE or FALSE.")
  expect_error(modify.land.mask(black.sea = "1"), "black.sea must be either TRUE or FALSE.")
  expect_error(modify.land.mask(baltic.sea = list(T)), "baltic.sea must be either TRUE or FALSE.")
  expect_silent(modify.land.mask(caspian.sea = c(T)))
  expect_error(modify.land.mask(arctic.ocean = NULL), "arctic.ocean must be either TRUE or FALSE.")
  expect_error(modify.land.mask(north.atlantic.ocean = c(F,T)), "north.atlantic.ocean must be either TRUE or FALSE.")
  expect_error(modify.land.mask(south.atlantic.ocean = as.Date("2021-01-12")), "south.atlantic.ocean must be either TRUE or FALSE.")
  expect_error(modify.land.mask(north.pacific.ocean = -Inf), "north.pacific.ocean must be either TRUE or FALSE.")
  expect_error(modify.land.mask(south.pacific.ocean = as.Date("2021-01-12")), "south.pacific.ocean must be either TRUE or FALSE.")
  expect_error(modify.land.mask(southern.ocean = c(T,"1",93)), "southern.ocean must be either TRUE or FALSE.")

  # Check acceptance of named lists
  list_input1 = list(
    A = c(1, 2, 3, 4),
    B = c(5, 4, 3, 2),
    C = c(2, 3, 4, 5)
  )
  expect_silent(modify.land.mask(custom.land.mask = list_input1))

  # Check acceptance of unnamed lists
  list_input2 = list(
    c(1, 2, 3, 4),
    c(5, 4, 3, 2),
    c(2, 3, 4, 5)
  )
  expect_silent(modify.land.mask(custom.land.mask = list_input2))

  # Check rejection of lists with elements not of length 4
  list_input3 = list(
    c(1, 2, 3, 4),
    c(5, 4, 3, 2, 5),
    c(2, 3, 4, 5)
  )
  expect_error(modify.land.mask(custom.land.mask = list_input3), "custom.land.mask must be a list of vectors of length 4 or NULL")

  list_input4 = list(
    c(1, 2, 3),
    c(5, 4, 3, 2),
    c(2, 3, 4, 5)
  )
  expect_error(modify.land.mask(custom.land.mask = list_input4), "custom.land.mask must be a list of vectors of length 4 or NULL")

  # Valid inputs testing
  expect_silent(
    modify.land.mask(
      med.sea = T,
      black.sea = T,
      baltic.sea = T,
      caspian.sea = T,
      arctic.ocean = T,
      north.atlantic.ocean = T,
      south.atlantic.ocean = T,
      north.pacific.ocean = T,
      south.pacific.ocean = T,
      southern.ocean = T,
      custom.land.mask = list(c(0, 0, 0, 0), c(1, 2, 3, 4))
    )
  )
  expect_silent(
    modify.land.mask(
      med.sea = F,
      black.sea = F,
      baltic.sea = F,
      caspian.sea = F,
      arctic.ocean = F,
      north.atlantic.ocean = F,
      south.atlantic.ocean = F,
      north.pacific.ocean = F,
      south.pacific.ocean = F,
      southern.ocean = F,
      custom.land.mask = NULL
    )
  )

})

test_that("The structure of the output of modify.land.mask", {

  output.all <- modify.land.mask(
    med.sea = T,
    black.sea = T,
    baltic.sea = T,
    caspian.sea = T,
    arctic.ocean = T,
    north.atlantic.ocean = T,
    south.atlantic.ocean = T,
    north.pacific.ocean = T,
    south.pacific.ocean = T,
    southern.ocean = T,
    custom.land.mask = list(c(0,0,0,0), c(1,2,3,4))
  )

  #check number of columns
  expect_true(length(output.all) == 4)
  #check output is landmask
  expect_true(class(output.all) == "landmask")
  #check col names
  expect_true(all(c("min.lon","max.lon","min.lat","max.lat") == names(output.all)))


})


test_that("checks the actual numerical result of the modify.land.mask for a specific set of arguments.", {

  output.check <- modify.land.mask(
    med.sea = T,
    black.sea = T,
    baltic.sea = T,
    caspian.sea = T,
    arctic.ocean = F,
    north.atlantic.ocean = F,
    south.atlantic.ocean = F,
    north.pacific.ocean = F,
    south.pacific.ocean = F,
    southern.ocean = F,
    custom.land.mask = list(c(0,0,0,0))
  )

  # Using expect_equal instead of expect_identical to allow for imperfect matching
  expect_identical(output.check$min.lon, c(14,0,27,355,27,45,0), "min.lon is the same as expected.")
  expect_identical(output.check$max.lon, c(33.5,27.0,40.0,360.0,45.0,62.0,0.0), "max.lon is the same as expected.")
  expect_identical(output.check$min.lat, c(51.4,30.0,30.0,30.0,40.0,35.0,0.0), "min.lat is the same as expected.")
  expect_identical(output.check$max.lat, c(66.2,48.0,40.0,42.0,48.0,48.0,0.0), "max.lat is the same as expected.")

})

