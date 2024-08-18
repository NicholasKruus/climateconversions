# Calling the testthat and climateconversions libraries
library(testthat)
library(climateconversions)

# Testing the gwp function

test_that("gwp handles numeric inputs correctly", {
  expect_equal(gwp(10, 5, 2, 1, 3, 4), 40684.5)
})

test_that("gwp handles string inputs that can be converted to numeric", {
  expect_equal(
    gwp("10.5", "5.2", "2.1", "1.3", "3.7", "4.6"),
    49008.08
    )
})

test_that("gwp returns an error for non-numeric string inputs", {
  expect_error(
    gwp("ten", 5, 2, 1, 3, 4),
    "One or more inputs contain values that are not numeric."
    )
  expect_error(
    gwp(10, 5, 2, 1, "three", 4),
    "One or more inputs contain values that are not numeric."
  )
})

test_that("gwp handles NA inputs by returning a warning and NA", {
  expect_warning(
    expect_equal(gwp(NA, 5, 2, 1, 3, 4), NA)
    )
  expect_warning(
    expect_equal(gwp(10, NA, 2, 1, 3, 4), NA)
    )
  expect_warning(
    expect_equal(gwp(10, 5, NA, 1, 3, 4), NA)
    )
  expect_warning(
    expect_equal(gwp(10, 5, 2, NA, 3, 4), NA)
    )
  expect_warning(
    expect_equal(gwp(10, 5, 2, 1, NA, 4), NA)
    )
  expect_warning(
    expect_equal(gwp(10, 5, 2, 1, 3, NA), NA)
    )
})

test_that("gwp handles vectors of inputs", {
  expect_equal(
    gwp(c(10, 20), c(5, 10), c(2, 3), c(1, 2), c(3, 4), c(4, 5)),
    c(40684.5, 72951)
    )
})

test_that("gwp returns an error for non-numeric inputs in vectors", {
  expect_error(
    gwp(c("ten", 20), c(5, 10), c(2, 3), c(1, 2), c(3, 4), c(4, 5)),
    "One or more inputs contain values that are not numeric."
    )
})

test_that("gwp handles dataframes as inputs", {
  data <- data.frame(CO2 = c(10, 20),
                   CH4 = c(5, 10),
                   CF4 = c(2, 3),
                   SF4 = c(1, 2),
                   N20 = c(3, 4),
                   CH3CHF2 = c(4, 5)
                   )
  expect_equal(
    gwp(data$CO2, data$CH4, data$CF4, data$SF4, data$N20, data$CH3CHF2),
    c(40684.5,72951)
    )
})
