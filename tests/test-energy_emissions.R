# Calling the testthat and climateconversions libraries
library(testthat)
library(climateconversions)

# Testing the energy_emissions function

test_that("energy_emissions handles kilowatt-hour inputs correctly", {
  expect_equal(energy_emissions(1000), 0.417)
})

test_that("energy_emissions handles gigawatt-hour inputs correctly", {
  expect_equal(
    energy_emissions(consumption = 1, units = "gigawatt-hours"),
    417
    )
})

test_that("energy_emissions handles watt-hour inputs correctly", {
  expect_equal(
    energy_emissions(consumption = 1000000, units = "watt-hours"),
    0.417
    )
})

test_that("energy_emissions handles string inputs that can be converted to numeric", {
  expect_equal(
    energy_emissions("1500.5", units = "kilowatt-hours"), 0.6257085
    )
  expect_equal(
    energy_emissions(consumption = "1", units = "gigawatt-hours"), 417
    )
  expect_equal(
    energy_emissions(consumption = "1000000", units = "watt-hours"), 0.417
    )
})

test_that("energy_emissions returns an error for unsupported units", {
  expect_error(
    energy_emissions(consumption = 1000, units = "megawatt-hours"),
    "Units must be either gigawatt-hours, kilowatt-hours, or watt-hours"
    )
})

test_that("energy_emissions returns an error for non-numeric string inputs", {
  expect_error(
    energy_emissions("one thousand"),
    "Consumption input values are not numeric. Please input numeric data."
    )
})

test_that("energy_emissions handles NA inputs and returns a warning and NA", {
  expect_warning(expect_equal(energy_emissions(NA), NA))
  expect_warning(expect_equal(energy_emissions(consumption = 1000, units = NA), NA))
})

test_that("energy_emissions handles vectors of inputs", {
  expect_equal(
    energy_emissions(c(1000, 2000)),
    c(0.417, 0.834)
    )
  expect_equal(
    energy_emissions(consumption = c(1, 2), units = "gigawatt-hours"),
    c(417, 834)
    )
  expect_equal(
    energy_emissions(consumption = c(1000000, 2000000), units = "watt-hours"),
    c(0.417, 0.834)
    )
})

test_that("energy_emissions returns an error for non-numeric inputs in vectors", {
  expect_error(
    energy_emissions(c("one thousand", 2000)),
    "Consumption input values are not numeric. Please input numeric data."
    )
})

test_that("energy_emissions handles dataframes as inputs", {
  data <- data.frame(
    neighborhood = c("greenville", "highlands ranch"),
    consumption = c(1000, 2000)
    )
  expect_equal(
    energy_emissions(data$consumption),
    c(0.417, 0.834)
    )
})
