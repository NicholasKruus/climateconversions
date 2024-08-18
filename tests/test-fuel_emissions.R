# Calling the testthat and climateconversions libraries
library(testthat)
library(climateconversions)

# Testing the fuel_emissions function

test_that("fuel_emissions handles gallon inputs correctly", {
  expect_equal(
    fuel_emissions(gasoline = 10, diesel = 5), 0.13977
    )
})

test_that("fuel_emissions handles liter inputs correctly", {
  expect_equal(
    fuel_emissions(gasoline = 10, diesel = 5, units = "liters"),
    0.03692332775
  )
})

test_that("fuel_emissions handles quart inputs correctly", {
  expect_equal(
    fuel_emissions(gasoline = 10, diesel = 5, units = "quarts"),
    0.0349425
  )
})

test_that("fuel_emissions handles string inputs that can be converted to numeric", {
  expect_equal(
    fuel_emissions(gasoline = "10.5", diesel = "5.2"), 0.1462495
    )
})

test_that("fuel_emissions returns an error for non-numeric string inputs", {
  expect_error(
    fuel_emissions(gasoline = "ten", diesel = 5),
    "Inputs for gasoline, diesel, or both contain values that are not numeric."
    )
})

test_that("fuel_emissions handles NA inputs by returning a warning and NA", {
  expect_warning(
    expect_equal(fuel_emissions(gasoline = NA, diesel = 5), NA)
    )
  expect_warning(
    expect_equal(fuel_emissions(gasoline = 10, diesel = NA), NA)
    )
  expect_warning(
    expect_equal(fuel_emissions(gasoline = NA, diesel = NA), NA)
    )
  expect_warning(
    expect_equal(fuel_emissions(gasoline = 10, diesel = 5, units = NA), NA)
  )
})

test_that("fuel_emissions returns an error for non-numeric inputs in vectors", {
  expect_error(
    fuel_emissions(gasoline = c("ten", 20), diesel = c(5, 10)),
    "Inputs for gasoline, diesel, or both contain values that are not numeric."
    )
})

test_that("fuel_emissions handles vectors of inputs", {
  expect_equal(
    fuel_emissions(gasoline = c(10, 20), diesel = c(5, 10)),
    c(0.13977, 0.27954)
    )
})

test_that("fuel_emissions handles dataframes as inputs", {
  data <- data.frame(gasoline = c(10, 20), diesel = c(5, 10))
  expect_equal(
    fuel_emissions(data$gasoline, data$diesel),
    c(0.13977, 0.27954)
    )
})

test_that("fuel_emissions returns an error for unsupported units", {
  expect_error(
    fuel_emissions(gasoline = 10, diesel = 5, units = "cubic meters"),
    "Units must be either gallons, liters, or quarts"
    )
  expect_error(
    fuel_emissions(gasoline = 10, diesel = 5, units = "ounces"),
    "Units must be either gallons, liters, or quarts"
    )
})

test_that("fuel_emissions handles non-lowercase unit inputs", {
  expect_equal(
    fuel_emissions(gasoline = 10, diesel = 5, units = "Liters"),
    0.03692332775
    )
  expect_equal(
    fuel_emissions(gasoline = 10, diesel = 5, units = "QUARTS"),
    0.0349425
    )
})
