# Defining the fuel_emissions function to convert gasoline
# or diesel consumption (in gallons, liters, or quarts)
# into CO2 emissions (in metric tons)
fuel_emissions <- function(gasoline = 0, diesel = 0, units = "gallons") {

  # Handling NA inputs
  if (any(is.na(c(gasoline, diesel, units)))) {
    warning("One or more inputs contain NA value(s). Returning NA.")
    return(NA)
  }
  # Ensuring inputs can be converted to numeric values
  if (any(is.na(suppressWarnings(as.numeric(c(gasoline, diesel)))))) {
    stop("Inputs for gasoline, diesel, or both contain values that are not numeric.")
  }

  # Converting quantitative inputs to numeric values
  gasoline = as.numeric(gasoline)
  diesel = as.numeric(diesel)

  # Converting fuel consumption into metric tons of CO2
  if (tolower(units) == "gallons") {
    emissions = (gasoline * 8.887 * 10^(-3)) + (diesel * 10.180 * 10^(-3))
  } else if (tolower(units) == "liters") {
    emissions = (
      ((gasoline / 3.785411784) * 8.887 * 10^(-3))
      + ((diesel / 3.785411784) * 10.180 * 10^(-3))
      )
  } else if (tolower(units) == "quarts") {
    emissions = (
      ((gasoline / 4) * 8.887 * 10^(-3))
      + ((diesel / 4) * 10.180 * 10^(-3))
      )
  } else {
    stop("Units must be either gallons, liters, or quarts")
  }
}
