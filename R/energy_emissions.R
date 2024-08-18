# Defining the energy_emissions function to convert
# electricity consumption (in gigawatt-hours, kilowatt-hours,
# or watt-hours) into CO2 emissions (in metric tons)
energy_emissions <- function(consumption = 0, units = "kilowatt-hours") {

  # Handling NA inputs
  if (any(is.na(c(consumption, units)))) {
    warning("Input values contain NA value(s). Returning NA.")
    return(NA)
  }
  # Ensuring the consumption input can be converted to numeric values
  if (any(is.na(suppressWarnings(as.numeric(consumption))))) {
    stop("Consumption input values are not numeric. Please input numeric data.")
  }

  # Converting consumption input to numeric values
  consumption = as.numeric(consumption)

  # Converting energy use into metric tons of CO2
  if (tolower(units) == "kilowatt-hours") {
    emissions = consumption * 4.17 * 10^(-4)
  } else if (tolower(units) == "gigawatt-hours") {
    emissions = consumption * 4.17 * 100
  } else if (tolower(units) == "watt-hours") {
    emissions = (consumption / 1000) * 4.17 * 10^(-4)
  } else {
    stop("Units must be either gigawatt-hours, kilowatt-hours, or watt-hours")
  }
}
