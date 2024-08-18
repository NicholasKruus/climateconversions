# Defining the gwp function to convert emissions
# of various greenhouse gases into CO2-equivalent
# units of global warming impact.
gwp <- function(CO2 = 0,
        CH4 = 0,
        CF4 = 0,
        SF4 = 0,
        N20 = 0,
        CH3CHF2 = 0) {

  inputs <- c(CO2, CH4, CF4, SF4, N20, CH3CHF2)

  # Handling NA inputs
  if (any(is.na(inputs))) {
    warning("One or more inputs contain NA value(s). Returning NA.")
    return(NA)
  }
  # Ensuring inputs can be converted to numeric values
  if (any(is.na(suppressWarnings(as.numeric(inputs))))) {
    stop("One or more inputs contain values that are not numeric.")
  }

  # Converting inputs to numeric values
  CO2 <- as.numeric(CO2)
  CH4 <- as.numeric(CH4)
  CF4 <- as.numeric(CF4)
  SF4 <- as.numeric(SF4)
  N20 <- as.numeric(N20)
  CH3CHF2 <- as.numeric(CH3CHF2)

  # Converting greenhouse gas emissions into CO2-equivalent units
  CO2e <- (CO2 +
             CH4 * 27.9 +
             CF4 * 7380 +
             SF4 * 24300 +
             N20 * 273 +
             CH3CHF2 * 164)
}

