\name{gwp}
\alias{gwp}
\title{Global Warming Potential (gwp)}
\description{
Converts emissions of various greenhouse gases (in any unit of mass) into CO2-equivalent units of global warming impact. Convertible greenhouse gases include CO2, CH4, CF4, SF4, N20, and CH3CHF2. Equations are sourced from the US EPA's regulations (2009), and global warming potentials come from the Intergovernmental Panel on Climate Change (2021).
}
\usage{
gwp(CO2 = 0, CH4 = 0, CF4 = 0, SF4 = 0, N20 = 0, CH3CHF2 = 0)
}
\arguments{
All arguments may be numeric values, string values (containing only numbers), or vectors of numeric values, string values (containing only numbers), or both.
  \item{CO2}{Mass of carbon dioxide (CO2) emissions. Default mass is 0.}
  \item{CH4}{Mass of methane (CH4) emissions. Default mass is 0.}
  \item{CF4}{Mass of tetrafluoromethane (CF4) emissions. Default mass is 0.}
  \item{SF4}{Mass of sulfur tetrafluoride (SF4) emissions. Default mass is 0.}
  \item{N20}{Mass of nitrous oxide (N2O) emissions. Default mass is 0.}
  \item{CH3CHF2}{Mass of 1,1-difluoroethane (CH3CHF2) emissions. Default mass is 0.}
}
\value{
A numeric value (or a vector of numeric values) representing the CO2-equivalent emissions of the input greenhouse gases.
}
\examples{
gwp(100, 50, 10, 1, 5, 3)
gwp("100", "50", "10", "1", "5", "3")
gwp(CO2 = 30, CH4 = 20, CF4 = 15, SF4 = 8, N20 = 0.5, N20 = 0.001, CH3CHF2 = 0)
gwp(CH4 = 150)
gwp(CO2 = c(10, 20), CF4 = c(15, 35))
fuel_emissions(data_frame$CO2, data_frame$CH4)
}