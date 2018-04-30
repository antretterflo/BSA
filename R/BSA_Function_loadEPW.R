# BSA - WUFI Modelling - Load all climate data ----------------------------

# Controlling file to model indoor climate conditions with WUFI Plus
# Florian Antretter
# Date: 2018-04-30
# Version: 0.1


# Load required packages and source files ---------------------------------

library(readr)


# Function definitions ----------------------------------------------------

###########################################################################
# Function to load Energy Plus weather file header

loadEPWheader <- function(v.Path) {
  epwHeader <- read_delim(file = v.Path,
                          delim = ",", n_max = 1, col_names = FALSE)
  
  names(epwHeader) <- c("Location",
                        "City",
                        "State",
                        "Country",
                        "Source",
                        "WMO",
                        "Latitude",
                        "Longitude",
                        "Timezone",
                        "Eleation")
  
  return(epwHeader)
}

###########################################################################
# Function to load Energy Plus weather file data

loadEPWdata <- function(v.Path) {
  
  epwData <- read_delim(file = v.Path,
                        delim = ",", skip = 9)
  
  names(epwData) <- c("Year", "Month", "Day", "Hour", "Minute", 
                      "Flags", 
                      "TemperatureDryBulb", 
                      "TemperatureDewPoint", 
                      "RelativeHumidity", 
                      "Pressure", 
                      "RadiationExtraterrHorizontal", 
                      "RadiationExtraterrDirectNormal",
                      'RadiationInfraredHorizontal',
                      "RadiationGlobalHorizontal",
                      "RadiationDirectNormal",
                      "RadiationDiffuseHorizontal",
                      "IlluminanceGlobalHorizontal",
                      "IlluminanceDirectNormal",
                      "IlluminanceDiffuseHorizontal",
                      "LuminanceZenith",
                      "WindDirection",
                      "WindSpeed",
                      "TotalSkyCover",
                      "OpaqueSkyCover",
                      "Visibility",
                      "CeilingHeight",
                      "WeatherObs",
                      "WeatherCodes",
                      "PrecipitableWater",
                      "AerosolOpticalDepth",
                      "SnowDepth",
                      "DaysSinceLastSnow",
                      "Albedo",
                      "LiquidPrecipitationDepth",
                      "LiquidPrecipitationQuantity")
  
  return(epwData)
}

###########################################################################
# Function to load Energy Plus weather files

loadEPW <- function(v.Path) {
  epwHeader <- loadEPWheader(v.Path)
  epwData   <- loadEPWdata(v.Path)
  list(Header = epwHeader, Data = epwData)
}
