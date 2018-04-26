# BSA - Load Climate Files and Simulation Results Functions ---------------

# Functions to load climate files and simulation results from different sources
# Florian Antretter
# Date: 2018-04-26
# Version: 0.2


# Function to Load WUFI Plus Results Data ---------------------------------

loadWPData <- function(v.Path)
{
  require(readr)
  require(lubridate)
  Count.Col            <- as.numeric(strsplit(as.character(read.table(v.Path, nrows=1, skip=4)[[4]]), ":"))
  Col.Names            <- as.character((read.table(v.Path, nrows=Count.Col, skip=5, sep="\t"))[,1])
  
  Dist.Col             <- c(24, rep(14,Count.Col-1))
  
  de_locale            <- locale(date_names = "de", date_format = "%d.%m.%Y %H:%M:%S", time_format = "%H:%M:%S",
                                 decimal_mark = ",", grouping_mark = "", tz = "UTC", encoding = "UTF-8", asciify = FALSE)
  
  myLocale             <- locale(date_names = "en", date_format = "%m/%d/%Y", time_format = "%H", 
                                 decimal_mark = ".", tz = "UTC", encoding = "UTF-8", asciify = FALSE)
  
  v.WufiData           <- read_fwf(file=v.Path, col_positions = fwf_widths(Dist.Col, col_names = Col.Names), 
                                   skip=6+Count.Col, col_types = paste(c("c", "n", rep("d", Count.Col-2)), sep="", collapse=""), locale=myLocale)
  
  v.WufiData[[1]]      <- mdy_h(v.WufiData[[1]])
  return(v.WufiData)
}

# Function to load Energy Plus Weather Data

loadEPWdata <- function(v.Path) {
  
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
  
  epwData <- read_delim(file = v.Path,
                        delim = ",", skip = 9)
  
  names(epwData) <- c("Year", "Month", "Day", "Hour", "Minute", 
                "Flags", 
                "TemperatureDryBulb", 
                "TemperatureDewPoint", 
                "RH", 
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
  
  list(Header = epwHeader, Data = epwData)
}