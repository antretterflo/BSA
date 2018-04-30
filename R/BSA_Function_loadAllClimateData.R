# BSA - WUFI Modelling - WUFI Plus Indoor Climate Modeling ----------------

# Function to load all climate data available in list
# Florian Antretter
# Date: 2018-04-30
# Version: 0.1


# Load all available climate data in list ---------------------------------

# the path where all the climate data sits "Data/In/Climate/4CITIES_EPW" 
# might need to be adjusted

loadAllClimateData <- function() {
  filesClimate   <- list.files("Data/In/Climate/4CITIES_EPW")
  allClimateData <- list()
  
  for(fileClimate in filesClimate) {
    # read Climate File
    #allClimateData[[fileClimate]][["Name"]]   <- fileClimate
    allClimateData[[fileClimate]][["Header"]] <- loadEPWheader(paste("Data/In/Climate/4CITIES_EPW/", fileClimate, sep=""))
    allClimateData[[fileClimate]][["Data"]]   <- loadEPWdata(paste("Data/In/Climate/4CITIES_EPW/", fileClimate, sep=""))
  }
  return(allClimateData)
}
