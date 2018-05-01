# BSA - WUFI Modelling - WUFI Plus Indoor Climate Modeling ----------------

# Controlling file to model indoor climate conditions with WUFI Plus
# Florian Antretter
# Date: 2018-04-30
# Version: 0.1


# Load required packages and source files ---------------------------------

suppressPackageStartupMessages(library(tidyverse))
library(tibble)
library(ggplot2)

source('R/BSA_Function_loadEPW.r')
source('R/BSA_Function_loadAllClimateData.r')
source('R/BSA_Function_calcAllInfiltration.R')
source('R/BSA_Function_generateBatch.R')
source('R/BSA_Function_loadsavemodifyXML.R')


# Path definitions for data files -----------------------------------------

pathDataFolder <- "C:/Users/fa0/ORNL/01_Projects/RProjects/BSA/Data"



# Generate probabilistics -------------------------------------------------

# Climate
climateData <- loadAllClimateData()

# Ventilation
climateData <- calcInfitrationData(maxInfData = 200)

# Set-points
calcSetPointConditions()

# Interior loads
calcInteriorLoads()


# Generate WUFI Plus input files ------------------------------------------

maxCaseNr   <- 100
xmlBaseCase <- "ExampleResidential_01.xml"

generateWUFIFiles()



for(i in 1:maxCaseNr) {
  
}


