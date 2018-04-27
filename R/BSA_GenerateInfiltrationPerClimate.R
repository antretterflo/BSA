# BSA - WUFI Modelling - Generate Infiltration Parametrics ----------------

# Generate parametric input with varying infiltration
# Florian Antretter
# Date: 2018-04-26
# Version: 0.1


# Load Required Packages --------------------------------------------------

library(tibble)
library(ggplot2)
suppressPackageStartupMessages(library(tidyverse))

source('R/BSA_GenerateInfiltrationParameterDistributions.r')
source('R/BSA_Function_CalcInfiltrationEPW.r')

# Generate climate dependent infiltration rate ----------------------------

filesClimate   <- list.files("Data/In/Climate/4CITIES_EPW")
Types          <- c("Flue", "noFlue")
Storys         <- c("oneS", "twoS", "threeS")
ShelterClasses <- 1:5
Foundations    <- c("Slab", "Crawl")

#hist(round(runif(n = 100000, min = 1, max = length(filesClimate)), 0))
countInfiltrations <- 200

allClimateData     <- list()
allInfiltrations   <- list()

for(fileClimate in filesClimate) {
  
  # read Climate File
  thisWeatherData <- loadEPWdata(paste("Data/In/Climate/4CITIES_EPW/", fileClimate, sep=""))[[2]] %>% 
    as.tibble() %>% 
    mutate(File = fileClimate)
  
  allClimateData[[fileClimate]] <- thisWeatherData
  
  Infiltrations <- list()
  
  for(j in 1:countInfiltrations) {
    anyInf       <- floor(runif(1, 1, countCases+1))
    thisFlowCoef <- InfDistributions[["FlowCoef"]][anyInf]
    thisPressExp <- InfDistributions[["PressExp"]][anyInf]
    thisType     <- sample(Types, 1)
    thisStory    <- sample(Storys, 1)
    thisShelt    <- sample(ShelterClasses, 1)
    thisFound    <- sample(Foundations, 1)
    
    caseName     <- paste("FC", round(thisFlowCoef, 3), "PE", round(thisPressExp, 2), thisType, thisStory, thisShelt, thisFound, sep="")
    
    Infiltration <- thisWeatherData %>% calcInfiltrationEPW(., TemperatureIndoor = 21,
                                                        FlowCoefficient = thisFlowCoef,
                                                        PressureExponent = thisPressExp,
                                                        TypeI = thisType,
                                                        StoryI = thisStory,
                                                        ShelterClassI = thisShelt,
                                                        FoundationI = thisFound)  
    Infiltrations[[caseName]] <- Infiltration[["AirFlowTotal"]]
    #names(Infiltrations[[j]]) <- paste("FC", thisFlowCoef, "PE", thisPressExp, thisType, thisStory, thisShelt, thisFound, sep="")
  }
  
  allInfiltrations[[fileClimate]] <- bind_cols(Infiltrations)
}

plot(ecdf(allInfiltrations[[6]][[1]]), xlim=c(0,0.35))
for(i in 1:190) {
  lines(ecdf(allInfiltrations[[6]][[i]]), col=rainbow(190)[i])
}
# climateData <- loadEPWdata(paste("Data/In/Climate/4CITIES_EPW/", filesClimate[3], sep=""))[[2]]
# plot(ecdf(climateData$LiquidPrecipitationDepth[climateData$LiquidPrecipitationDepth > 0]))
# 
# hist(climateData$LiquidPrecipitationDepth[climateData$LiquidPrecipitationDepth > 0])
# plot(climateData$LiquidPrecipitationDepth)
# plot(cumsum(climateData$LiquidPrecipitationDepth), type='l')
# 
# ggplot(allClimateData, aes(x=File, y=LiquidPrecipitationDepth)) +
#   geom_boxplot(aes(color = File))
