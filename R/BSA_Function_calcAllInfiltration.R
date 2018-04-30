# BSA - WUFI Modelling - WUFI Plus Indoor Climate Modeling ----------------

# Function to load all climate data available in list
# Florian Antretter
# Date: 2018-04-30
# Version: 0.1


# Function Definitons -----------------------------------------------------

###########################################################################
# calcInfiltrationEPW - Function to calc hourly infiltration rates based on
# Enhanced Model from ASHRAE Handbook of Fundamentals 2017
# Section 16.24

# generate model inputs data
genInfModelInputs <- function() {

  # Shelter Factor from Table 9
  ShelterFactorTi <- tibble(Value        = c(1.0,      0.9,      0.7,      0.5,      0.3,
                                             1.1,      1.02,     0.86,     0.7,      0.54,
                                             1.07,     0.98,     0.81,     0.64,     0.47,
                                             1.06,     0.97,     0.79,     0.61,     0.43),
                            ShelterClass = c(1,        2,        3,        4,        5,
                                             1,        2,        3,        4,        5,
                                             1,        2,        3,        4,        5,
                                             1,        2,        3,        4,        5),
                            Type         = c("noFlue", "noFlue", "noFlue", "noFlue", "noFlue",
                                             "oneS",   "oneS",   "oneS",   "oneS",   "oneS",
                                             "twoS",   "twoS",   "twoS",   "twoS",   "twoS",
                                             "threeS", "threeS", "threeS", "threeS", "threeS"))
  
  # Wind Coefficient Cw in (PA s2 / m2)^n from Table 8
  WindCoefficientTi <- tibble(Value    = c(0.156,    0.142,   0.170,    0.156,   0.170,    0.167,
                                           0.128,    0.128,   0.142,    0.142,   0.151,    0.154),
                              Type       = c("noFlue", "Flue",  "noFlue", "Flue",  "noFlue", "Flue",
                                             "noFlue", "Flue",  "noFlue", "Flue",  "noFlue", "Flue"),
                              Story      = c("oneS",   "oneS",  "twoS",   "twoS",  "threeS", "threeS",
                                             "oneS",   "oneS",  "twoS",   "twoS",  "threeS", "threeS"),
                              Foundation = c("Slab",   "Slab",  "Slab",   "Slab",  "Slab",   "Slab", 
                                             "Crawl",  "Crawl", "Crawl",  "Crawl", "Crawl",  "Crawl"))
  
  # Stack Coefficient Cs in (PA / K)^n from Table 8
  StackCoefficientTi <- tibble(Value = c(0.054,    0.069,  0.078,    0.089,  0.098,    0.107),
                               Type  = c("noFlue", "Flue", "noFlue", "Flue", "noFlue", "Flue"),
                               Story = c("oneS",   "oneS", "twoS",   "twoS", "threeS", "threeS"))
  
  # WindSpeed Multiplier from Table 7
  WindSpeedMultiplierTi <- tibble(Value = c(0.48, 0.59, 0.67),
                                  Story = c("oneS", "twoS", "threeS"))
  
  InfModelInputs <- list()
  InfModelInputs[["ShelterFactor"]]       <- ShelterFactorTi
  InfModelInputs[["WindCoefficient"]]     <- WindCoefficientTi
  InfModelInputs[["StackCoefficient"]]    <- StackCoefficientTi
  InfModelInputs[["WindSpeedMultiplier"]] <- WindSpeedMultiplierTi
  
  return(InfModelInputs)
}

# calculate infiltration data
calcInfiltrationEPW <- function(WeatherDataTi, #tibble with weather data
                                TemperatureIndoor = 21, 
                                TemperatureOutdoorCol = "TemperatureDryBulb", 
                                WindSpeedMeteoCol = "WindSpeed", 
                                FlowCoefficient, 
                                PressureExponent = 0.67, 
                                TypeI = "noFlue", 
                                StoryI = "oneS", 
                                ShelterClassI = 3, 
                                FoundationI = "Slab") {
  
  InfModelInputs        <- genInfModelInputs()
  ShelterFactorTi       <- InfModelInputs[["ShelterFactor"]]
  WindCoefficientTi     <- InfModelInputs[["WindCoefficient"]]
  StackCoefficientTi    <- InfModelInputs[["StackCoefficient"]]
  WindSpeedMultiplierTi <- InfModelInputs[["WindSpeedMultiplier"]] 
  
  # Select values from tables
  WindSpeedMeteo      <- WeatherDataTi[[WindSpeedMeteoCol]]
  TemperatureOutdoor  <- WeatherDataTi[[TemperatureOutdoorCol]]
  WindSpeedDesign     <- filter(WindSpeedMultiplierTi, Story == StoryI) %>% pull(Value) * WindSpeedMeteo
  StackCoefficient    <- StackCoefficientTi %>% filter(Type == TypeI & Story == StoryI) %>% pull(Value)
  WindCoefficient     <- WindCoefficientTi %>% filter(Type == TypeI & Story == StoryI & Foundation == FoundationI) %>% pull(Value)
  TypeShelterF        <- ifelse(TypeI == "noFlue", TypeI, StoryI)
  ShelterFactor       <- ShelterFactorTi %>% filter(Type == TypeShelterF & ShelterClass == ShelterClassI) %>% pull(Value)
  
  # Calculate stack and wind induced air flows
  AirFlowStack        <- FlowCoefficient * StackCoefficient * (TemperatureIndoor - TemperatureOutdoor) ^ PressureExponent
  AirFlowWind         <- FlowCoefficient * WindCoefficient * (ShelterFactor * WindSpeedDesign) ^ (2 * PressureExponent)
  
  # Superposition of both air flows
  AirFlowTotal        <- sqrt(AirFlowStack^2 + AirFlowWind^2)
  AirFlowTotal[is.nan(AirFlowTotal)] <- 0
  
  # Generate return tibble
  tibble(AirFlowTotal, AirFlowStack, AirFlowWind)
}

###########################################################################
# calcInfiltrationDist - Function to calc infiltration distributions from
# Input distributions from "Analysis of air leakage measurements of US houses"
# by Chan, Joh and Sherman (2013) in Energy and Buildings

calcInfiltrationDist <- function(maxInfValues = 10000) {
  
  countCases <- maxInfValues
  
  # from figure 3
  PressureExponentDistribution <- rnorm(n = countCases*2, mean = 0.646, sd = 0.057)
  PressureExponentDistribution <- PressureExponentDistribution[PressureExponentDistribution >= 0.5 & 
                                                               PressureExponentDistribution <= 1][1:countCases]
  
  # from figure 2
  NormalizedLeakage <- rlnorm(n = countCases*2, meanlog = log(0.61), sdlog = log(2.5))
  NormalizedLeakage <- NormalizedLeakage[NormalizedLeakage < 10][1:countCases]

  # Floor Area (number in between floor area in paper and stated from housing survey)
  # but with an IQR close to the stated one
  FloorArea <- rnorm(n = countCases*2, mean = 150, sd = 38)
  FloorArea <- FloorArea[FloorArea >= 50 & FloorArea <= 250][1:countCases]
  # IQR(FloorArea)
  
  # BuildingHeight according to simplified assumption stated in paper
  BuildingHeight <- ifelse(FloorArea < 200, 2.5, 2.5*2) * runif(n = countCases, min = 0.9, max = 1.1)
  
  # Q50 - based on eq(2) in paper, solved for Q50
  AirFlow50 <- (NormalizedLeakage * FloorArea) / (75 * (BuildingHeight / 2.5)^0.3)
  ACH50     <- (AirFlow50 * 3600) / (FloorArea * BuildingHeight)
  
  # Flow Coefficient
  FlowCoefficientDistribution <- AirFlow50 / (50^NormalizedLeakage)
  
  # Collection of all values in one tibble
  InfDistributions <- tibble(PressExp = PressureExponentDistribution,
                             FlowCoef = FlowCoefficientDistribution,
                             NormLeak = NormalizedLeakage,
                             AirF50   = AirFlow50,
                             ACH50    = ACH50)
  return(InfDistributions)
}



###########################################################################
# calcInfiltrationData - Function to add probabilistic infiltration data to
# climate data in climate data list

calcInfitrationData <- function(allClimateData = climateData, 
                                maxInfData = 200, 
                                maxInfValues = 10000) {
  
  filesClimate     <- names(climateData)
  InfDistributions <- calcInfiltrationDist(maxInfValues = maxInfValues)
  
  Types          <- c("Flue", "noFlue")
  Storys         <- c("oneS", "twoS", "threeS")
  ShelterClasses <- 1:5
  Foundations    <- c("Slab", "Crawl")
  
  for(fileClimate in filesClimate) {
    Infiltrations  <- list()
    thisWeatherData <- climateData[[fileClimate]][["Data"]]
    
    for(j in 1:maxInfData) {
      anyInf       <- floor(runif(1, 1, maxInfValues+1))
      thisFlowCoef <- InfDistributions[["FlowCoef"]][anyInf]
      thisPressExp <- InfDistributions[["PressExp"]][anyInf]
      thisType     <- sample(Types, 1)
      thisStory    <- sample(Storys, 1)
      thisShelt    <- sample(ShelterClasses, 1)
      thisFound    <- sample(Foundations, 1)
      
      caseName     <- paste("FC", round(thisFlowCoef, 3), "PE", round(thisPressExp, 2), 
                            thisType, thisStory, thisShelt, thisFound, sep="")
      
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
    
    allClimateData[[fileClimate]][["Infiltrations"]] <- bind_cols(Infiltrations)
  }
  
  return(allClimateData)
}
  
