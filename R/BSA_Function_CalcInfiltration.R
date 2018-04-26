# BSA - WUFI Modelling - Functions Infiltration Rate ----------------------

# Function to compute hourly infiltration rates with LBL Enhanced Model
# Florian Antretter
# Date: 2018-04-25
# Version: 0.1


# Load Required Packages --------------------------------------------------

require(tibble)


# LBL Enhanced Model ------------------------------------------------------

# Enhanced Model from ASHRAE Handbook of Fundamentals 2017
# Section 16.24

# WindSpeedMeteo     <- getFromClimate(Windspeed)
# Temperatureoutdoor <- getFromClimate(Temperature)

calcInfiltration <- function(TemperatureIndoor = 21, 
                             TemperatureOutdoor, 
                             WindSpeedMeteo, 
                             FlowCoefficient, 
                             PressureExponent = 0.67, 
                             TypeI = "noFlue", 
                             StoryI = "oneS", 
                             ShelterClassI = 3, 
                             FoundationI = "Slab") {
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
  
  # Select values from tables
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
  
  # Generate return tibble
  tibble(AirFlowStack, AirFlowWind, AirFlowTotal)
}




