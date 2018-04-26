# BSA - WUFI Modelling - Infiltration Rate Distributions ------------------

# Modelling distributions for infiltration rates
# Florian Antretter
# Date: 2018-04-26
# Version: 0.1


# Load Required Packages --------------------------------------------------

require(tibble)


# Generate input distributions --------------------------------------------

# Input distributions from "Analysis of air leakage measurements of US houses"
# by Chan, Joh and Sherman (2013) in Energy and Buildings
countCases <- 10000

# from figure 3
PressureExponentDistribution <- rnorm(n = countCases*2, mean = 0.646, sd = 0.057)
PressureExponentDistribution <- PressureExponentDistribution[PressureExponentDistribution >= 0.5 & PressureExponentDistribution <= 1][1:countCases]

# from figure 2
NormalizedLeakage <- rlnorm(n = countCases*2, meanlog = log(0.61), sdlog = log(2.5))
NormalizedLeakage <- NormalizedLeakage[NormalizedLeakage < 10][1:countCases]
#hist(NormalizedLeakage, breaks = seq(0,10,0.25))

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

# # 
# hist(FlowCoefficientDistribution)
# 
# uii <- data.frame(NormalizedLeakage, ACH50)
# abc <- subset(uii, uii$ACH50 > 9.5 & uii$ACH50 < 10.5)
# mean(abc$NormalizedLeakage)
# mean(abc$ACH50)
# plot(abc$NormalizedLeakage, abc$ACH50)

InfDistributions <- tibble(PressExp = PressureExponentDistribution,
                           FlowCoef = FlowCoefficientDistribution,
                           NormLeak = NormalizedLeakage,
                           AirF50   = AirFlow50,
                           ACH50    = ACH50)
