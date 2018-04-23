# BSA - WUFI Modelling - Load Project XML ---------------------------------

# Exemplary parametric simulation set-up
# ...
# Florian Antretter
# 2018-04-23



# Load required packages --------------------------------------------------

library(xml2)
library(stringr)
library(dplyr)
library(ggplot2)


# Generate Load Files -----------------------------------------------------

# Number of different values from normal distribution
NrOfValues <- 100000

# Generate values from normal distribution per input
# internal loads
hconv <- rnorm(NrOfValues, 300, 100)
hrad  <- rnorm(NrOfValues, 150, 50)
moist <- rnorm(NrOfValues, 100, 50)
co2   <- rnorm(NrOfValues, 80, 20)
met   <- rnorm(NrOfValues, 1.2, 0.4)
met[met < 0.8] <- 0.8
clo   <- rnorm(NrOfValues, 0.75, 0.25)
clo[clo < 0.4] <- 0.4
airv  <- rnorm(NrOfValues, 0.1, 0.04)
airv[airv < 0.001] <- 0.001

# ventilation
vent           <- rnorm(NrOfValues, 125, 50)
vent[vent < 0] <- 0

# temperature set-points
setTmin <- rnorm(NrOfValues, 20, 2)
setTmin[setTmin > 22.5] <- 22.5
setTmax <- rnorm(NrOfValues, 25, 2)
setTmax[setTmax < 22.5] <- 22.5

# generate and write tables with internal loads, set-points and vent rates
NrOfInputFiles    <- 100
BoundCondFilePath <- "Data/XML_In/BoundaryConditionFiles/Variants1/"

for(i in 1:NrOfInputFiles) {
  InnerLoads <- tibble(sample(hconv, 8760), 
                      sample(hrad, 8760), 
                      sample(moist, 8760), 
                      sample(co2, 8760), 
                      sample(met, 8760), 
                      sample(clo, 8760), 
                      sample(airv, 8760))
  write_delim(x = InnerLoads, 
              path = paste(BoundCondFilePath, "InnerLoads_", i, ".txt", sep=""),
              col_names = F, delim = " ")
  
  SetTemps <- tibble(sample(setTmin, 8760), sample(setTmax, 8760))
  write_delim(x = SetTemps, 
              path = paste(BoundCondFilePath, "Temperatures_", i, ".txt", sep=""),
              col_names = F, delim = " ")
  
  SetVent  <- tibble(sample(vent, 8760), 0)
  write_delim(x = SetVent, 
              path = paste(BoundCondFilePath, "Ventilations_", i, ".txt", sep=""),
              col_names = F, delim = " ")
}


# Modify and save WUFI Plus input XML files -------------------------------

ExampleFilePath <- "Data/XML_In/"
xmlFileName   <- paste(ExampleFilePath, "ExampleResidential_01.xml", sep="")
xmlWUFI       <- read_xml(xmlFileName)

fullFilePath   <- "C:\\Users\\fa0\\ORNL\\01_Projects\\RProjects\\BSA\\Data\\XML_In\\BoundaryConditionFiles\\Variants1\\"
folderOutput   <- "Data/XML_Out/"
InnerLoadFiles <- c()
TempFiles      <- c()
VentFiles      <- c()

# Generate strings with path to file
for(i in 1:100) {
  InnerLoadFiles[i] <- paste(fullFilePath, "InnerLoads_", i, ".txt", sep="")
  TempFiles[i]      <- paste(fullFilePath, "Temperatures_", i, ".txt", sep="")
  VentFiles[i]      <- paste(fullFilePath, "Ventilations_", i, ".txt", sep="")
}

# Modify base xml and save new XML
for(j in 1:20) {
  RandIntL <- round(runif(1)*100, 0)
  RandVent <- round(runif(1)*100, 0)
  RandTemp <- round(runif(1)*100, 0)
  xml_set_text(xml_find_all(xmlWUFI, "/WUFIplusProject/ExternalFiles/ExternalFile[1]/FileName"), as.character(InnerLoadFiles[RandIntL]))
  xml_set_text(xml_find_all(xmlWUFI, "/WUFIplusProject/ExternalFiles/ExternalFile[2]/FileName"), as.character(VentFiles[RandVent]))
  xml_set_text(xml_find_all(xmlWUFI, "/WUFIplusProject/ExternalFiles/ExternalFile[3]/FileName"), as.character(TempFiles[RandTemp]))
  
  write_xml(xmlWUFI, file=paste(paste(folderOutput, "Int", RandIntL, "_Vent", RandVent, "_Temp", RandTemp, ".xml", sep="")))
}


# Read files in output folder and generate batch files to run WUFI --------

## Read Files in Output Folder
fileNames    <- list.files(folderOutput)
fileUnlist   <- as.character(unlist(fileNames))

## Installation Path of WUFI Plus
pathWUFIPlus <- "C:\\Program Files (x86)\\WUFI\\masterWUFI\\WUFIplus\\WUFIplus.exe"
pathWUFIRC   <- c(pathWUFIPlus, "C", "R")

## Generate Batch File with Simulations per CPU
countCPUs    <- 4
perCPUsims   <- (length(fileNames) / countCPUs)

numberSimToCPU <- cbind(fileUnlist, 1:countCPUs)

for(i in 1:countCPUs) {
  batchString  <- t(c(pathWUFIRC, numberSimToCPU[,1][as.numeric(numberSimToCPU[,2]) == i]))
  write.table(batchString, file=paste(folderOutput, "RunBatch", i,".bat", sep=""), quote=TRUE, append=FALSE, row.names=FALSE, col.names=FALSE)
}
