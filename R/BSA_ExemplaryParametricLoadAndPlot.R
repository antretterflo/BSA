# BSA - WUFI Modelling - ExamplePlot --------------------------------------

# Function to load simulation results and exemplary plot
# Florian Antretter
# Date: 2018-04-23
# Version: 0.1


# Function to Load WUFI Plus Results Data ---------------------------------

f.GetWPData <- function(v.Path)
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



# Load Results ------------------------------------------------------------

# Load Required Packages --------------------------------------------------

require(stringr)
require(dplyr)
require(ggplot2)


# Read all Results Files in Folder ----------------------------------------

fileNames    <- list.files("Data/XML_Out/")
filesResults <- fileNames[grep("xml.Results_", fileNames)]


# Load Results Files and Combine to New Results Table ----------------------

## Define Empty Results Table and Column Names
#resultsTable <- data.frame() 
#resultsNames <- c("in.Wall.R.Value", "in.Roof.R.Value", "in.Slab.R.Value", "in.Window.U.Value", "in.Window.SHGC", "in.Heat.Recovery", "in.SW.Shade.Depth", "out.Heating.Demand", "out.Heating.Load", "out.Cooling.Demand", "out.Cooling.Load", "out.Primary.Energy", "out.Site.Energy")

## Run Loop for all Results Files
# This part is not very clean at the moment, as the output XML is not very clean
# It works for the current implementation but needs improvement after cleaning up of the results XML file
resultsDataAll <- tibble()

for(fileNameResults in filesResults) {
  
  resultsData    <- f.GetWPData(paste("Data/XML_Out/", fileNameResults, sep=""))
  resultsDataAll <- resultsData %>% mutate(Case = str_split(fileNameResults, ".xml.")[[1]][1]) %>% bind_rows(resultsDataAll)
}

PlotData <- select(resultsDataAll, c(1, 8, 9), Case)
names(PlotData) <- c("DateTime", "Tin", "RHin", "Case")
ggplot(PlotData, aes(x=DateTime, y=Tin)) +
  geom_line(aes(color=Case))

ggplot(PlotData, aes(x=Tin+RHin)) +
  stat_ecdf(aes(color=Case))
