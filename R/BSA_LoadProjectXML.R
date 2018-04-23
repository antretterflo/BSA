
# BSA - WUFI Modelling - Load Project XML ---------------------------------

# Load the WUFI Plus XML Project
# ...
# Florian Antretter
# 2018-04-23



# Load required packages --------------------------------------------------

library(xml2)


# Read XML Input File -----------------------------------------------------

xmlFileName   <- "Data/XML_In/BoundaryConditionFiles/InnerLoads/ExampleResidential.xml"
xmlWUFI       <- read_xml(xmlFileName)
