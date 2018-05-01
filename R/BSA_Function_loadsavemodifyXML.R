# BSA - WUFI Modelling - Load, Modify and Save XML ------------------------

# Read baseline XML model, modify XML structure, save XML project file
# Florian Antretter
# Date: 2018-05-01
# Version: 0.1


# Load required packages --------------------------------------------------

library(xml2)


# Read XML Input File -----------------------------------------------------

xmlLoadBaseCase  <- function(xmlWPInputPath) {
  read_xml(xmlWPInputPath)
}


# Write XML Project File --------------------------------------------------

xmlSaveFile <- function(filePath, xmlCase = xmlWUFI) {
  write_xml(xmlCase, file=filePath)
}


# Modify XML elements -----------------------------------------------------

# Modify the climate section of the XML file for external climate file
xmlModifyClimate <- function(filePath, xmlCase = xmlWUFI, fileHeader) {
  xml_set_text(xml_find_all(xmlCase, "/WUFIplusProject/Variants/Variant[1]/ClimateLocation/FileName"), as.character(filePath))
  xml_set_text(xml_find_all(xmlCase, "/WUFIplusProject/Variants/Variant[1]/ClimateLocation/Name"), as.character(fileHeader[["City"]]))
  xml_set_text(xml_find_all(xmlCase, "/WUFIplusProject/Variants/Variant[1]/ClimateLocation/Latitude"), as.character(fileHeader[["Latitude"]]))  
  xml_set_text(xml_find_all(xmlCase, "/WUFIplusProject/Variants/Variant[1]/ClimateLocation/Longitude"), as.character(fileHeader[["Longitude"]]))
  xml_set_text(xml_find_all(xmlCase, "/WUFIplusProject/Variants/Variant[1]/ClimateLocation/HeightNN"), as.character(fileHeader[["Elevation"]]))
  xml_set_text(xml_find_all(xmlCase, "/WUFIplusProject/Variants/Variant[1]/ClimateLocation/dUTC"), as.character(fileHeader[["Timezone"]]))  
}

# Modify the section with references to external data files
xmlModifyExtData <- function(filePath, fileNumber, xmlCase = xmlWUFI) {
  xml_set_text(xml_find_all(xmlCase, paste("/WUFIplusProject/ExternalFiles/ExternalFile[", fileNumber, "]/FileName", sep=""), as.character(filePath)))
}
