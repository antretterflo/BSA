# BSA - WUFI Modelling - Generate Batch Files -----------------------------

# Read all parametric XML files and generate batch files to run all of them
# Florian Antretter
# Date: 2018-05-01
# Version: 0.1


# Load required packages --------------------------------------------------



# Read files in output folder and generate batch files to run WUFI --------

generateBatch <- function(folderPath,
                          countCPUs = 4,
                          pathWUFIPlus = "C:\\Program Files (x86)\\WUFI\\masterWUFI\\WUFIplus\\WUFIplus.exe") {
  
  ## Read Files in Output Folder
  fileNames    <- list.files(folderPath)
  fileUnlist   <- as.character(unlist(fileNames))
  
  ## Installation Path of WUFI Plus
  # pathWUFIPlus <- "C:\\Program Files (x86)\\WUFI\\masterWUFI\\WUFIplus\\WUFIplus.exe"
  pathWUFIRC   <- c(pathWUFIPlus, "C", "R")
  
  ## Generate Batch File with Simulations per CPU
  # countCPUs    <- 4
  perCPUsims   <- (length(fileNames) / countCPUs)
  
  numberSimToCPU <- cbind(fileUnlist, 1:countCPUs)
  
  for(i in 1:countCPUs) {
    batchString  <- t(c(pathWUFIRC, numberSimToCPU[,1][as.numeric(numberSimToCPU[,2]) == i]))
    write.table(batchString, file=paste(folderPath, "RunBatch", i,".bat", sep=""), quote=TRUE, append=FALSE, row.names=FALSE, col.names=FALSE)
  }
}
