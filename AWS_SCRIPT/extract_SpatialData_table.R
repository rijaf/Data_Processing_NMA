
### Path to the folder AWS_DATA
AWS_DATA_DIR <- "/Users/rijaf/Desktop/ETHIOPIA2018/AWS_Data_Processing/AWS_DATA"
### Path to the folder AWS_SCRIPT
AWS_SCRIPT_DIR <- "/Users/rijaf/Desktop/ETHIOPIA2018/AWS_Data_Processing/AWS_SCRIPT"

### AWS system
## "ADCON" or VAISALA
AWS <- "ADCON"

##############
## variable to extract
# Rainfall: "RR"
# Min Temperature: "TN"
# Mean Temperature: "TM"
# Max Temperature: "TX"

variable <- "RR"

### Time step
# Time scale to extract
# 15 minutes data: "15min"
# hourly data: "1hr"
# daily data: "1day"
# dekadal data: "10day"

TimeStep <- "10day"

### Date to extract
# for 15 minute  "YYYYMMDDHHMM" ; minutes: 00, 15, 30, 45
# for hourly "YYYYMMDDHH"
# for daily "YYYYMMDD"
# for dekad "YYYYMMD" ; dekad: 1, 2, 3

date.to.extract <- "2017071"

### File to save the output table in CSV format
table.csv <- "/Users/rijaf/Desktop/Test.csv"

########################################## End Edit #############################################

library(stringr)

date.to.use <- date.to.extract
source(file.path(AWS_SCRIPT_DIR, "AWS_SpatialData.R"))

AWS.DAT[is.na(AWS.DAT)] <- ""
write.table(AWS.DAT, table.csv, sep = ",", col.names = TRUE, row.names = FALSE, quote = FALSE)
cat("Extraction finished successfully\n")

