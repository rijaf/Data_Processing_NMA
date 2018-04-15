
### Path to the folder AWS_DATA
AWS_DATA_DIR <- "/Users/rijaf/Desktop/ETHIOPIA2018/AWS_Data_Processing/AWS_DATA"
### Path to the folder AWS_SCRIPT
AWS_SCRIPT_DIR <- "/Users/rijaf/Desktop/ETHIOPIA2018/AWS_Data_Processing/AWS_SCRIPT"

### AWS system
## "ADCON" or VAISALA
AWS <- "ADCON"

##############
## AWS to extract
# List of station to extract
aws.id <- c("ARKOFE15", "SHADDI15", "ILGORE15", "WOWEGE15")

##############
## variable to extract
# Rainfall: "RR"
# Min Temperature: "TN"
# Mean Temperature: "TM"
# Max Temperature: "TX"

variable <- "TX"

### Time step
# Time scale to extract
# 15 minutes data: "15min"
# hourly data: "1hr"
# daily data: "1day"
# dekadal data: "10day"

TimeStep <- "1hr"

### Date range to extract
# for 15 minute  "YYYYMMDDHHMM" ; minutes: 00, 15, 30, 45
# for hourly "YYYYMMDDHH"
# for daily "YYYYMMDD"
# for dekad "YYYYMMD" ; dekad: 1, 2, 3

date.start <- "2017010100"
date.end <- "2017013023"

### File to save the table in CSV format
table.csv <- "/Users/rijaf/Desktop/Test.csv"

########################################## End Edit #############################################
library(stringr)

source(file.path(AWS_SCRIPT_DIR, "AWS_TimeSeries.R"))

don <- rbind(c("DATE", STN.NAME), cbind(daty0, DATA.AWS))
don[is.na(don)] <- ""
write.table(don, table.csv, sep = ",", col.names = FALSE, row.names = FALSE, quote = FALSE)

cat('Extraction finished\n')

