
### Path to the folder AWS_DATA
AWS_DATA_DIR <- "/Users/rijaf/Desktop/ETHIOPIA2018/AWS_Data_Processing/AWS_DATA"

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

## Start date to extract in the format "YYYY-MM-DD"
start_time <- "2017-01-01"
## End date to extract in the format "YYYY-MM-DD"
end_time <- "2017-01-10"

##########
# Note
# precipitation at 2014-01-23 are computed using hourly data from 2014-01-23 09:00:00 to 2014-01-24 08:00:00

########################################## End Edit #############################################
cat("Extract daily data to CDT format .........\n")

coord.dir <- file.path(AWS_DATA_DIR, "coordinates_files")
coords <- read.table(file.path(coord.dir, paste0(AWS, ".csv")), header = TRUE, sep = ',', colClasses = 'character', stringsAsFactors = FALSE)
coords <- coords[, c(1, 3, 4)]
coords[, 2:3] <- apply(coords[, 2:3], 2, as.numeric)

###################

start_time <- as.Date(start_time)
end_time <- as.Date(end_time)
daty <- seq(start_time, end_time, "day")

###################

AWS_DIR1day <- file.path(AWS_DATA_DIR, paste0(AWS, "_AWS"), "compressed_data", "data_daily")
awslistx <- list.files(AWS_DIR1day, ".rds")
awslist <- gsub(".rds", "", awslistx)

data.aws <- lapply(awslist, function(aws){
	don <- readRDS(file.path(AWS_DIR1day, paste0(aws, ".rds")))
	don.daty <- if(variable == "RR") "date.RR" else "date.VAR"
	don.daty <- as.Date(don[[don.daty]], "%Y%m%d")
	xx <- switch(variable, 
				"RR" = don$RR[, 1],
				"TN" = don$TT[, 1],
				"TM" = don$TT[, 2],
				"TX" = don$TT[, 3])

	xx[match(daty, don.daty)]
})

data.aws <- do.call(cbind, data.aws)
ix <- match(awslist, coords[, 1])
crd <- as.matrix(coords[ix, ])
crd <- crd[!is.na(ix), ]
don <- data.aws[, !is.na(ix)]

xhead <- t(crd)
daty <- format(daty, "%Y%m%d")
capt <- c("AWS_ID", "LON", "DATE/LAT")
don <- rbind(cbind(capt, xhead), cbind(daty, don))
don[is.na(don)] <- -99

dirOUT <- file.path(AWS_DATA_DIR, "AWS_1day_CDT_Station_Format")
dir.create(dirOUT, showWarnings = FALSE, recursive = TRUE)
csvfile <- file.path(dirOUT, paste0("cdt_daily_", daty[1], "_to_", daty[length(daty)], ".csv"))
write.table(don, csvfile, sep = ",", col.names = FALSE, row.names = FALSE, quote = FALSE)

rm(don, data.aws); gc()
cat("Extracting daily data finished successfully\n")




