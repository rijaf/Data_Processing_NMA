
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

## Start date to extract in the format "YYYY-MM-D"
## Dekad must be 1, 2 or 3
start_time <- "2016-01-1"
## End date to extract in the format "YYYY-MM-D"
end_time <- "2017-12-3"

########################################## End Edit #############################################
cat("Extract dekadal data to CDT format .........\n")

coord.dir <- file.path(AWS_DATA_DIR, "coordinates_files")
coords <- read.table(file.path(coord.dir, paste0(AWS, ".csv")), header = TRUE, sep = ',', colClasses = 'character', stringsAsFactors = FALSE)
coords <- coords[, c(1, 3, 4)]
coords[, 2:3] <- apply(coords[, 2:3], 2, as.numeric)

###################

start_time <- as.Date(start_time)
end_time <- as.Date(end_time)
daty <- seq(start_time, end_time, "day")
daty <- daty[as.numeric(format(daty, "%d")) <= 3]

###################

AWS_DIRdek <- file.path(AWS_DATA_DIR, paste0(AWS, "_AWS"), "compressed_data", "data_dekad")
awslistx <- list.files(AWS_DIRdek, ".rds")
awslist <- gsub(".rds", "", awslistx)

data.aws <- lapply(awslist, function(aws){
	don <- readRDS(file.path(AWS_DIRdek, paste0(aws, ".rds")))
	don.daty <- if(variable == "RR") "date.RR" else "date.TMP"
	don.daty <- as.Date(don[[don.daty]], "%Y%m%d")
	xx <- switch(variable, 
				"RR" = don$RR[, 1],
				"TN" = don$TMP[, 1],
				"TM" = don$TMP[, 2],
				"TX" = don$TMP[, 3])

	xx[match(daty, don.daty)]
})

data.aws <- do.call(cbind, data.aws)
ix <- match(awslist, coords[, 1])
crd <- as.matrix(coords[ix, ])
crd <- crd[!is.na(ix), ]
don <- data.aws[, !is.na(ix)]

xhead <- t(crd)
daty <- paste0(format(daty, "%Y%m"), as.numeric(format(daty, "%d")))
capt <- c("AWS_ID", "LON", "DATE/LAT")
don <- rbind(cbind(capt, xhead), cbind(daty, don))
don[is.na(don)] <- -99

dirOUT <- file.path(AWS_DATA_DIR, "AWS_10days_CDT_Station_Format")
dir.create(dirOUT, showWarnings = FALSE, recursive = TRUE)
csvfile <- file.path(dirOUT, paste0("cdt_dekadal_", daty[1], "_to_", daty[length(daty)], ".csv"))
write.table(don, csvfile, sep = ",", col.names = FALSE, row.names = FALSE, quote = FALSE)

rm(don, data.aws); gc()
cat("Extracting dekadal data finished successfully\n")




