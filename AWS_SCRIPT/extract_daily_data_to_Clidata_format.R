
### Path to the folder AWS_DATA
AWS_DATA_DIR <- "/Users/rijaf/Desktop/ETHIOPIA2018/AWS_Data_Processing/AWS_DATA"

### AWS system
## "ADCON" or VAISALA
AWS <- "ADCON"

##############

## Start date to extract in the format "YYYY-MM-DD"
start_time <- "2011-01-01"
## End date to extract in the format "YYYY-MM-DD"
end_time <- "2017-12-31"

##########
# Note
# precipitation at 2014-01-23 are computed using hourly data from 2014-01-23 09:00:00 to 2014-01-24 08:00:00

########################################## End Edit #############################################
library(reshape2)
library(stringr)

cat("Extract daily data to Clidata export format .........\n")

# coord.dir <- file.path(AWS_DATA_DIR, "coordinates_files")
# coords <- read.table(file.path(coord.dir, paste0(AWS, ".csv")), header = TRUE, sep = ',', colClasses = 'character', stringsAsFactors = FALSE)
# coords <- coords[, c(1, 3, 4)]
# coords[, 2:3] <- apply(coords[, 2:3], 2, as.numeric)

###################

start_time <- as.Date(start_time)
end_time <- as.Date(end_time)
daty <- seq(start_time, end_time, "day")

yr <- format(daty, "%Y")
mo <- format(daty, "%m")
dy <- format(daty, "%d")

###################

AWS_DIRinfo <- file.path(AWS_DATA_DIR, paste0(AWS, "_AWS"), "compressed_data", "infos")
AWS_DIR1day <- file.path(AWS_DATA_DIR, paste0(AWS, "_AWS"), "compressed_data", "data_daily")
awslist1d <- list.files(AWS_DIR1day, ".rds")
awslist.d <- gsub(".rds", "", awslist1d)

# AWS_DIR1hour <- file.path(AWS_DATA_DIR, paste0(AWS, "_AWS"), "compressed_data", "data_1hr")
# awslist1h <- list.files(AWS_DIR1hour, ".rds")
# awslist.h <- gsub(".rds", "", awslist1h)

data.aws <- lapply(awslist.d, function(aws){
	info <- readRDS(file.path(AWS_DIRinfo, paste0(aws, ".rds")))
	don <- readRDS(file.path(AWS_DIR1day, paste0(aws, ".rds")))

	daty.RR <- as.Date(don[["date.RR"]], "%Y%m%d")
	RR <- don$RR[match(daty, daty.RR), 1]
	RR <- data.frame(yr = yr, mo = mo, dy = dy, rr = RR)
	RR <- reshape2::acast(RR, dy ~ mo ~ yr, value.var = "rr", drop = FALSE)
	dymoyr <- dimnames(RR)
	RR <- lapply(seq_along(dymoyr[[3]]), function(j){
		yyr <- dymoyr[[3]][j]
		mon <- dymoyr[[2]]
		icol <- as.numeric(dymoyr[[1]])
		val <- matrix(NA, length(mon), 31)
		val[, icol] <- t(RR[, , j])
		ina <- apply(val, 1, function(x) all(is.na(x)))
		val <- cbind(info$name, info$id, "PRECIP", yyr, mon, "9:00", val)
		val <- val[!ina, , drop = FALSE]
		val[is.na(val)] <- "na"
		return(val)
	})
	RR <- do.call(rbind, RR)

	daty.VAR <- as.Date(don[["date.VAR"]], "%Y%m%d")
	TN <- don$TT[match(daty, daty.VAR), 1]
	TX <- don$TT[match(daty, daty.VAR), 3]

	TN <- data.frame(yr = yr, mo = mo, dy = dy, tn = TN)
	TN <- reshape2::acast(TN, dy ~ mo ~ yr, value.var = "tn", drop = FALSE)
	TX <- data.frame(yr = yr, mo = mo, dy = dy, tx = TX)
	TX <- reshape2::acast(TX, dy ~ mo ~ yr, value.var = "tx", drop = FALSE)

	dymoyr <- dimnames(TN)
	TN <- lapply(seq_along(dymoyr[[3]]), function(j){
		yyr <- dymoyr[[3]][j]
		mon <- dymoyr[[2]]
		icol <- as.numeric(dymoyr[[1]])
		val <- matrix(NA, length(mon), 31)
		val[, icol] <- t(TN[, , j])
		ina <- apply(val, 1, function(x) all(is.na(x)))
		val <- cbind(info$name, info$id, "TMPMIN", yyr, mon, "9:00", val)
		val <- val[!ina, , drop = FALSE]
		val[is.na(val)] <- "na"
		return(val)
	})
	TN <- do.call(rbind, TN)

	TX <- lapply(seq_along(dymoyr[[3]]), function(j){
		yyr <- dymoyr[[3]][j]
		mon <- dymoyr[[2]]
		icol <- as.numeric(dymoyr[[1]])
		val <- matrix(NA, length(mon), 31)
		val[, icol] <- t(TX[, , j])
		ina <- apply(val, 1, function(x) all(is.na(x)))
		val <- cbind(info$name, info$id, "TMPMAX", yyr, mon, "18:00", val)
		val <- val[!ina, , drop = FALSE]
		val[is.na(val)] <- "na"
		return(val)
	})
	TX <- do.call(rbind, TX)

	don <- rbind(RR, TN, TX)
	a=don[order(don[, 4], don[, 5]), ]
})
data.aws <- do.call(rbind, data.aws)
xhead <- c("STN_Name", "EG_GH_ID", "EG_EL", "YEAR", "MONTH", "TIME", paste0("Val", str_pad(1:31, 2, pad = "0")))
data.aws <- rbind(xhead, data.aws)

dirOUT <- file.path(AWS_DATA_DIR, "AWS_1day_CLIDATA_Format")
dir.create(dirOUT, showWarnings = FALSE, recursive = TRUE)
csvfile <- file.path(dirOUT, paste0("clidata_daily_", daty[1], "_to_", daty[length(daty)], ".csv"))
write.table(data.aws, csvfile, sep = ",", col.names = FALSE, row.names = FALSE, quote = FALSE)

rm(data.aws); gc()
cat("Extracting daily data finished successfully\n")




