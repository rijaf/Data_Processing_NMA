
dir_dat <- switch(str_trim(TimeStep),
				"15min" = "data_15minQC",
				"1hr" = "data_1hr",
				"1day" = "data_daily",
				"10day" = "data_dekad")

format.date <- switch(str_trim(TimeStep),
				"15min" = "%Y%m%d%H%M%S",
				"1hr" = "%Y%m%d%H",
				"1day" = "%Y%m%d",
				"10day" = "%Y%m%d")

format.limit <- switch(str_trim(TimeStep),
				"15min" = "%Y%m%d%H%M",
				"1hr" = "%Y%m%d%H",
				"1day" = "%Y%m%d",
				"10day" = "%Y%m%d")

coord.dir <- file.path(AWS_DATA_DIR, "coordinates_files")
coords <- read.table(file.path(coord.dir, paste0(AWS, ".csv")), header = TRUE, sep = ',', colClasses = 'character', stringsAsFactors = FALSE)

STN <- file.path(AWS_DATA_DIR, paste0(AWS, "_AWS"), "compressed_data", dir_dat, paste0(coords$ID, ".rds"))
aws.exst <- file.exists(STN)
AWS.CRD <- coords[aws.exst, 1:4, drop = FALSE]
AWS.STN <- STN[aws.exst]
rm(coords, STN)

AWS.DAT <- lapply(AWS.STN, function(pth){
	don <- readRDS(pth)
	if(str_trim(variable) == "RR"){
		if(str_trim(TimeStep)%in%c("1day", "10day")){
			daty <- don$date.RR
			vals <- don$RR[, 1]
		}else{
			daty <- don$date
			if(str_trim(TimeStep) == "15min"){
				# vals <- if(AWS == "LSI") don$data$RR$Tot else don$data$RR
				vals <- don$data$RR
			}else vals <- don$RR[, 1]
		}
	}else{
		if(str_trim(TimeStep) == "15min"){
			# nom.var <- names(don$data)
			# nom.var <- nom.var[nom.var%in%c("TT", "TT2")]
			# if(length(nom.var) == 0) return(NULL)
			# if(length(nom.var) == 2) nom.var <- "TT"
			# daty <- don$date
			# if(AWS == "LSI"){
			# 	idx <- switch(str_trim(variable), "TN" = "Min", "TM" = "Ave", "TX" = "Max")
			# 	vals <- don$data[[nom.var]][[idx]]
			# }else{
			# 	if(str_trim(variable)%in%c("TN", "TX")) return(NULL)
			# 	vals <- don$data[[nom.var]]
			# }
			daty <- don$date
			vals <- don$data[['TT']]
		}else if(str_trim(TimeStep) == "10day"){
			nom.var <- names(don)
			if(!"TMP"%in%nom.var) return(NULL)
			daty <- don$date.TMP
			idx <- switch(str_trim(variable), "TN" = 1, "TM" = 2, "TX" = 3)
			vals <- don[["TMP"]][, idx]
		}else{
			# nom.var <- names(don)
			# nom.var <- nom.var[nom.var%in%c("TT", "TT2")]
			# if(length(nom.var) == 0) return(NULL)
			# if(length(nom.var) == 2) nom.var <- "TT"
			daty <- if(str_trim(TimeStep) == "1day") don$date.VAR else don$date
			vals <- don[["TT"]]
			idx <- switch(str_trim(variable), "TN" = 1, "TM" = 2, "TX" = 3)
			vals <- vals[, idx]
		}
	}

	daty <- strptime(daty, format.date, tz = "Africa/Addis_Ababa")
	daty <- if(str_trim(TimeStep) == "10day") paste0(format(daty, "%Y%m"), as.numeric(format(daty, "%d"))) else format(daty, format.limit)
	vals <- vals[daty == str_trim(date.to.use)]
	if(length(vals) == 0) vals <- NA
	return(vals)
})

inull <- sapply(AWS.DAT, is.null)
AWS.DAT <- AWS.DAT[!inull]
if(length(AWS.DAT) == 0) stop("No data available\n")
AWS.DAT <- do.call(c, AWS.DAT)
AWS.CRD <- AWS.CRD[!inull, ]
if(!any(!is.na(AWS.DAT))) stop("No data available\n")
AWS.DAT <- cbind(AWS.CRD, values = AWS.DAT)

