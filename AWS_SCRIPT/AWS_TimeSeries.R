
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

start.date <- strptime(str_trim(date.start), format.limit, tz = "Africa/Addis_Ababa")
end.date <- strptime(str_trim(date.end), format.limit, tz = "Africa/Addis_Ababa")

if(str_trim(TimeStep) == "10day"){
	daty0 <- seq(start.date, end.date, "day")
	dek <- as.numeric(format(daty0, "%d"))
	daty0 <- paste0(format(daty0, "%Y%m")[dek <= 3], dek[dek <= 3])
}else{
	pas <- switch(str_trim(TimeStep),
					"15min" = "15 min",
					"1hr" = "hour",
					"1day" = "day")
	daty0 <- seq(start.date, end.date, pas)
	daty0 <- format(daty0, format.limit)
}

# donnees <- lapply(c("ADCON", "VAISALA"), function(AWS){
	
	aws <- aws.id
	# if(length(aws) == 0) return(NULL)
	if(length(aws) == 0) stop("No data available\n")
	aws_pth <- file.path(AWS_DATA_DIR, paste0(AWS, "_AWS"), "compressed_data", dir_dat, paste0(aws, ".rds"))
	ff.exst <- file.exists(aws_pth)
	id.aws <- as.character(aws)[ff.exst]
	aws_pth <- aws_pth[ff.exst]
	# if(length(aws_pth) == 0) return(NULL)
	if(length(aws_pth) == 0) stop("No data available\n")

	STN <- lapply(aws_pth, function(pth){
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
				# 	idx <- switch(str_trim(plt_pars$var), "TN" = "Min", "TM" = "Ave", "TX" = "Max")
				# 	vals <- don$data[[nom.var]][[idx]]
				# }else{
				# 	if(str_trim(plt_pars$var)%in%c("TN", "TX")) return(NULL)
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
		idt <- daty >= start.date & daty <= end.date
		daty <- daty[idt]
		vals <- vals[idt]
		if(length(daty) == 0) return(NULL)
		if(str_trim(TimeStep) == "10day"){
			dek <- as.numeric(format(daty, "%d"))
			daty <- paste0(format(daty, "%Y%m"), dek)
		}else daty <- format(daty, format.limit)
		ix <- match(daty0, daty)
		vals <- vals[ix]
		return(vals)
	})

	inull <- sapply(STN, is.null)
	STN <- STN[!inull]
	# if(length(STN) == 0) return(NULL)
	if(length(STN) == 0) stop("No data available\n")
	id.aws <- id.aws[!inull]
	STN <- do.call(cbind, STN)
	# return(list(ID = id.aws, data = STN))
# })

coord.dir <- file.path(AWS_DATA_DIR, "coordinates_files")
coords <- read.table(file.path(coord.dir, paste0(AWS, ".csv")), header = TRUE, sep = ',', colClasses = 'character', stringsAsFactors = FALSE)
istn <- match(id.aws, coords$ID)
STN.NAME <- coords$Name[istn]

ID.AWS <- id.aws
DATA.AWS <- STN

# ID.AWS <- NULL
# DATA.AWS <- NULL
# STN.NAME <- NULL
# if(!is.null(donnees[[1]])){
# 	ID.AWS <- donnees[[1]]$ID
# 	DATA.AWS <- donnees[[1]]$data
# 	coords.LSI <- read.table(file.path(coord.dir, "LSI.csv"), header = TRUE, sep = ',', colClasses = 'character', stringsAsFactors = FALSE)
# 	istn <- match(donnees[[1]]$ID, coords.LSI$id)
# 	STN.NAME <- coords.LSI$station_name[istn]
# }
# if(!is.null(donnees[[2]])){
# 	ID.AWS <- c(ID.AWS, donnees[[2]]$ID)
# 	DATA.AWS <- cbind(DATA.AWS, donnees[[2]]$data)
# 	coords.REMA <- read.table(file.path(coord.dir, "REMA.csv"), header = TRUE, sep = ',', colClasses = 'character', stringsAsFactors = FALSE)
# 	istn <- match(donnees[[2]]$ID, coords.REMA$id)
# 	STN.NAME <- c(STN.NAME, coords.REMA$station_name[istn])
# }
# rm(donnees)
# if(is.null(ID.AWS)) stop("No data available\n")

