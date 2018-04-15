
getADCONAWS.STN <- function(aws, OUTDIR, ftp){
	tmpdir <- file.path(OUTDIR, "tmp")
	if(!dir.exists(tmpdir)) dir.create(tmpdir, showWarnings = FALSE, recursive = TRUE)
	tmpfile <- file.path(tmpdir, aws)
	on.exit(unlink(tmpfile))
	file0 <- paste0(ftp$AWS$ADCON$ftp, aws)
	ret <- try(getFTPData(file0, tmpfile, userpwd = ftp$AWS$ADCON$userpwd), silent = TRUE)

	if(inherits(ret, "try-error")) return(NULL)
	if(ret != 0) return(NULL)

	hhd <- try(read.table(tmpfile, nrows = 1, sep = ",", colClasses = "character", stringsAsFactors = FALSE), silent = TRUE)
	if(inherits(hhd, "try-error")) return(NULL)
	nm.head <- str_trim(as.character(hhd[1, ]))
	len.head <- length(nm.head)

	don <- read.table(tmpfile, skip = 1, nrows = 2, sep = ",", colClasses = "character", stringsAsFactors = FALSE, row.names = NULL)
	len.data <- length(names(don))
	# stn.name <- don[2, 1]
	# stn.id <- don[2, 2]
	if(!(len.head == len.data & len.head != 9)) return(NULL)
	# fileinfo <- file.path(OUTDIR, "ADCON_AWS", "compressed_data", "infos", paste0(stn.id, ".rds"))
	# if(!file.exists(fileinfo)){
	# 	info <- NULL
	# 	info$id <- stn.id
	# 	info$name <- stn.name
	# 	info$start <- NULL
	# 	info$end <- NULL
	# }
	don <- read.table(tmpfile, skip = 1, sep = ",", colClasses = "character",
						stringsAsFactors = FALSE, row.names = NULL, na.strings = "na")
	don <- apply(don, 2, str_trim)
	don <- data.frame(don, stringsAsFactors = FALSE)
	names(don) <- nm.head

	return(don)
}

getADCONAWS.15minData <- function(don15min, OUTDIR){
	temps <- paste0(don15min$YEAR, str_pad(as.numeric(don15min$MONTH), 2, pad = "0"),
					str_pad(as.numeric(don15min$DAY), 2, pad = "0"), " ", don15min$TIME)
	utc <- as.POSIXct(strptime(temps, "%Y%m%d %H:%M:%S", tz = "UTC"))
	eat <- as.POSIXct(format(utc, tz = "Africa/Addis_Ababa"), tz = "Africa/Addis_Ababa")
	odt <- order(eat)
	daty <- format(eat[odt], "%Y%m%d%H%M%S")
	dupdt <- duplicated(daty)
	daty <- daty[!dupdt]
	dates <- strptime(daty, "%Y%m%d%H%M%S", tz = "Africa/Addis_Ababa")

	don15min <- don15min[odt, ]
	don15min <- don15min[!dupdt, ]

	stn.id <- don15min[2, 2]
	nm.head <- names(don15min)

	fileinfo <- file.path(OUTDIR, "ADCON_AWS", "compressed_data", "infos", paste0(stn.id, ".rds"))
	if(file.exists(fileinfo)){
		info <- readRDS(fileinfo)
		idaty <- dates > strptime(info$end, "%Y%m%d%H%M%S", tz = "Africa/Addis_Ababa")
		if(!any(idaty)) return(NULL)

		dates <- dates[idaty]
		don15min <- don15min[idaty, , drop = FALSE]
	}

	outdata <- list()
	outdata[["RR"]] <- as.numeric(don15min[[nm.head[nm.head%in%c("APRECP", "APREC")]]])
	outdata[["RH"]] <- as.numeric(don15min[["RELHUM"]])
	outdata[["TT"]] <- as.numeric(don15min[["TEMPTR"]])
	outdata[["RAD"]] <- as.numeric(don15min[[nm.head[nm.head%in%c("RADGLO", "PYRSPL")]]])
	outdata[["FF2"]] <- as.numeric(don15min[["AWINSP"]])
	outdata[["DD2"]] <- if("AWINDR"%in%nm.head) as.numeric(don15min[["AWINDR"]]) else NULL
	outdata[["FF10"]] <- if(any(c("AWIS10", "AWISP10")%in%nm.head)) as.numeric(don15min[[nm.head[nm.head%in%c("AWIS10", "AWISP10")]]]) else NULL
	outdata[["DD10"]] <- if(any(c("AWID10", "AWIND10")%in%nm.head)) as.numeric(don15min[[nm.head[nm.head%in%c("AWID10", "AWIND10")]]]) else NULL

	daty <- format(dates, "%Y%m%d%H%M%S")
	if(!file.exists(fileinfo)){
		info <- list(id = stn.id, name =  don15min[2, 1], start = daty[1], end = daty[length(daty)])
	}else{
		info$end <- daty[length(daty)]
	}
	saveRDS(info, file = fileinfo)

	return(list(date = daty, data = outdata))
}

QC0.Limit.ADCON <- function(X, VAR){
	vlim <- switch(VAR,
				"RR" = c(0, 50),
				"RH" = c(1, 100),
				"TT" = c(-5, 50),
				"RAD" = c(0, 1600),
				"FF2" = c(0, 50),
				"FF10" = c(0, 65),
				"DD2" = c(0, 360),
				"DD10" = c(0, 360),
				NULL)
	if(!is.null(vlim)) X[!is.na(X) & (X < vlim[1] | X > vlim[2])] <- NA
	return(X)
}

getADCONAAWS.SimpleQC <- function(data15min){
	data15minqc <- data15min
	var.data15 <- names(data15min$data)
	data15minqc$data <- lapply(var.data15, function(VAR) QC0.Limit.ADCON(data15min$data[[VAR]], VAR))
	names(data15minqc$data) <- var.data15
	return(data15minqc)
}

getADCONAWS.write15minData <- function(aws, OUTDIR, ftpserver){
	data15min <- getADCONAWS.STN(aws, OUTDIR, ftpserver)
	if(is.null(data15min)) return(NULL)
	####
	aws <- data15min[2, 2]
	data15min <- getADCONAWS.15minData(data15min, OUTDIR)
	if(is.null(data15min)) return(NULL)

	ADCON_DIR <- file.path(OUTDIR, "ADCON_AWS", "compressed_data", "data_15min")
	if(!dir.exists(ADCON_DIR)) dir.create(ADCON_DIR, showWarnings = FALSE, recursive = TRUE)
	file15min <- file.path(ADCON_DIR, paste0(aws, ".rds"))
	if(file.exists(file15min)){
		data.aws <- readRDS(file15min)
		data.aws$date <- c(data.aws$date, data15min$date)
		for(ii in names(data15min$data))
			data.aws$data[[ii]] <- c(data.aws$data[[ii]], data15min$data[[ii]])
	}else data.aws <- data15min
	con1 <- gzfile(file15min, compression = 9)
	open(con1, "wb")
	saveRDS(data.aws, con1)
	close(con1)
	rm(data.aws)

	data15minqc <- getADCONAAWS.SimpleQC(data15min)
	ADCON_DIR <- file.path(OUTDIR, "ADCON_AWS", "compressed_data", "data_15minQC")
	if(!dir.exists(ADCON_DIR)) dir.create(ADCON_DIR, showWarnings = FALSE, recursive = TRUE)
	file15minqc <- file.path(ADCON_DIR, paste0(aws, ".rds"))
	if(file.exists(file15minqc)){
		data.aws <- readRDS(file15minqc)
		data.aws$date <- c(data.aws$date, data15minqc$date)
		for(ii in names(data15minqc$data))
			data.aws$data[[ii]] <- c(data.aws$data[[ii]], data15minqc$data[[ii]])
	}else data.aws <- data15minqc
	con2 <- gzfile(file15minqc, compression = 9)
	open(con2, "wb")
	saveRDS(data.aws, con2)
	close(con2)
	rm(data.aws, data15minqc, data15min)
	return(aws)
}

