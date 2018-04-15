
computeAWS.15minto1hr <- function(aws, AWS, OUTDIR){
	fileinfo <- file.path(OUTDIR, paste0(AWS, "_AWS"), "compressed_data", "infos", paste0(aws, ".rds"))
	if(!file.exists(fileinfo)) return(NULL)
	info <- readRDS(fileinfo)
	if(is.null(info$hour)){
		scalc <- strptime(info$start, "%Y%m%d%H%M%S", tz = "Africa/Addis_Ababa")
	}else{
		## a changer
		# sadd <- if(info$hour$full) 4200 else 600 # 10min
		sadd <- if(info$hour$full) 4500 else 900
		scalc <- strptime(info$hour$end, "%Y%m%d%H", tz = "Africa/Addis_Ababa") + sadd
	}

	filerds <- file.path(OUTDIR, paste0(AWS, "_AWS"), "compressed_data", "data_15minQC", paste0(aws, ".rds"))
	data.aws <- readRDS(filerds)

	icalc <- strptime(data.aws$date, "%Y%m%d%H%M%S", tz = "Africa/Addis_Ababa") >= scalc
	if(!any(icalc)) return(NULL)

	data.aws$date <- data.aws$date[icalc]
	if(AWS == "ADCON"){
		for(ii in names(data.aws$data))
			data.aws$data[[ii]] <- data.aws$data[[ii]][icalc]
	}
	# if(AWS == "VAISALA"){
	# 	for(ii in names(data.aws$data))
	# 		data.aws$data[[ii]] <- data.aws$data[[ii]][icalc]
	# }

	data1hr <- getAWS.VARSData.15minto1hr(data.aws)
	rm(data.aws)
	file1hrs <- file.path(OUTDIR, paste0(AWS, "_AWS"), "compressed_data", "data_1hr", paste0(aws, ".rds"))
	if(file.exists(file1hrs)){
		data.aws <- readRDS(file1hrs)
		for(ii in names(data1hr$data)){
			fooc <- if(ii == "date") c else rbind
			tmp <- if(info$hour$full) data.aws[[ii]] else head(data.aws[[ii]], n = -1)
			data.aws[[ii]] <- fooc(tmp, data1hr$data[[ii]])
		}
	}else data.aws <- data1hr$data

	condon <- gzfile(file1hrs, compression = 9)
	open(condon, "wb")
	saveRDS(data.aws, condon)
	close(condon)

	info$hour <- data1hr$last
	saveRDS(info, file = fileinfo)
	return(aws)
}

getAWS.VARSData.15minto1hr <- function(data15min){
	dates <- strptime(data15min$date, "%Y%m%d%H%M%S", tz = "Africa/Addis_Ababa")
	index <- getAWS.index.15minto1hr(dates)
	nl <- length(index)
	full.hour <- if(length(index[[nl]]) < 4) FALSE else TRUE

	var.noms <- names(data15min$data)
	outdata <- list()
	outdata[["date"]] <- getAWS.date.15minto1hr(dates)
	for(ii in var.noms){
		don15min <- data15min$data[[ii]]
		if(ii == "RR") outdata[[ii]] <- getAWSPrecip.data.15minto1hr(don15min, index, 4)
		if(ii%in%c("RH", "TT", "RAD"))
			outdata[[ii]] <- getAWSClimVars.data.15minto1hr(don15min, index, 3)
	}

	## WIND data
	# ff.min, ff.ave, ff.max, ff.moy, dd.moy
	if(any(c("FF2", "DD2")%in%var.noms)){
		if("FF2"%in%var.noms & !"DD2"%in%var.noms){
			don15min <- data15min$data[["FF2"]]
			wd.tmp <- getAWSClimVars.data.15minto1hr(don15min, index, 3)
			outdata[["WIND2"]] <- cbind(wd.tmp, wd.tmp[, 3], NA)
		}
		if(!"FF2"%in%var.noms & "DD2"%in%var.noms){
			don15min <- data15min$data[["DD2"]]
			wd.tmp <- getAWSAverage.data.15minto1hr(don15min, index, 3)
			outdata[["WIND2"]] <- cbind(NA, NA, NA, NA, wd.tmp)
		}
		if("FF2"%in%var.noms & "DD2"%in%var.noms){
			FF <- data15min$data[["FF2"]]
			DD <- data15min$data[["DD2"]]
			ff.tmp <- getAWSClimVars.data.15minto1hr(FF, index, 3)
			ff.dd.tmp <- getAWSWind.data.15minto1hr(FF, DD, index, 3)
			outdata[["WIND2"]] <- cbind(ff.tmp, ff.dd.tmp)
		}
	}

	if(any(c("FF10", "DD10")%in%var.noms)){
		if("FF10"%in%var.noms & !"DD10"%in%var.noms){
			don15min <- data15min$data[["FF10"]]
			wd.tmp <- getAWSClimVars.data.15minto1hr(don15min, index, 3)
			outdata[["WIND10"]] <- cbind(wd.tmp, wd.tmp[, 3], NA)
		}
		if(!"FF10"%in%var.noms & "DD10"%in%var.noms){
			don15min <- data15min$data[["DD10"]]
			wd.tmp <- getAWSAverage.data.15minto1hr(don15min, index, 3)
			outdata[["WIND10"]] <- cbind(NA, NA, NA, NA, wd.tmp)
		}
		if("FF10"%in%var.noms & "DD10"%in%var.noms){
			FF <- data15min$data[["FF10"]]
			DD <- data15min$data[["DD10"]]
			ff.tmp <- getAWSClimVars.data.15minto1hr(FF, index, 3)
			ff.dd.tmp <- getAWSWind.data.15minto1hr(FF, DD, index, 3)
			outdata[["WIND10"]] <- cbind(ff.tmp, ff.dd.tmp)
		}
	}

	return(list(data = outdata, last = list(end = outdata[["date"]][nl], full = full.hour)))
}

getAWS.date.15minto1hr <- function(dates){
	daty <- format(dates, "%Y%m%d%H%M")
	indx <- split(seq_along(daty), substr(daty, 1, 10))
	temps <- substr(daty[sapply(indx, '[[', 1)], 1, 10)
	return(temps)
}

# getAWS.index.15minto1hr <- function(dates){
# 	daty <- format(dates, "%Y%m%d%H%M")
# 	indx0 <- tapply(dates, substr(daty, 1, 10), function(x) format(x + 900, "%Y%m%d%H%M"))
# 	indx <- relist(match(unlist(indx0), daty), indx0)
# 	return(indx)
# }
getAWS.index.15minto1hr <- function(dates){
	daty <- format(dates, "%Y%m%d%H%M")
	indx0 <- lapply(split(dates, substr(daty, 1, 10)), function(x){
		xx <- seq(strptime(format(x[1], "%Y%m%d%H"), "%Y%m%d%H", tz = "Africa/Addis_Ababa")+900, length.out = 4, by = "15 min")
		format(xx, "%Y%m%d%H%M")
	})
	indx <- relist(match(unlist(indx0), daty), indx0)
	indx <- lapply(indx, function(x) x[!is.na(x)])
	return(indx)
}

################################################################################################

getAWSPrecip.data.15minto1hr <- function(don15min, index, nb.obs.min.precip = 4)
{
	ix <- unlist(index)
	naHr0 <- sapply(index, function(x) length(x[!is.na(x)])) < nb.obs.min.precip

	rr <- relist(don15min[ix], index)

	naHr1 <- sapply(rr, function(x) length(x[!is.na(x)])) < nb.obs.min.precip

	rr <- sapply(rr, sum, na.rm = TRUE)
	rr[naHr0 | naHr1] <- NA
	rr <- matrix(rr, ncol = 1)
	rr <- round(rr, 1)
	return(rr)
}

getAWSAverage.data.15minto1hr <- function(don15min, index, nb.obs.min.var = 3)
{
	ix <- unlist(index)
	naHr0 <- sapply(index, function(x) length(x[!is.na(x)])) < nb.obs.min.var

	vari <- relist(don15min[ix], index)
	naHr1 <- sapply(vari, function(x) length(x[!is.na(x)])) < nb.obs.min.var

	vari <- sapply(vari, mean, na.rm = TRUE)
	vari[naHr0 | naHr1] <- NA
	vari <- matrix(vari, ncol = 1)
	vari <- round(vari, 1)
	return(vari)
}

getAWSClimVars.data.15minto1hr <- function(don15min, index, nb.obs.min.var = 3)
{
	ix <- unlist(index)
	naHr0 <- sapply(index, function(x) length(x[!is.na(x)])) < nb.obs.min.var

	tmp.min <- relist(don15min[ix], index)
	tmp.ave <- tmp.min
	tmp.max <- tmp.min

	naHrMin <- sapply(tmp.min, function(x) length(x[!is.na(x)])) < nb.obs.min.var
	naHrAve <- sapply(tmp.ave, function(x) length(x[!is.na(x)])) < nb.obs.min.var
	naHrMax <- sapply(tmp.max, function(x) length(x[!is.na(x)])) < nb.obs.min.var

	tmp.min <- suppressWarnings(sapply(tmp.min, min, na.rm = TRUE))
	tmp.ave <- suppressWarnings(sapply(tmp.ave, mean, na.rm = TRUE))
	tmp.max <- suppressWarnings(sapply(tmp.max, max, na.rm = TRUE))

	tmp.min[naHr0 | naHrMin] <- NA
	tmp.ave[naHr0 | naHrAve] <- NA
	tmp.max[naHr0 | naHrMax] <- NA

	tmp <- cbind(tmp.min, tmp.ave, tmp.max)
	tmp <- round(tmp, 1)
	dimnames(tmp) <- NULL

	return(tmp)
}

getAWSWind.data.15minto1hr <- function(ff, dd, index, nb.obs.min.var = 3)
{
	ix <- unlist(index)
	naHr0 <- sapply(index, function(x) length(x[!is.na(x)])) < nb.obs.min.var

	xu <- -ff * sin(2*pi*dd/360)
	xv <- -ff * cos(2*pi*dd/360)
	ina <- is.na(xu) | is.na(xv)
	xu[ina] <- NA
	xv[ina] <- NA
	xu <- relist(xu, index)
	xv <- relist(xv, index)
	naHrUV <- sapply(xu, function(x) length(x[!is.na(x)])) < nb.obs.min.var

	mu <- sapply(xu, mean, na.rm = TRUE)
	mv <- sapply(xv, mean, na.rm = TRUE)
	ff.moy <- sqrt(mu^2 + mv^2)
	dd.ave <- (atan2(mu, mv) * 360/2/pi) + ifelse(ff.moy < 1e-14, 0, 180)

	ff.moy[naHr0 | naHrUV] <- NA
	dd.ave[naHr0 | naHrUV] <- NA

	wnd <- cbind(ff.moy, dd.ave)
	wnd <- round(wnd, 1)

	dimnames(wnd) <- NULL
	return(wnd)
}

##########################################################
	### get 1 hr
	## Note
	## data at 10 hour are computed using data from 10:15:00 to 11:00:00

################################################################################################


# OUTDIR <- "/Users/rijaf/Desktop/ETHIOPIA2018/AWS_Data_Processing/AWS_DATA"
# # c("ADCON", "VAISALA")
# AWS <- "ADCON"

# ######################
# dirADCON <- file.path(OUTDIR, paste0(AWS, "_AWS"), "compressed_data", "infos")
# allrds <- list.files(dirADCON, ".rds")
# allADCON <- gsub(".rds", "", allrds)

# ret.1hr <- lapply(allADCON, function(aws){
# 	cat(aws, '\n')
# 	computeAWS.15minto1hr(aws, AWS, OUTDIR)
# })



