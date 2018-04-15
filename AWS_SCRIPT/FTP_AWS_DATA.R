
### Path to the folder AWS_DATA
AWS_DATA_DIR <- "/Users/rijaf/Desktop/ETHIOPIA2018/AWS_Data_Processing/AWS_DATA"
### Path to the folder AWS_SCRIPT
AWS_SCRIPT_DIR <- "/Users/rijaf/Desktop/ETHIOPIA2018/AWS_Data_Processing/AWS_SCRIPT"

########################################## End Edit #############################################

readLinesFtpFileTail <- function(url, userpwd, n, buff){
	curl <- getCurlHandle(ftp.use.epsv = FALSE, userpwd = userpwd)
	on.exit({
		rm(curl)
		gc()
	})
	filecon <- getURL(url, nobody = TRUE, header = TRUE, curl = curl)

	filecon <- unlist(strsplit(filecon, "\r\n"))
	size <- gsub("[^[:digit:]]", "", filecon[grep("Content-Length:", filecon)])
	size <- as.numeric(size)

	bufferSize <- as.integer(buff*n)
	pos1 <- size - bufferSize
	pos2 <- size
	text <- character()
	k <- 0L

	while(TRUE){
		chars <- getURL(url, range = paste(pos1, pos2, sep = "-"),
						nobody = FALSE, header = FALSE, curl = curl)

		k <- k + length(gregexpr(pattern = "\\n", text = chars)[[1]])
		text <- paste0(chars, text)

		if(k > n || pos1 == 0) break
		bufferSize <- as.integer(buff*(n-k+10))
		pos2 <- pos1-1
		pos1 <- max(pos1-bufferSize, 0)
		Sys.sleep(2)
	}
	text <- tail(strsplit(text, "\\n")[[1]], n)
	return(text)
}

############################################

getFTPData <- function(file0, tmpfile, userpwd){
	curl <- getCurlHandle(userpwd = userpwd) 
	on.exit({
		rm(curl)
		gc()
	})
	filecon <- getURL(file0, curl = curl)
	don <- readLines(textConnection(filecon))
	cat(don, file = tmpfile, sep = "\n")
	return(0)
}

is.leapyear <- function(year){
	leap <- ((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0)
	return(leap)
}

############################################
library(stringr)
library(RCurl)
library(tools)
library(R.utils)

source(file.path(AWS_SCRIPT_DIR, "getADCONAWS_data.R"))
source(file.path(AWS_SCRIPT_DIR, "getVAISLAAWS_data.R"))
source(file.path(AWS_SCRIPT_DIR, "AWS_15minto1hour.R"))
source(file.path(AWS_SCRIPT_DIR, "AWS_1hourto1day.R"))
source(file.path(AWS_SCRIPT_DIR, "AWS_1dayto10days.R"))
auth <- readRDS(file.path(AWS_DATA_DIR, "coordinates_files", "ftpserver.nma"))

############################################
# c("ADCON", "VAISALA")

ret1hr <- lapply("ADCON", function(AWS){
	cat(paste("Process", AWS, "data ......"), '\n')
	curl <- getCurlHandle(userpwd = auth$AWS[[AWS]]$userpwd, ftp.use.epsv = FALSE, dirlistonly = TRUE)
	listAWS <- try(getURL(auth$AWS[[AWS]]$ftp, curl = curl), silent = TRUE)
	rm(curl); gc()
	if(inherits(listAWS, "try-error")){
		cat(paste("Unable to connect to", AWS), '\n')
		return(NULL)
	}

	listAWS <- unlist(strsplit(listAWS, "\r?\n"))
	if(AWS == "ADCON"){
		AWSstn <- listAWS[grep('*.csv*$', listAWS)]
		get15minutesData <- getADCONAWS.write15minData
	}

	## VAISALA  <<-- penser autrement
	if(AWS == "VAISALA"){
		# AWSstn <- listAWS[grep("*_MSG_RAW_*.his*$", listAWS)]
		# AWSstn <- gsub("[^[:digit:]]", "", AWSstn)
		# get15minutesData <- getVAISALAAWS.write15minData
	}

	### get 15 min
	ret.aws <- lapply(AWSstn, function(aws){
		cat(paste("Check", aws, "from ADCON Server"), "\n")
		get15minutesData(aws, AWS_DATA_DIR, auth)
	})


	inull <- sapply(ret.aws, is.null)
	aws.1hr <- unlist(ret.aws[!inull])
	if(length(aws.1hr) == 0) return(NULL)

	### get 1 hr
	## Note
	## data at 10 hour are computed using data from 10:10:00 to 11:00:00
	ret.1hr <- lapply(aws.1hr, function(aws) computeAWS.15minto1hr(aws, AWS, AWS_DATA_DIR))

	inull <- sapply(ret.1hr, is.null)
	aws.1day <- unlist(ret.1hr[!inull])
	if(length(aws.1day) == 0) return(NULL)

	### get 1 day
	## Note 
	## precip for 2014-01-23 are computed using hourly data from 2014-01-23 09:00:00 to 2014-01-24 08:00:00
	## others vars for 2014-01-23 are computed using hourly data from 2014-01-23 00:00:00 to 2014-01-23 23:00:00

	ret.1day <- lapply(aws.1day, function(aws) computeAWS.1hrto1day(aws, AWS, AWS_DATA_DIR))

	inull <- sapply(ret.1day, is.null)
	aws.10day <- unlist(ret.1day[!inull])
	if(length(aws.10day) == 0) return(NULL)

	### get 10 days

	ret.10days <- lapply(aws.10day, function(aws) computeAWS.1dayto10days(aws, AWS, AWS_DATA_DIR))

	cat(paste("Processing", AWS, "data done"), '\n')
	return(0)
})

