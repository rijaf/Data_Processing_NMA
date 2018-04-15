
### Path to the folder AWS_DATA
AWS_DATA_DIR <- "/Users/rijaf/Desktop/ETHIOPIA2018/AWS_Data_Processing/AWS_DATA"
### Path to the folder AWS_SCRIPT
AWS_SCRIPT_DIR <- "/Users/rijaf/Desktop/ETHIOPIA2018/AWS_Data_Processing/AWS_SCRIPT"

### AWS system
## "ADCON" or VAISALA
AWS <- "ADCON"

##############
## AWS to plot
# List of station to plot
aws.id <- c("ARKOFE15", "SHADDI15", "ILGORE15", "WOWEGE15")

##############
## variable to plot
# Rainfall: "RR"
# Min Temperature: "TN"
# Mean Temperature: "TM"
# Max Temperature: "TX"

variable <- "TX"

### Time step
# Time scale to plot
# 15 minutes data: "15min"
# hourly data: "1hr"
# daily data: "1day"
# dekadal data: "10day"

TimeStep <- "1day"

### Date range to plot
# for 15 minute  "YYYYMMDDHHMM" ; minutes: 00, 15, 30, 45
# for hourly "YYYYMMDDHH"
# for daily "YYYYMMDD"
# for dekad "YYYYMMD" ; dekad: 1, 2, 3

date.start <- "20160101"
date.end <- "20170930"

### File to save plot in JPEG
polt.jpeg <- "/Users/rijaf/Desktop/Test.jpg"


########################################## End Edit #############################################
library(stringr)

source(file.path(AWS_SCRIPT_DIR, "AWS_TimeSeries.R"))

if(str_trim(TimeStep) == "10day"){
	dek <- as.numeric(substr(daty0, 7, 7))
	dek <- ifelse(dek == 1, "05", ifelse(dek == 2, 15, 25))
	daty0 <- paste0(substr(daty0, 1, 6), dek)
} 
daty <- strptime(daty0, format.limit, tz = "Africa/Addis_Ababa")

kolor <- rainbow(ncol(DATA.AWS))
ylim <- range(pretty(DATA.AWS))
ylab <- switch(str_trim(variable), 
			"RR" = "Rainfall (mm)",
			"TN" = "Minimum Temperature (C)",
			"TM" = "Mean Temperature (C)",
			"TX" = "Maximum Temperature (C)")

temps <- switch(str_trim(TimeStep),
				"15min" = "15 minutes",
				"1hr" = "Hourly",
				"1day" = "Daily",
				"10day" = "Dekadal")
ylab <- paste(temps, ylab)

jpeg(polt.jpeg, width = 11, height = 8, units = "in", res = 400)
layout( matrix(1:2, ncol = 1), widths = 1, heights = c(0.9, 0.2), respect = FALSE)
op <- par(mar = c(3.5, 6.5, 2.5, 2.1))
plot(daty, DATA.AWS[, 1], type = 'n', ylim = ylim, xlab = "", ylab = ylab)
abline(v = axTicks(1), col = "lightgray", lty = "dotted")
abline(h = axTicks(2), col = "lightgray", lty = "dotted")
for(j in seq(ncol(DATA.AWS))) lines(daty, DATA.AWS[, j], type = 'o', lwd = 1.5, col = kolor[j], pch = 21, bg = kolor[j], cex = 0.7)
par(op)
op <- par(mar = c(1, 6.5, 0, 2.1))
plot.new()
legend("center", "groups", legend = STN.NAME, col = kolor, lwd = 3, ncol = 3, cex = 0.8)
par(op)
dev.off()

cat("Plot finished\n")


