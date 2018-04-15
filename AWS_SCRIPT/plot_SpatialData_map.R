
### Path to the folder AWS_DATA
AWS_DATA_DIR <- "/Users/rijaf/Desktop/ETHIOPIA2018/AWS_Data_Processing/AWS_DATA"
### Path to the folder AWS_SCRIPT
AWS_SCRIPT_DIR <- "/Users/rijaf/Desktop/ETHIOPIA2018/AWS_Data_Processing/AWS_SCRIPT"

### AWS system
## "ADCON" or VAISALA
AWS <- "ADCON"

##############
## variable to plot
# Rainfall: "RR"
# Min Temperature: "TN"
# Mean Temperature: "TM"
# Max Temperature: "TX"

variable <- "RR"

### Time step
# Time scale to plot
# 15 minutes data: "15min"
# hourly data: "1hr"
# daily data: "1day"
# dekadal data: "10day"

TimeStep <- "10day"

### Date to plot
# for 15 minute  "YYYYMMDDHHMM" ; minutes: 00, 15, 30, 45
# for hourly "YYYYMMDDHH"
# for daily "YYYYMMDD"
# for dekad "YYYYMMD" ; dekad: 1, 2, 3

date.to.plot <- "2017071"

### shapefile for administrative delimitation
## value: 0 to 3
## country: 0
## province: 1

shp <- 1

### File to save plot in JPEG
polt.jpeg <- "/Users/rijaf/Desktop/TEST.jpg"

########################################## End Edit #############################################
library(stringr)
library(rgdal)
library(fields)

date.to.use <- date.to.plot
source(file.path(AWS_SCRIPT_DIR, "AWS_SpatialData.R"))
source(file.path(AWS_SCRIPT_DIR, "map_functions.R"))
file.shp <- paste0("ETH_adm", shp)
shpf <- file.path(AWS_DATA_DIR, "shapefiles")
map <- readOGR(dsn = shpf, layer = file.shp, verbose = FALSE)
ocrds <- getBoundaries(map)

leglab <- switch(str_trim(variable), 
			"RR" = "Rainfall (mm)",
			"TN" = "Minimum Temperature (C)",
			"TM" = "Mean Temperature (C)",
			"TX" = "Maximum Temperature (C)")

temps <- switch(str_trim(TimeStep),
				"15min" = "15 minutes",
				"1hr" = "Hourly",
				"1day" = "Daily",
				"10day" = "Dekadal")
leglab <- paste(temps, leglab)

xrmap <- range(ocrds[, 1], na.rm = TRUE)
yrmap <- range(ocrds[, 2], na.rm = TRUE)
nx <- nx_ny_as.image(diff(xrmap))
ny <- nx_ny_as.image(diff(yrmap))

AWS.DAT$Lon <- as.numeric(AWS.DAT$Lon)
AWS.DAT$Lat <- as.numeric(AWS.DAT$Lat)
ina <- is.na(AWS.DAT$Lon) | is.na(AWS.DAT$Lat)
AWS.DAT <- AWS.DAT[!ina, , drop = FALSE]

tmp <- as.image(AWS.DAT$values, nx = nx, ny = ny, x = cbind(AWS.DAT$Lon, AWS.DAT$Lat))

xlim <- c(min(c(xrmap, tmp$x)), max(c(xrmap, tmp$x)))
ylim <- c(min(c(yrmap, tmp$y)), max(c(yrmap, tmp$y)))

jpeg(polt.jpeg, width = 11, height = 8, units = "in", res = 400)
opar <- par(mar = c(7, 4, 2.5, 2.5))
plot(1, xlim = xlim, ylim = ylim, xlab = "", ylab = "", type = "n", xaxt = 'n', yaxt = 'n')
axlabsFun <- if(Sys.info()["sysname"] == "Windows") LatLonAxisLabels else LatLonAxisLabels1
axlabs <- axlabsFun(axTicks(1), axTicks(2))
axis(side = 1, at = axTicks(1), labels = axlabs$xaxl, tcl = -0.2, cex.axis = 0.8)
axis(side = 2, at = axTicks(2), labels = axlabs$yaxl, tcl = -0.2, las = 1, cex.axis = 0.8)

breaks <- pretty(tmp$z, n = 10, min.n = 5)
breaks <- if(length(breaks) > 0) breaks else c(0, 1)
kolor <- tim.colors(length(breaks)-1)
legend.args <- list(text = leglab, cex = 0.8, side = 1, line = 2)

abline(h = axTicks(2), v = axTicks(1), col = "lightgray", lty = 3)
lines(ocrds[, 1], ocrds[, 2], lwd = 1, col = "black")

image.plot(tmp, breaks = breaks, col = kolor, horizontal = TRUE,
			xaxt = 'n', yaxt = 'n', add = TRUE, legend.mar = 3.5,
			legend.width = 0.7, legend.args = legend.args,
			axis.args = list(at = breaks, labels = breaks, cex.axis = 0.7,
			font = 2, tcl = -0.3, mgp = c(0, 0.5, 0)))
par(opar)
dev.off()
cat("Plot finished\n")
