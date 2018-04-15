

## parse vaisala history data
parse.vaisala <- function(){
	require(stringr)

	dirVaisala <- "/Users/rijaf/Desktop/ETHIOPIA2018/vaisala_history"
	dirOUT <- "/Users/rijaf/Desktop/ETHIOPIA2018/Vaisala_data_parsed"

for(year in 2016:2018){
	for(month in str_pad(1:12, 2, pad = "0")){
		for(day in str_pad(1:31, 2, pad = "0")){
			for(aws in str_pad(1:51, 3, pad = "0")){
				file.aws <- file.path(dirVaisala, year, month, day, paste0(aws, "_MSG_RAW_", day, ".his"))
				if(!file.exists(file.aws)) next

				cat(paste(year, month, day, aws), '\n')

				dat.aws <- readLines(file.aws, encoding = "UTF-8")
				dat.aws <- dat.aws[-(1:2)]

				donne.aws <- lapply(seq_along(dat.aws), function(jj){
					# cat(jj, '\n')
					xx <- dat.aws[jj]
					xx <- lapply(strsplit(xx, ",")[[1]], function(x){
						rr <- try(str_trim(x), silent = TRUE)
						if(inherits(rr, "try-error")) "" else rr
					})
					if(xx[[3]] == "") return(NULL)
					xx <- strsplit(xx[[3]], "\\\001SMS")[[1]]
					name.par <- str_match(xx, ".*S: *(.*?) *;.*")
					if(!is.na(name.par[1, 2])){
						if(grepl("\\(|\\)", name.par[1, 2]))
							xx <- gsub(gsub("\\)", "\\\\)", gsub("\\(", "\\\\(", name.par[1, 2])), gsub("\\(|\\)", "", name.par[1, 2]), xx)
					}
					if(length(xx) > 1){
						xx1 <- lapply(seq_along(xx), function(j){
							xtmp <- regmatches(xx[j], gregexpr("(?<=\\().*?(?=\\))", xx[j], perl=T))[[1]]
							if(length(xtmp > 0)) xtmp else NULL
						})
						xx1 <- xx1[!sapply(xx1, is.null)]
						if(length(xx1) == 0) return(NULL)
						xx <- xx1[[1]]
					}else xx <- regmatches(xx, gregexpr("(?<=\\().*?(?=\\))", xx, perl=T))[[1]]
					if(length(xx) == 0) return(NULL)

					xx <- lapply(strsplit(xx, ";")[[1]], strsplit, split = ":")
					xx <- lapply(lapply(xx, '[[', 1), function(x){
						x <- str_trim(x)
						if(length(x) == 3 & substr(x, 1, 3)[1] == "WDG") x <- c(x[1], paste0(x[2], ":", x[3]))
						x
					})
					xx <- lapply(xx, function(x){
						isna <- x[2]%in%c("-9999.9", "9999.9", "NaN", sapply(1:10, function(j) paste0(rep('/', j), collapse = "")))
						if(isna) x[2] <- NA
						c(x[1], x[2])
					})
					xx <- lapply(xx, function(x) c(strsplit(x[1], "\\|")[[1]], x[2]))
					if(any(sapply(xx[-(1:5)], length) != 7)) return(NULL)
					stn.name <- xx[[1]][2]
					stn.id <- xx[[4]][2]
					daty <- paste0(20, xx[[2]][2], xx[[3]][2])
					xx <- do.call(rbind, xx[-(1:5)])
					list(stn.name = stn.name, stn.id = stn.id, date = daty, data = xx)
				})

				donne.aws <- donne.aws[!sapply(donne.aws, is.null)]
				if(length(donne.aws) > 0){
					dirDay <- file.path(dirOUT, year, month, day)
					dir.create(dirDay, showWarnings = FALSE, recursive = TRUE)
					fileout <- file.path(dirDay, paste0(aws, ".rds"))
					con <- gzfile(fileout, compression = 9)
					open(con, "wb")
					saveRDS(donne.aws, con)
					close(con)
				}
				rm(donne.aws, dat.aws)
			}
		}
	}
}

	return(NULL)
}


# convert parsed vaisala data into csv by station name
convert.vaisala.2csv <- function(){
	require(stringr)

	datadir <- "/Users/rijaf/Desktop/ETHIOPIA2018/Vaisala_data_parsed"
	outdir <- "/Users/rijaf/Desktop/ETHIOPIA2018/Vaisala_all_data_csv"

for(year in 2016:2018){
	for(month in str_pad(1:12, 2, pad = "0")){
		for(day in str_pad(1:31, 2, pad = "0")){
			for(aws in str_pad(1:51, 3, pad = "0")){
				file.aws <- file.path(datadir, year, month, day, paste0(aws, ".rds"))
				if(!file.exists(file.aws)) next

				cat(paste(year, month, day, aws), '\n')

				don <- readRDS(file.aws)
				ret <- lapply(don, function(xx){
					stn.name <- xx$stn.name
					stn.id <- xx$stn.id
					daty <- xx$date
					dat <- xx$data

					if(nrow(dat) != 144) return(NULL)

					res <- matrix(c(aws, stn.id, stn.name, daty, dat[, 7]), nrow = 1)
					stn.name <- sub("/", " ", stn.name)
					file.out <- file.path(outdir, paste0(stn.name, ".csv"))
					write.table(res, file.out, col.names = FALSE, row.names = FALSE, sep = ",", append = TRUE)

					return(0)
				})
			}
		}
	}
}

	nom <- apply(don[[1]]$data[, 1:6], 1, paste, collapse = "|")
	nom <- c("ID_File", "ID_Data", "NAME", "TIME", nom)

	allcsv <- list.files(outdir, ".csv")
	for(csv in allcsv){
		file.csv <- file.path(outdir, csv)
		don <- read.table(file.csv, sep = ",")
		names(don) <- nom
		write.table(don, file.csv, col.names = TRUE, row.names = FALSE, sep = ",")
	}

	return(NULL)
}

