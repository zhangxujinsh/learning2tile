source("code/tiling_lib.r")
source("code/bow_lib.r")

videoinfo <- read.table(paste("data",FILE.SEPERATOR,"raw.video.info",sep=""),colClasses = c("character", "numeric","numeric", "character", "character", "character"))


dir.create(TARGET_DIR)
FINAL_BOWDIR = paste(TARGET_DIR,FILE.SEPERATOR,"spbow",FILE.SEPERATOR,sep="")
dir.create(FINAL_BOWDIR)

if(exists("SPATIAL_POOLING") == FALSE) SPATIAL_POOLING = "avg"

experiment.summary <- paste("EXP_ID",EXP_ID,"\n",
				"SPATIAL_POOLING =", SPATIAL_POOLING, "\n",
			    "WORKING_DIR =", WORKING_DIR, "\n",
				"TEMPORAL_POOLING=", TEMPORAL_POOLING, "\n",
			    "TARGET_DIR =", TARGET_DIR, "\n",
			    "DIM = ", DIM, "\n",
			    "TILING_STYLE =", TILING_STYLE, "\n",
			    "TILING_PARA =", paste(TILING_PARA, collapse=" ", sep = " "), "\n",
			    "SOFTMEMBERSHIP_PARA =", paste(SOFTMEMBERSHIP_PARA, collapse=" ", sep = " "), "\n")
				
write(experiment.summary, file=paste(TARGET_DIR,FILE.SEPERATOR,"experiment-summary.txt",sep=""))


getVideoID <- function(filename) {
	return(unlist(strsplit(filename, "\\."))[1])
}


#format HVC51_06^40.540.txyc"
parsetime.keyframe <- function(filenamestr) {
	str1 <- strsplit(filenamestr, split = "_")[[1]][2]
	temps <- strsplit(str1, split = "[\\^\\.]")[[1]]
	minute <- as.numeric(temps[1])
	second <- as.numeric(temps[2])
	msecond <- as.numeric(temps[3])
	duration <- minute * 60 + second + msecond/1000
	return(duration)
}

#format 00:01:03.06
parsetime.videoinfo <- function(videoinfoduration) {
	temps <- strsplit(videoinfoduration, split = "[:\\.]")[[1]]
	hour = as.numeric(temps[1])
	minute = as.numeric(temps[2])
	second = as.numeric(temps[3])
	msecond <- as.numeric(temps[4])
	duration <- hour * 3600 + 60 * minute + second + msecond/100
	return(duration)
}




#calculating the dim of the final BOW
if(TILING_STYLE == "square") {
	final.bow.dim <- TILING_PARA[1]*TILING_PARA[2]*DIM
} else if(TILING_STYLE == "diamond") {
	region.all <- TilingDiamond(TILING_PARA[1],TILING_PARA[2], c(), 500, 333)
	final.bow.dim<- length(region.all$tiles)*DIM
} else if(TILING_STYLE == "hexagon") {
	#projecting to the universal screen with
	#width = 555
	#height = 300
	region.all <- TilingHexagon(TILING_PARA, c(), 500, 333)
	final.bow.dim<- length(region.all$tiles)*DIM
} else if(TILING_STYLE == "camera") {
	final.bow.dim <- 5*DIM
} else if(TILING_STYLE == "ellipse") {
	final.bow.dim  <- (TILING_PARA+4)*DIM
} else {
	print("Unkown Tiling Style! Please check it again.")
	quit()
}
final.bow.dim = final.bow.dim * TEMPORAL_TILING_PARA




images.txyc.location <- as.matrix(read.table(WORKING_DIR))

for(i in 1:length(images.txyc.location)) {
	thisvideo.txycfiles <- list.files(path=images.txyc.location[i], pattern=".+txyc", full.names = TRUE);
	
	print(paste("processing ",basename(images.txyc.location[i]), "...", sep=""))
	
	keyframe.timestamps <- unlist(lapply(thisvideo.txycfiles, parsetime.keyframe))
	
	
	
	#temporal information
	duration <- videoinfo[which(videoinfo[,1] == getVideoID(basename(images.txyc.location[i]))) ,5]
	if(length(duration) == 0 || is.na(duration)) {
		print(paste("duration info of ",basename(images.txyc.location[i]), " is missing", sep=""))
		next
	}
	duration = parsetime.videoinfo(duration)
	intervals = seq(0,duration,duration/TEMPORAL_TILING_PARA)
	
	temporal.indexes = replicate(length(thisvideo.txycfiles),0)
	for(j in 1:length(thisvideo.txycfiles)) {
		temporal.indexes[j] = min(which(keyframe.timestamps[j] < intervals))-2
	}
	
	
	
	#spatial info
	resf <- videoinfo[which(videoinfo[,1] == getVideoID(basename(images.txyc.location[i]))) ,2:3]
	width = as.numeric(resf[1])
	height = as.numeric(resf[2])
	if(is.na(width)||is.na(height))	{
		print(paste("frame info of ",basename(images.txyc.location[i]), " is missing", sep=""))
		next;
	}
	

	
	final.bow <- vector(mode="numeric",length=final.bow.dim)
	#check whether the foler is empty?
	if(length(thisvideo.txycfiles) == 0)  {
		write(final.bow, file=paste(FINAL_BOWDIR,getVideoID(basename(images.txyc.location[i])),".spbow",sep=""), ncol=length(final.bow))
		next		#empty foler
	}
	
	
	
	for(j in 1:length(thisvideo.txycfiles)) {
		#for the txyc file
		txyc_matrix = matrix(0,0,0)
		
		if(file.info(thisvideo.txycfiles[j])$size <= 0)  {
			next		#empty txyc
		}
		
		#read the txyc matrix
		txyc_matrix <- read.matrix(thisvideo.txycfiles[j])
		
		if(nrow(txyc_matrix) == 0) {
			next		#empty matrix
		}
		
		#adjust the cluster center starting from 1
		txyc_matrix[,3:ncol(txyc_matrix)] = txyc_matrix[,3:ncol(txyc_matrix)]+1
		
		if(TILING_STYLE == "square") {
			region.all <- TilingRectangle(TILING_PARA[1],TILING_PARA[2], txyc_matrix, width, height)
		} else if(TILING_STYLE == "diamond") {
			ptxyc_matrix <- Project2AUniformSize(txyc_matrix, owidth=width, oheight=height, pwidth = 500, pheight = 333)
			region.all <- TilingDiamond(TILING_PARA[1],TILING_PARA[2], ptxyc_matrix, 500, 333)
		} else if(TILING_STYLE == "camera") {
			region.all <- TilingCamera(TILING_PARA, txyc_matrix, width, height)
		} else if(TILING_STYLE == "hexagon") {
			ptxyc_matrix <- Project2AUniformSize(txyc_matrix, owidth=width, oheight=height, pwidth = 500, pheight = 333)
			region.all <- TilingHexagon(TILING_PARA, ptxyc_matrix, 500, 333)
		} else if(TILING_STYLE == "ellipse") {
			region.all <- TilingEllipse(TILING_PARA, txyc_matrix, width, height)
		}

		
		if(tolower(SPATIAL_POOLING) == "max") {
			#max pooling
			file.bow = GenSpbow.MaxPooling(txyc_matrix, region.all, "l1", SOFTMEMBERSHIP_PARA, DIM)
		} else {
			#average pooling
			file.bow = GenSpbow(txyc_matrix, region.all, "l1", SOFTMEMBERSHIP_PARA, DIM)
		}
		
		
		#normalize
		file.bow = file.bow/length(region.all$tiles)
		thisids <- (temporal.indexes[j]*length(file.bow)+1) : ((temporal.indexes[j]+1)*length(file.bow))
		
		if(tolower(TEMPORAL_POOLING) == "max") {
			final.bow[thisids] = pmax(final.bow[thisids], file.bow)
		} else {
			#L1 norm the video level
			final.bow[thisids] = final.bow[thisids] + file.bow/(length(thisvideo.txycfiles))
		}
	}
	
	#write out the final bag of words of this video
	write(final.bow, file=paste(FINAL_BOWDIR, getVideoID(basename(images.txyc.location[i])), ".spbow",sep=""), ncol=length(final.bow))		
}
