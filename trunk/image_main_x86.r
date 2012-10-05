

resolution <- read.table("frame.info")


FILE.SEPERATOR = "\\"
dir.create(TARGET_DIR)
FINAL_BOWDIR = paste(TARGET_DIR,FILE.SEPERATOR,"spbow",FILE.SEPERATOR,sep="")
dir.create(FINAL_BOWDIR)


TILING_STYLE = "square"
TILING_STYLE_PARA
experiment.summary <- paste(" EXP_ID",EXP_ID,"\n",
			    "WORKING_DIR =", WORKING_DIR, "\n",
			    "TARGET_DIR =", TARGET_DIR, "\n",
			    "DIM = ", DIM, "\n",
			    "TILING_PARA =", HORIZIONTAL_GRID, "\n",
			    "TILING_STYLE =", VERTICAL_GRID, "\n",
			    "SOFTMEMBERSHIP_PARA =", paste(SOFTMEMBERSHIP_PARA, collapse=" ", sep = " "), "\r\n")
write(experiment.summary, file=paste(TARGET_DIR,FILE.SEPERATOR,"experiment-summary.txt",sep=""))


getVideoID <- function(filename) {
	return(unlist(strsplit(filename, "\\."))[1])
}


images.txyc.location <- as.matrix(read.table(WORKING_DIR))


for(i in 1:length(images.location)) {
	#check whether the file has been processed
	if(file.exists(paste(FINAL_BOWDIR,basename(images.txyc.location[i]),".spbow",sep="")))	next;
	print(paste("processing ",basename(images.txyc.location[i]), "...", sep=""))
	
	#for the txyc file
	resf <- resolution[which(resolution[,1] == getVideoID(basename(images.txyc.location[i]))) ,2:3]
	width = as.numeric(resf[1])
	height = as.numeric(resf[2])
	if(is.na(width)||is.na(height))	{
		print(paste("frame info of ",basename(images.txyc.location[i]), " is missing", sep=""))
		next;
	}
	
	final.bow <- vector(mode="numeric",length=abs(HORIZIONTAL_GRID) * VERTICAL_GRID * DIM)
	
	wmatrix = matrix(0,0,0)
	#check whether the matrix file is empty?
	if(file.info(images.txyc.location[i])$size <= 0)  {
		write(final.bow, file=paste(FINAL_BOWDIR,basename(videos.dirs[i]),".spbow",sep=""), ncol=length(final.bow))
		next		#empty file
	}
	wmatrix <- read.matrix(images.txyc.location[i])
	if(nrow(wmatrix) == 0) {
		write(final.bow, file=paste(FINAL_BOWDIR,basename(videos.dirs[i]),".spbow",sep=""), ncol=length(final.bow))
		next									#empty matrix
	}
	
	#adjust the cluster center starting from 1
	wmatrix[,3:ncol(wmatrix)] = wmatrix[,3:ncol(wmatrix)]+1
	
	
	
	
	if(TILING_STYLE == "square") {
		region.all <- TilingSquare(TILING_PARA[1],TILING_PARA[2], wmatrix, width, height)
	} else if(TILING_STYLE = "") {
	}

	
	#generate bow for each region
	file.bow <- vector(mode="numeric",length=0)
	for(k in 1:length(region.all$tiles)) {
		if(length(region.all$tiles[[k]])==0) {
			this.bow <- replicate(DIM,0)
		} else {
			this.bow <- SoftMembership(wmatrix[region.all$tiles[[k]],], para= SOFTMEMBERSHIP_PARA, dim=DIM)
			this.bow <- L1normalize(this.bow)
		}
		file.bow <- c(file.bow, this.bow)
	}
	
	#normalize
	file.bow = file.bow/length(region.all$tiles)
	final.bow = file.bow
	
	#write out the final bag of words of this video
	write(final.bow, file=paste(FINAL_BOWDIR, getVideoID(basename(images.txyc.location[i])), ".spbow",sep=""), ncol=length(final.bow))		
}
