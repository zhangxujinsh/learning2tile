source("code/tiling_lib.r")
source("code/bow_lib.r")

resolution <- read.table(paste("data",FILE.SEPERATOR,"frame.info",sep=""))


dir.create(TARGET_DIR)
FINAL_BOWDIR = paste(TARGET_DIR,FILE.SEPERATOR,"spbow",FILE.SEPERATOR,sep="")
dir.create(FINAL_BOWDIR)



experiment.summary <- paste("EXP_ID",EXP_ID,"\n",
			    "WORKING_DIR =", WORKING_DIR, "\n",
			    "TARGET_DIR =", TARGET_DIR, "\n",
			    "DIM = ", DIM, "\n",
			    "TILING_STYLE =", TILING_STYLE, "\n",
			    "TILING_PARA =", paste(TILING_PARA, collapse=" ", sep = " "), "\n",
			    "SOFTMEMBERSHIP_PARA =", paste(SOFTMEMBERSHIP_PARA, collapse=" ", sep = " "), "\n")
				
write(experiment.summary, file=paste(TARGET_DIR,FILE.SEPERATOR,"experiment-summary.txt",sep=""))


getVideoID <- function(filename) {
	return(unlist(strsplit(filename, "\\."))[1])
}



images.txyc.location <- as.matrix(read.table(WORKING_DIR))

#calculating the dim of the final BOW
if(TILING_STYLE == "square") {
	final.bow.dim <- TILING_PARA[1]*TILING_PARA[2]*DIM
} else if(TILING_STYLE == "Diamond") {
	final.bow.dim <- (TILING_PARA[1]+1)*(TILING_PARA[2]+1)*DIM
} else if(TILING_STYLE == "hexagon") {
	#projecting to the universal screen

} else if(TILING_STYLE == "camera") {
	final.bow.dim <- 5*DIM
} else {
	print("Unkown Tiling Style! Please check it again.")
	quit()
}



for(i in 1:length(images.txyc.location)) {
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
	
	final.bow <- vector(mode="numeric",length=final.bow.dim)
	
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
		region.all <- TilingRectangle(TILING_PARA[1],TILING_PARA[2], wmatrix, width, height)
	} else if(TILING_STYLE == "") {
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
