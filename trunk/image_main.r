source("code/tiling_lib.r")
source("code/bow_lib.r")

resolution <- read.table(paste("data",FILE.SEPERATOR,"frame.info",sep=""),colClasses = c("character", "numeric","numeric"))


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
	
	txyc_matrix = matrix(0,0,0)
	#check whether the matrix file is empty?
	if(file.info(images.txyc.location[i])$size <= 0)  {
		write(final.bow, file=paste(FINAL_BOWDIR,basename(videos.dirs[i]),".spbow",sep=""), ncol=length(final.bow))
		next		#empty file
	}
	txyc_matrix <- read.matrix(images.txyc.location[i])
	if(nrow(txyc_matrix) == 0) {
		write(final.bow, file=paste(FINAL_BOWDIR,basename(videos.dirs[i]),".spbow",sep=""), ncol=length(final.bow))
		next									#empty matrix
	}
	
	#adjust the cluster center starting from 1
	txyc_matrix[,3:ncol(txyc_matrix)] = txyc_matrix[,3:ncol(txyc_matrix)]+1
	
	if(TILING_STYLE == "square") {
		region.all <- TilingRectangle(TILING_PARA[1],TILING_PARA[2], txyc_matrix, width, height)
	} else if(TILING_STYLE == "diamond") {
		ptxyc_matrix <- Project2AUniformSize(txyc_matrix, owidth=width, oheight=height, pwidth = 500, pheight = 333)
		region.all <- TilingDiamond(TILING_PARA[1],TILING_PARA[2], txyc_matrix, 500, 333)
	} else if(TILING_STYLE == "camera") {
		region.all <- TilingCamera(TILING_PARA, txyc_matrix, width, height)
	} else if(TILING_STYLE == "hexagon") {
		ptxyc_matrix <- Project2AUniformSize(txyc_matrix, owidth=width, oheight=height, pwidth = 500, pheight = 333)
		region.all <- TilingHexagon(TILING_PARA, txyc_matrix, 500, 333)
	} else if(TILING_STYLE == "ellipse") {
		region.all <- TilingEllipse(TILING_PARA, txyc_matrix, width, height)
	}

	
	file.bow = GenSpbow(txyc_matrix, region.all, "l1", SOFTMEMBERSHIP_PARA, DIM)
	
	
	#normalize
	file.bow = file.bow/length(region.all$tiles)
	final.bow = file.bow
	
	#write out the final bag of words of this video
	write(final.bow, file=paste(FINAL_BOWDIR, getVideoID(basename(images.txyc.location[i])), ".spbow",sep=""), ncol=length(final.bow))		
}
