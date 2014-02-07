


#individual runnable tiling code for federate search
args <- commandArgs(TRUE)

if(length(args) != 6) {
	print("Usage:")
	print("resf = c(agrs[1], args[2]) #width height")
	print("DIM = args[3] #dim")
	print("images.txyc.location = args[4] #input txyc filename")
	print("FINAL_BOWDIR = args[5] #output bow DIR")
	print("CodeDir = args[6]  #Code DIR")
	quit()
}

source(paste(args[6], "tiling_lib.r", sep = ""))
source(paste(args[6], "bow_lib.r", sep = ""))

resf = c(as.numeric(args[1]), as.numeric(args[2]))	#width height
DIM = as.numeric(args[3])	#dim
images.txyc.location = args[4]	#input txyc filename
FINAL_BOWDIR = args[5] #output bow DIR




TILING_STYLE = "square"
SOFTMEMBERSHIP_PARA = c(1,1/2,1/3,1/4,1/5,1/6,1/7,1/8,1/9,1/10)

getVideoID <- function(filename) {
	return(unlist(strsplit(filename, "\\."))[1])
}

ToLibSVMBoF <- function(rowvector) {
	idx = which(rowvector != 0)
	spbof = paste("", paste(paste(idx, rowvector[idx], sep=":"), collapse = " "))
	return(spbof)
}



#calculating the dim of the final BOW
if(TILING_STYLE == "square") {
	final.bow.dim <- 8*DIM
}

{
	
	#check whether the file has been processed
	#if(file.exists(paste(FINAL_BOWDIR,basename(images.txyc.location),".spbow",sep="")))	next;
	print(paste("processing ",basename(images.txyc.location), "...", sep=""))
	
	#for the txyc file
	width = as.numeric(resf[1])
	height = as.numeric(resf[2])
	if(is.na(width)||is.na(height))	{
		print(paste("frame info of ",basename(images.txyc.location), " is missing", sep=""))
		next;
	}
	
	final.bow <- vector(mode="numeric",length=final.bow.dim)
	
	txyc_matrix = matrix(0,0,0)
	#check whether the matrix file is empty?
	if(file.info(images.txyc.location)$size <= 0)  {
		write(final.bow, file=paste(FINAL_BOWDIR,getVideoID(basename(images.txyc.location)),".spbow",sep=""), ncol=length(final.bow))
		next		#empty file
	}
	txyc_matrix <- read.matrix(images.txyc.location)
	if(nrow(txyc_matrix) == 0) {
		write(final.bow, file=paste(FINAL_BOWDIR,getVideoID(basename(images.txyc.location)),".spbow",sep=""), ncol=length(final.bow))
		next									#empty matrix
	}
	
	#adjust the cluster center starting from 1
	txyc_matrix[,3:ncol(txyc_matrix)] = txyc_matrix[,3:ncol(txyc_matrix)]+1
	
	
	region.all <- TilingRectangle(1,1, txyc_matrix, width, height)
	file.bow1 = GenSpbow(txyc_matrix, region.all, "l1", SOFTMEMBERSHIP_PARA, DIM)
	file.bow1 = file.bow1/length(region.all$tiles)
	
	region.all <- TilingRectangle(2,2, txyc_matrix, width, height)
	file.bow2 = GenSpbow(txyc_matrix, region.all, "l1", SOFTMEMBERSHIP_PARA, DIM)
	file.bow2 = file.bow2/length(region.all$tiles)
	
	region.all <- TilingRectangle(1,3, txyc_matrix, width, height)
	file.bow3 = GenSpbow(txyc_matrix, region.all, "l1", SOFTMEMBERSHIP_PARA, DIM)
	file.bow3 = file.bow3/length(region.all$tiles)
	
	
	
	
	#normalize

	final.bow = c(file.bow1, file.bow2, file.bow3)
	
	#write out the final bag of words of this video
	write(ToLibSVMBoF(final.bow), file=paste(FINAL_BOWDIR, getVideoID(basename(images.txyc.location)), ".spbof",sep=""))		
}