


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
TILING_PARA = c(2,2)
SOFTMEMBERSHIP_PARA = c(1,1/2,1/3,1/4,1/5,1/6,1/7,1/8,1/9,1/10)

getVideoID <- function(filename) {
	return(unlist(strsplit(filename, "\\."))[1])
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
	
	if(TILING_STYLE == "square") {
		region.all <- TilingRectangle(TILING_PARA[1],TILING_PARA[2], txyc_matrix, width, height)
	} else if(TILING_STYLE == "diamond") {
		region.all <- TilingDiamond(TILING_PARA[1],TILING_PARA[2], txyc_matrix, width, height)
	} else if(TILING_STYLE == "camera") {
		region.all <- TilingCamera(TILING_PARA, txyc_matrix, width, height)
	} else if(TILING_STYLE == "hexagon") {
		ptxyc_matrix <- Project2AUniformSize(txyc_matrix, owidth=width, oheight=height, pwidth = 500, pheight = 333)
		region.all <- TilingHexagon(TILING_PARA, ptxyc_matrix, 500, 333)
	} else if(TILING_STYLE == "ellipse") {
		region.all <- TilingEllipse(TILING_PARA, txyc_matrix, width, height)
	}

	
	file.bow = GenSpbow(txyc_matrix, region.all, "l1", SOFTMEMBERSHIP_PARA, DIM)
	
	
	#normalize
	file.bow = file.bow/length(region.all$tiles)
	final.bow = file.bow
	
	#write out the final bag of words of this video
	write(final.bow, file=paste(FINAL_BOWDIR, getVideoID(basename(images.txyc.location)), ".spbow",sep=""), ncol=length(final.bow))		
}