library(Matrix)
library(tseries)
library(igraph)
library(pls)
library(gplots)


load.spbof <- function(infile, dim) {
	txt <- read.delim(infile, header = FALSE)
	txt <- as.character(txt[1,1])
	tokens <- strsplit(txt, split=" ")[[1]]
	result <- vector(mode = "numeric", length = dim)
	for(i in 1:length(tokens)) {
		if(tokens[i]=="") next
		temp <- strsplit(tokens[i], split=":")[[1]]
		result[as.numeric(temp[1])] = temp[2]
	}
	return(result)
}


batch.loadspbof <- function(indir, idlistfile, dim) {
	idlist <- read.table(idlistfile, header = FALSE, colClasses = "character")[,1]
	result <- matrix(0, nrow = length(idlist), ncol = dim)
	for(i in 1:length(idlist)) {
		if(!file.exists(paste(indir, "HVC",idlist[i],".spbof",sep=""))) {
			print(paste(indir, "HVC",idlist[i],".spbof",sep="", " is missing"))
			next
		}
		result[i,] = load.spbof(paste(indir, "HVC",idlist[i],".spbof",sep=""),dim)
	}
	return(result)
}

#bin_matrix is a matrix each row is a sample and each column is the label for an event
#copied from the google code in learning2tile
generate.splits <- function(bin_matrix, ncopy = 10) {
	splits <- matrix(nrow = nrow(bin_matrix), ncol = ncopy)
	for(i in 1:ncopy) {
		result =  replicate(nrow(bin_matrix),-1)
		#sample each class
		for(j in 1:ncol(bin_matrix)) {
			population = which(bin_matrix[,j]==1)
			trids <- population[sample(1:length(population), ceiling(length(population)*0.5))]
			tsids <- setdiff(population,trids)
			result[trids] = 0
			result[tsids] = 1
		}
		#for null videos
		{
			population = which(result==-1)
			trids <- population[sample(1:length(population), ceiling(length(population)*0.5))]
			tsids <- setdiff(population,trids)
			result[trids] = 0
			result[tsids] = 1
		}
		splits[,i] = result
	}
	write(t(splits), ncolumn = ncol(splits), file = "data/11745.10.splits")
}


map <- function(run, gt) {
	colname = unique(run[,2])
	aps <- replicate(length(colname), 0)
	for(i in 1:length(colname)) {
		thisids <- which(run[,2] == colname[i])
		thispos.ids <- gt[which(gt[,2] == colname[i]),1]
		thisevent <- matrix(nrow = length(thisids), ncol=2)
		thisevent[,1] <- 0 #label
		thisevent[,2] = as.numeric(run[thisids,3])	#confidence
		for(id in thispos.ids) {
			thisevent[which(run[thisids,1]==id),1] = 1	
		}
		aps[i] = ap(thisevent)
	}
	return(cbind(colname, aps))
}

ap <- function(prediction) {
	result = 0
	ranklist <- prediction[sort(prediction[,2],decreasing=TRUE, index.return=TRUE)$ix,]
	numpos <- length(which(ranklist[,1]==1))
	deltaRecall <- 1/numpos
	pcount <- 0
		
	for(i in 1:nrow(ranklist)) {
		if(ranklist[i,1] == 1) {
			pcount <- pcount + 1
			precision <- pcount/i
			result <- result + precision*deltaRecall
		}
	}
	return(result)
}


randomMAP <- function(numpos, numneg, randcnt = 20, binary = TRUE) {
	map <- 0
	for(i in 1:randcnt) {
		if(!binary) {
			prediction <- matrix(0,(numpos+numneg),2)
			prediction[,2] <- runif(numpos+numneg)
			prediction[sample(1:nrow(prediction),numpos),1] <- 1
			map = map + ap(prediction)
		} else {
			prediction <- matrix(0,(numpos+numneg),2)
			prediction[,2] <- sample(c(0,1),numpos+numneg, replace=TRUE)
			prediction[sample(1:nrow(prediction),numpos),1] <- 1
			map = map + ap(prediction)
		}
	}
	map = map/randcnt
	return(map)
}