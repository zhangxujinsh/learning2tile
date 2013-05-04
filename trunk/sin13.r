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


loadSubXML <- function(infile, idhashtable, default_value = 0, rank = FALSE) {
	xml <- read.table(infile, colClasses = c("numeric","numeric", "character", "numeric","numeric", "character"))
	xml <- droplevels(xml)
	xml <- xml[,c(1,3,4,5)]
		
	xml[,1] <- xml[,1]-1000
		
		
	if(rank) {
		xml[,4] <- 2000-xml[,3]
	} else {
		xml[,4] <- 1-0.0005*xml[,3]
	}
		
	run <- matrix(default_value, nrow = length(idhashtable), ncol = max(xml[,1]))
	for(i in unique(xml[,1])) {
		print(i)
		subtab <- droplevels(xml[which(xml[,1]==i),])
		for(j in 1:nrow(subtab)) {
			run[idhashtable[[subtab[j,2]]],i] = subtab[j,4]
		}
	}
	return(run)
}


generate.submission <- function(predictions, concept_id, idtable, runType) {
	#idhashtable <- new.env()
	#for(i in 1:length(idtable)) idhashtable[[idtable[i]]] = i
		
	baseID = -1
	
	if(runType == "f" || runType == "l") {
		baseID = 1000
	} else if(runType == "p") {
		baseID = 1900
	}
		
	submission <- matrix(nrow=2000, ncol=ncol(predictions))
	for( i in 1:ncol(predictions)) {
		submission[,i] = sort(predictions[,i],decreasing = TRUE, index.return = TRUE)$ix[1:2000]
	}
		
	numofrow = ncol(submission)*2000
	col1 <- vector(mode = "numeric", length = numofrow)
	col2 <- vector(mode = "numeric", length = numofrow)
	col3 <- vector(mode = "character", length = numofrow)
	col4 <- vector(mode = "numeric", length = numofrow)	#rank
	col5 <- vector(mode = "numeric", length = numofrow)
	col6 <- replicate(numofrow, "CMU")
		
	for(i in 1:ncol(predictions)) {
		idrange = (1+(i-1)*2000):(i*2000)
		col1[idrange] = baseID + i
		col4[idrange] = 1:2000
		col5[idrange] = 9999:8000
		col3[idrange] = idtable[submission[,i]]
		col6[idrange] = concept_id[i,2]
	}
		
		
	#return as a dataframe
	final <- data.frame(col1, col2, col3, col4, col5, col6)
		
	return(final)
	#write.table(final, file = "test1.xml", quote = FALSE, col.names = FALSE, row.names= FALSE)
}
	
	
	
transPerlOutput <- function(tab) {
	valuematrix <- matrix(0, nrow=500, ncol=2)
	cnt = 1
	for(i in 1:length(tab)) {
		if(length(grep("infAP",tab[i])) != 0) {
			strs <- strsplit(grep("infAP",tab[i],value = TRUE),"\t+")[[1]]
			if(strs[2] == "all") next
			valuematrix[cnt,1] = as.numeric(strs[2])
			valuematrix[cnt,2] = as.numeric(strs[3])
			cnt = cnt + 1
		}
	}
		
	cnt  = cnt - 1
	return(valuematrix[1:cnt,])
}

#analysis how many unique video in each 2000 keyframes
unique.video.analysis <- function(submission, col.ids) {
	result = replicate(length(col.ids),0)
	cnt = 1
	for(i in col.ids) {
		subsub <- submission[which(submission[,1] == i),]
		subsub[,3] = as.vector(subsub[,3])
		thisinfo = vector(mode="character", length=2000)
		for(j in 1:2000) {
			thisinfo[j] = strsplit(subsub[j,3],"_")[[1]][1]
		}
		result[cnt] = length(unique(thisinfo))
		cnt = cnt + 1
	}
	return(result)
}



inspectkeyframe <- function(submission, col.ids, topn = 200, outdir = "test\\", keyframe.dir = "..\\tv12.testing.tar\\") {
	for( i in col.ids) {
		subsub <- submission[which(submission[,1] == i),]
		thisoutdir <- paste(outdir,subsub[1,6],sep="")
		dir.create(thisoutdir)
		for(j in 1:topn) {
			outfilename = paste(thisoutdir, "\\", j, "_", as.character(subsub[j,3]), "_RKF.jpg", sep="")
			file.copy(paste(keyframe.dir, as.character(subsub[j,3]), "_RKF.jpg", sep=""), outfilename)
		}
	}
}


#match the loclist and generate a loclist corresponding to the
#sequence of idlist
matchIDLocList <- function(idlist, loclist) {
	rids = lapply(idlist,function(x){return(which(loclist==x))})
	return(rids)
}


#annolist is a list of prased annotation like
#120800 0
#17 0
inspectkeyframe2 <- function(annolist, keyframe.loclist, conceptnames = NULL, topn = 500, outdir = "test\\") {
	for(i in 1:length(annolist)) {
		if(is.null(conceptnames)) {
			thisoutdir <- paste(outdir,i,sep="")
		} else {
			thisoutdir <- paste(outdir,conceptnames[i],sep="")
		}
		dir.create(thisoutdir)
		filenames <- keyframe.loclist[annolist[[i]][which(annolist[[i]][,2] == 1),1]]
		for(j in 1:min(topn,length(filenames))) {
			outfilename = paste(thisoutdir, "\\", basename(filenames[j]), sep="")
			file.copy(filenames[j], outfilename)
		}
	}
}

