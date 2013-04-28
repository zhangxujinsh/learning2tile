require(Matrix)
require(tseries)


RandomSplitBackup <- function(labelfile, ncopy = 10) {
	d =as.vector(read.table(labelfile))
	classes = unique(d[,2])
	for(i in 1:ncopy) {
		result =  replicate(nrow(d),-1)
		#sample each class
		for(j in classes) {
			population = which(d[,2]==j)
			trids <- population[sample(1:length(population), ceiling(length(population)/2))]
			tsids <- setdiff(population,trids)
			result[trids] = 0
			result[tsids] = 1
		}
		partation = d
		partation[,2] = result
		write(t(partation), file = paste("partation",i,".par",sep=""), ncol = 2)
	}
}


RandomSplit <- function(labelfile, ncopy = 10, multilabel.balance=FALSE) {
	#multilabel.balance for a multiclass classification whether ensure each class has at least 50% training data
	#constrcting a dataset with more training data
	d = as.vector(read.table(labelfile))
	labs <- list()
	classes <- c()
	for(i in 1:nrow(d)) {
		labs[[i]] = unassemble(as.character(d[i,2]))
		classes = union(classes, labs[[i]])
	}
	#to binary matrix
	
	
	classes = sort(unique(classes))
	
	bin_matrix <- matrix(0, nrow = nrow(d), ncol = length(classes))
	for(i in 1:nrow(d)) {
		for(j in labs[[i]]) {
			bin_matrix[i,j]=1
		}
	}
	
	if(multilabel.balance) {
		for(i in 1:ncopy) {
			result =  replicate(nrow(d),-1)
			#sample each class
			for(j in classes) {
				population = which(bin_matrix[,j]==1)
				samplesize = ceiling(length(population)/2)
				population = intersect(population,which(result!=0))
				trids <- population[sample(1:length(population), min(samplesize,length(population)))]
				tsids <- setdiff(population,trids)
				result[trids] = 0
				result[tsids] = 1
			}
			partation = d
			partation[,2] = result
			write(t(partation), file = paste("partation",i,".par",sep=""), ncol = 2)
		}
	} else {
		for(i in 1:ncopy) {
			result =  replicate(nrow(d),-1)
			#sample each class
			for(j in classes) {
				population = which(bin_matrix[,j]==1)
				trids <- population[sample(1:length(population), ceiling(length(population)*0.5))]
				tsids <- setdiff(population,trids)
				result[trids] = 0
				result[tsids] = 1
			}
			partation = d
			partation[,2] = result
			write(t(partation), file = paste("partation",i,".par",sep=""), ncol = 2)
		}
	}
	
	
}



unassemble <- function(str.label) {
	return(as.numeric(strsplit(str.label,",")[[1]]))
}
