require(Matrix)
require(tseries)


RandomSplit(labelfile, ncopy = 10) {
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
