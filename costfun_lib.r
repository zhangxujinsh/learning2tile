require(gplots)

JSNorm <- function(d1,d2) {
	#presudo count
	#necessary to avoid Inf
	d1 <- d1 + 1
	d2 <- d2 + 1
	#normalize
	d1 <- d1/sum(d1)
	d2 <- d2/sum(d2)
	d <- (d1+d2)/2
	result = 0.5*sum(d1*log(d1/d)) + 0.5*sum(d2*log(d2/d))
	return(result)
}

GetLabelHist <- function(spbow.list, binary.label.matrix) {
	#get the sum histgram for each label
	#spbow.list is a list of spbow (each element corresponds to a file)
	#binary.label.matrix is a binary label matrix
	#return a set of histgram for each label 
	l = binary.label.matrix
	label.hist = list()
	hist.dim = length(spbow.list[[1]])
	
	for(i in 1:ncol(l)) {
		this.label.hist = vector(length=hist.dim, mode = "numeric")
		for(j in which(l[,i]==1)) {
			this.label.hist = this.label.hist + spbow.list[[j]]
		}
		label.hist[[i]] = this.label.hist
	}
	return(label.hist)
}

FitnessClass <- function(pos.hist, neg.hist, dim) {
	#Given a specific tiling and a class return its fitness value
	numoftile = length(pos.hist)/dim
	if(numoftile != round(numoftile)) {
		print("dim is incorectin FitnessEvent!")
		return(0)
	}
	fitness = 0
	for(i in 1:numoftile) {
		this.tile.posbow = GetTilingRegion(pos.hist, i, dim)
		this.tile.negbow = GetTilingRegion(neg.hist, i, dim)
		fitness = fitness + JSNorm(this.tile.posbow, this.tile.negbow)
	}
	fitness = fitness/numoftile
	return(fitness)
}


FitnessAllClasses <- function(pos.hist.list, dim) {
	#a list of positive hisgram for each class
	fitness = 0.0
	numofclasses <- length(pos.hist.list)
	hist.length <- length(pos.hist.list[[1]])
	
	for(i in 1:numofclasses) {
		#for each class
		this.class.posspow <- pos.hist.list[[i]]
		#approximation for the real value of negspbow for the sake of efficiency
		this.class.negspow <- vector(length=hist.length , mode = "numeric")
		for(j in setdiff(1:numofclasses,i)) {
			this.class.negspow = this.class.negspow + pos.hist.list[[j]]
		}
		#norm
		this.class.posspow = this.class.posspow
		this.class.negspow = this.class.negspow / (numofclasses-1)
		
		fitness = fitness + FitnessClass(this.class.posspow, this.class.negspow, dim)
	}
	fitness = fitness/numofclasses
	return(fitness)
}

OutputFitness <- function(fitness, tilingfuns, outfile) {
	tiling_names <- vector(mode="character", lengt=nrow(tilingfuns))
	for(i in 1:nrow(tilingfuns)) {
		tiling_names[i] = TilingMembership2String(tilingfuns[i,])
	}
	write(t(cbind(tiling_names,fitness)), file = outfile, ncol=2)
}


FindBestFSTiling <- function(spbow.list, partations, label.binary, tiling.functions, dim=1024) {
	foldnum <- length(partations)
	fitness = vector(mode="numeric", length=nrow(tiling.functions))
	
	for(i in 1:foldnum) {
		tr.spbow.list <- list()
		tr.ids <- which(partations[[i]][,2]==0)
		for(j in 1:length(tr.ids)) {
			tr.spbow.list[[j]] = spbow.list[[tr.ids[j]]]
		}
		
		#for each partation
		label.hist <- GetLabelHist(tr.spbow.list,label.binary[tr.ids,])
		
		for(k in 1:nrow(tiling.functions)) {
			transformed.spbow <- list()
			for(i in 1:length(label.hist)) {
				transformed.spbow[[i]] = TransformSpbow(label.hist[[i]],tiling.functions[k,],"null",dim) 
			}
			fitness[k] = fitness[k] + FitnessAllClasses(transformed.spbow,dim)
		}
	}
	fitness = fitness/foldnum
	return(fitness)
}


ToBinaryLabel <- function(l) {
	labs <- list()
	classes <- c()
	for(i in 1:nrow(l)) {
		labs[[i]] = unassemble(as.character(l[i,2]))
		classes = union(classes, labs[[i]])
	}
	classes = sort(unique(classes))
	
	bin_matrix <- matrix(0, nrow = nrow(l), ncol = length(classes))
	for(i in 1:nrow(l)) {
		for(j in labs[[i]]) {
			bin_matrix[i,j]=1
		}
	}
	return(bin_matrix)
}

unassemble <- function(str.label) {
	return(as.numeric(strsplit(str.label,",")[[1]]))
}

GenTilingReport <- function(tiling_functions, fitness, tiling_style=NULL, tiling_paras=NULL, save.filename=NULL) {
	numtiles = apply(tiling_functions,1,max)+1		#number of the tiles
	report = matrix(nrow = length(unique(numtiles)), ncol = 3)
	
	cnt = 1
	for(i in unique(numtiles)) {
		this.level.ids <- which(numtiles == i)
		best.id <- which(fitness[this.level.ids] == max(fitness[this.level.ids]))[1]
		best.id <- this.level.ids[best.id]
		report[cnt,3] = fitness[best.id]
		report[cnt,2] = i
		report[cnt,1] = TilingMembership2String(tiling_functions[best.id,])
		cnt = cnt + 1
	}
	colnames(report) <-  c("tiling", "num_of_tiles", "fitness")
	if(!is.null(save.filename)) {
		pdf(save.filename)
		info <- matrix(nrow=6)
		colnames(info) <- "https://code.google.com/p/learning2tile/"
		info[1,] = "################################################################"
		info[2,] = "Configuration Information:"
		info[3,] = paste("TILING_STYLE =",tiling_style)
		info[4,] = paste("TILING_PARA =", paste(tiling_paras, collapse=" ", sep = " "))
		info[5,] = "See the following page for the learnt tiling functions"
		info[6,] = "################################################################"
		
		textplot(info, show.rownames = FALSE)
		textplot(report, show.rownames = FALSE)
		txyc_matrix = as.matrix(expand.grid(1:150,1:100))
		if(tiling_style == "square") {
			base.screen <- TilingRectangle(tiling_paras[1],tiling_paras[2], txyc_matrix, 150, 100)
		} else if(tiling_style == "diamond") {
			base.screen <- TilingDiamond(tiling_paras[1],tiling_paras[2], txyc_matrix, 150, 100)
		} else if(tiling_style == "camera") {
			base.screen <- TilingCamera(tiling_paras, txyc_matrix, 150, 100)
		} else if(tiling_style == "hexagon") {
			base.screen <- TilingHexagon(tiling_paras, txyc_matrix, 150, 100)
		}
		
		#draw each tiling
		for(i in 1:nrow(report)) {
			tiling_membership = String2TilingMembership(report[i,1])
			mytiling = TilingMembership(base.screen, tiling_membership)
			PlotTiling(txyc_matrix, mytiling)
			title(report[i,1])
		}
		dev.off()		
	
	}
	return(report)
}














