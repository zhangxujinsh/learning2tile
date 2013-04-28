require(Matrix)
require(tseries)


SoftMembership <- function(txyc_matrix, para = c(1,0,0,0,0,0,0,0,0,0), dim=4096) {
	#default method is the hard-membership method
	#the column of this matirx is 12
	
	if(is.null(para) || length(para) != 10) {
		print("Need parameter in distance rank: 10 dims weights")
		quit()
	}
	
	final.vector <- vector(mode = "numeric",dim)
	m <- txyc_matrix
	if(is.vector(m)) {
		m <- matrix(m, 1, 12)
	}

	for(i in 1:nrow(m)) {
		final.vector[m[i,3:12]] = final.vector[m[i,3:12]] + para
	}
	return(final.vector)
}


SoftMembership.MaxPooling <- function(txyc_matrix, para = c(1,0,0,0,0,0,0,0,0,0), dim=4096) {
	#default method is the hard-membership method
	#the column of this matirx is 12
	
	if(is.null(para) || length(para) != 10) {
		print("Need parameter in distance rank: 10 dims weights")
		quit()
	}
	
	final.vector <- vector(mode = "numeric",dim)
	m <- txyc_matrix
	if(is.vector(m)) {
		m <- matrix(m, 1, 12)
	}

	for(i in 1:nrow(m)) {
		final.vector[m[i,3:12]] = pmax(final.vector[m[i,3:12]], para)
	}
	return(final.vector)
}


GenSpbow.MaxPooling <- function(txyc_matrix, region.all, norm= c("l1", "l2", "none"), para = c(1,0,0,0,0,0,0,0,0,0), dim = 4096) {
	file.bow <- vector(mode="numeric",length=0)
	for(k in 1:length(region.all$tiles)) {
		if(length(region.all$tiles[[k]])==0) {
			this.bow <- replicate(dim,0)
		} else {
			this.bow <- SoftMembership.MaxPooling(txyc_matrix[region.all$tiles[[k]],], para, dim)
			if(norm == "l1")
				this.bow <- L1normalize(this.bow)
			else if(norm == "l2") {
				this.bow <- L2normalize(this.bow)
			} 
		}
		file.bow <- c(file.bow, this.bow)
	}
	return(file.bow)
}




GenSpbow <- function(txyc_matrix, region.all, norm= c("l1", "l2", "none"), para = c(1,0,0,0,0,0,0,0,0,0), dim = 4096) {
	file.bow <- vector(mode="numeric",length=0)
	for(k in 1:length(region.all$tiles)) {
		if(length(region.all$tiles[[k]])==0) {
			this.bow <- replicate(dim,0)
		} else {
			this.bow <- SoftMembership(txyc_matrix[region.all$tiles[[k]],], para, dim)
			if(norm == "l1")
				this.bow <- L1normalize(this.bow)
			else if(norm == "l2") {
				this.bow <- L2normalize(this.bow)
			} 
		}
		file.bow <- c(file.bow, this.bow)
	}
	return(file.bow)
}



TransformSpbow <- function(spbow, tiling_membership, norm = c("l1", "l2", "none"), dim = 4096) {
	#note the input spbow should not be normalized
	
	N = max(tiling_membership)+1
	result <- vector(mode="numeric", N*dim)
	
	for(i in 1:N) {
		normdivider <- length(which(tiling_membership==(i-1)))
		for(j in 1:length(tiling_membership)) {
			if(i == (tiling_membership[j]+1)) {
				result[((i-1)*dim+1):(i*dim)] = result[((i-1)*dim+1):(i*dim)] + GetTilingRegion(spbow,j,dim)
			}
		}
		if(norm == "l1") {
				result[((i-1)*dim+1):(i*dim)] <- L1normalize(result[((i-1)*dim+1):(i*dim)])/N
		}	else if(norm == "l2") {
				result[((i-1)*dim+1):(i*dim)] <- L2normalize(result[((i-1)*dim+1):(i*dim)])/N
		} 
		
	}
	return(result)
}




L1normalize <- function(row_vector) {
	divider = sum(row_vector)
	if(divider == 0) {
		return(row_vector)
	}
	else {
		return(row_vector/divider)
	}	
}


L2normalize <- function(row_vector) {
	divider = sqrt(sum(row_vector^2))
	if (divider == 0) {
		return(row_vector)
	}
	else {
		return(row_vector/divider)
	}	
}


GetTilingRegion <- function(spbow, regionID,dim=4096) {
	#indexing a region from a spatial BoW using the following ID
	#indexing:
	#|--------------
	#|    |    |    |
	#| 3  | 6  | 9  |
	#|--------------
	#|    |    |    |
	#| 2  | 5  | 8  |
	#|--------------
	#|    |    |    |
	#| 1  | 4  | 7  |
	#|--------------
	return(spbow[((regionID-1)*dim+1):(regionID*dim)])
}