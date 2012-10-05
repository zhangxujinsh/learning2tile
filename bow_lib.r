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

GenSpbow <- function(spbow, region.all, norm= c("l1", "l2", "none"), dim = 4096) {
	#!!!!not tested!!!!
	#generate the spatial bag of word of the given tiling
	#norm controls whether to normalize the result vector
	N = max(region.all$tiles)+1
	result <- vector(mode="numeric",N*dim)
	for(i in 1:N) {
		for(j in 1:length(tiling)) {
			if(i == (tiling[j]+1)) {
				result[((i-1)*dim+1):(i*dim)] = result[((i-1)*dim+1):(i*dim)] + getRegion(spbow,j,dim)
			}
		}
	}
	if(norm) {
		for(i in 1:N) {
			this.sum = sum(result[((i-1)*dim+1)])
			if(this.sum != 0) {
				result[((i-1)*dim+1):(i*dim)] = result[((i-1)*dim+1):(i*dim)]/sum(result[((i-1)*dim+1):(i*dim)])/N
			}
			else {
				result[((i-1)*dim+1):(i*dim)] = replicate(dim,0)
			}
		}
	}
	return(result)
}