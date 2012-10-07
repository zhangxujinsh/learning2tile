require(Matrix)
require(tseries)

#To test the library using the following dense tile
#txyc_matrix = as.matrix(expand.grid(1:500,1:333))


PlotTiling <- function(txyc_matrix, region.all) {
	width = region.all$width
	height = region.all$height
	windows.options(width=width, height=height)
	colors = rainbow(length(region.all$tiles))
	plot(c(1,width),c(1,height),type="n",xlab="Width", ylab="Height")
	
	for( i in 1:length(region.all$tiles)) {
		lines(txyc_matrix[region.all$tiles[[i]],1:2],type = "p",pch=20,col = colors[i])
	}
}


SaveTilingPlot <- function(txyc_matrix, region.all, save_location) {
	width = region.all$width
	height = region.all$height
	
	png(save_location, width=826*0.6, height=483*0.6, pointsize  = 8)
	windows.options(width=width, height=height)
	colors = rainbow(length(region.all$tiles))
	plot(c(1,width),c(1,height),type="n",xlab="Width", ylab="Height")
	
	for(i in 1:length(region.all$tiles)) {
		lines(txyc_matrix[region.all$tiles[[i]],1:2],type = "p",pch=1,col = colors[i])
	}
	dev.off()
}


TilingRectangle <- function(u, v, txyc_matrix, width, height) {
	m <- txyc_matrix
	widthseq = floor(seq.int(1,width,length.out=u+1))
	heightseq = floor(seq.int(1,height,length.out=v+1))
	
	region.all = list()
	region.all$width = width
	region.all$height = height
	region.all$style = "rectangle"
	region.all$para = c(u,v)
	region.all$tiles = list()
	cnt = 1
	for(i in 1:(length(widthseq)-1)) {
		for(j in 1:(length(heightseq)-1)) {
			region0 = which(m[,1]>=widthseq[i])
			region1 = which(m[,1]<widthseq[i+1])
			region2 = which(m[,2]>=heightseq[j])
			region3 = which(m[,2]<heightseq[j+1])
			region = intersect(intersect(intersect(region0,region1),region2),region3)
			region.all$tiles[[cnt]] <- region
			cnt <- cnt +1		
		}
	}
	return(region.all)
}


TilingDiamond <- function(u, v, txyc_matrix, width, height) {
	m <- txyc_matrix
	#the origin is at the right bottom conner
	#m = as.matrix(expand.grid(1:width,1:height))

	right_lines <- matrix(0, nrow=u+2, ncol=2)	#storing the line equations
	left_lines <- matrix(0, nrow=v+2, ncol=2) 	#storing the line equations
	
	u_intersec_pots <- (ceiling((u-1)/2)+1)
	v_intersec_pots <- (ceiling((v-1)/2)+1)
	
	right_lines[1,1] = -1*height/width			#k
	right_lines[1,2] = 0						#b
	i=2
	cnt = 1
	while( i < floor((u+2)/2+1)) {
		right_lines[i,1] = -1*height/width
		right_lines[i,2] = height/u_intersec_pots*cnt
		i = i + 1
		cnt = cnt + 1
	}
	if(u%%2 != 0) {
		#odd
		right_lines[i,1] = -1*height/width
		right_lines[i,2] = height
		i = i + 1
	}
	cnt = 1
	while(i < u+2) {
		right_lines[i,1] = -1*height/width
		right_lines[i,2] = height*((u_intersec_pots+1)/u_intersec_pots)*cnt
		i = i + 1
		cnt = cnt + 1
	}
	right_lines[u+2,1] = -1*height/width		#k
	right_lines[u+2,2] = 2*height 				#b
	

	left_lines[1,1] = height/width			#k
	left_lines[1,2] = -1*height				#b
	
	i=2
	cnt = 1
	while( i < floor((v+2)/2+1)) {
		left_lines[i,1] = height/width
		left_lines[i,2] = -1* height/v_intersec_pots*cnt
		i = i + 1
		cnt = cnt + 1
	}
	if(v%%2 != 0) {
		#odd
		left_lines[i,1] = height/width
		left_lines[i,2] = 0
		i = i + 1
	}
	cnt = 1
	while(i < v+2) {
		left_lines[i,1] = height/width
		left_lines[i,2] = height/v_intersec_pots*cnt
		i = i + 1
		cnt = cnt + 1
	}
	
	left_lines[v+2,1] = height/width		#k
	left_lines[v+2,2] = height				#b
	
	right_lines
	left_lines
	
	
	region.all = list()
	region.all$width = width
	region.all$height = height
	region.all$style = "diamond"
	region.all$para = c(u,v)
	region.all$tiles = list()
	
	cnt = 1
	for(i in 1:(nrow(right_lines)-1)) {
		for(j in 1:(nrow(left_lines)-1)) {
			region0 = which(right_lines[i,1]*m[,1]+right_lines[i,2] - m[,2] < 0)
			region1 = which(right_lines[i+1,1]*m[,1]+right_lines[i+1,2] - m[,2] >= 0)
			region2 = which(left_lines[j,1]*m[,1]+left_lines[j,2] -m[,2] < 0)
			region3 = which(left_lines[j+1,1]*m[,1]+left_lines[j+1,2] -m[,2] >= 0)
			region = intersect(intersect(intersect(region0,region1),region2),region3)
			region.all$tiles[[cnt]] <- region
			cnt <- cnt +1
		}
	}
	
	return(region.all)
}



Hexfun <- function(x, y, l) {
	lfun <- matrix(0, nrow=6, ncol=2)
	#l1
	lfun[1,1] = sqrt(3)
	lfun[1,2] = y - sqrt(3)*x - sqrt(3)/2*l
	#l2
	lfun[2,1] = 0
	lfun[2,2] = y
	#l3
	lfun[3,1] = -1*sqrt(3)
	lfun[3,2] = sqrt(3)*x + 3*sqrt(3)/2*l + y
	#l4
	lfun[4,1] = sqrt(3)
	lfun[4,2] = -1*sqrt(3)*x - 5*sqrt(3)/2*l + y
	#l5
	lfun[5,1] = 0
	lfun[5,2] = y - sqrt(3)*l
	#l6
	lfun[6,1] = -1*sqrt(3)
	lfun[6,2] = sqrt(3)*x + y - sqrt(3)/2*l
	return(lfun)
}


TilingHexagon <- function(u, txyc_matrix, width, height) {
	#insect eyes
	m <- txyc_matrix	
	#calculating the hexagon height
	hex_edge_len <- ceiling(height/(u*sqrt(3)))
	
	#finding the left coner (relative origin) for all hexagons
	region.all = list()
	region.all$width = width
	region.all$height = height
	region.all$style = "hexagon"
	region.all$para = u
	region.all$tiles = list()
	
	cnt = 1
	for(i in 1:(2*u+1)) {
		j = 1
		cur_x = 0
		cur_y = hex_edge_len*i*sqrt(3)/2
		while(TRUE) {
			if(i%%2 != 0) {
				#odd
				cur_x = (3*j-9/2) * hex_edge_len
			} else {
				#even
				cur_x =  (3*j-3) * hex_edge_len
			}
			if(cur_x > width) break
			lfun = Hexfun(cur_x,cur_y,hex_edge_len)
			region1 = which(lfun[1,1]*m[,1]+lfun[1,2] >= m[,2])
			region2 = which(lfun[2,1]*m[,1]+lfun[2,2] >= m[,2])
			region3 = which(lfun[3,1]*m[,1]+lfun[3,2] > m[,2])
			region4 = which(lfun[4,1]*m[,1]+lfun[4,2] < m[,2])
			region5 = which(lfun[5,1]*m[,1]+lfun[5,2] < m[,2])
			region6 = which(lfun[6,1]*m[,1]+lfun[6,2] <= m[,2])
			region = intersect(intersect(intersect(intersect(intersect(region1,region2),region3),region4),region5),region6)
			region.all$tiles[[cnt]] <- region
			cnt = cnt + 1
			j = j + 1
		}
	}
	return(region.all)
}


TilingCamera <- function(scalepara=0.5, txyc_matrix, width, height) {
	m <- txyc_matrix
	f1 = m[,2]-(height/width)*m[,1]
	f2 = m[,2]+(height/width)*m[,1]-height
	f3 = (m[,1]-width/2)^2/(scalepara*width)^2 + (m[,2]-height/2)^2/(scalepara*height)^2
	
	region.all = list()
	region.all$width = width
	region.all$height = height
	region.all$style = "camera"
	region.all$para = scalepara
	region.all$tiles = list()
	
	region4 = which(f3<1)
	region0 = setdiff(intersect(which(f1>0),which(f2<0)),region4)
	region1 = setdiff(intersect(which(f1>0),which(f2>0)),region4)
	region2 = setdiff(intersect(which(f1<0),which(f2>0)),region4)
	region3 = setdiff(intersect(which(f1<0),which(f2<0)),region4)
	region.all$tiles[[1]] <- region0
	region.all$tiles[[2]] <- region1
	region.all$tiles[[3]] <- region2
	region.all$tiles[[4]] <- region3
	region.all$tiles[[5]] <- region4
	return(region.all)
}


TilingMembership <- function(region.all, membership_vector) {
	n = max(membership_vector)
	result = region.all
	result$membership = membership_vector
	result$tiles = list()	
	
	for(i in 0:n) {
		#for each tile member
		tile.member.indexes = which(membership_vector==i)
		thisvector= vector(mode="numeric",length=0)
		for(j in tile.member.indexes) {
			 thisvector = union(thisvector, region.all$tiles[[j]])
		}
		result$tiles[[(i+1)]] <- thisvector
	}
	return(result)
}



Project2AUniformSize <- function(txyc_matrix, owidth, oheight, pwidth = 500, pheight = 333) {
	#project the txyc of any keyframe to an predefined uniform keyframe.
	widthscale <- pwidth/owidth
	heightscale <- pheight/oheight
	result <- txyc_matrix
	result[,1] = round(txyc_matrix[,1] * widthscale) #projected width
	result[,2] = round(txyc_matrix[,2] * heightscale) #projected height
	return(result)
}




