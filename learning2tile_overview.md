## User Guide ##
This article introduces how to use the code to learn promising tilings from a dataset.

### 1) Install R ###

Download and install R from http://www.r-project.org/
  * For more information on Windows see http://cran.r-project.org/bin/windows/base/
  * Ubuntu Linux see http://help.nceas.ucsb.edu/installing_r_on_ubuntu

### 2) Download the source archive ###


**Note the following package only contains the feature for illustration, not the features used in our experiments!**
  1. Download the example archive from [here](https://code.google.com/p/learning2tile/downloads/detail?name=learning2tile_v1.1_example.zip&can=2&q=#makechanges) which comes with a toy example.
  1. Unzip the archive in a folder.
    * rectangle\_mask\_4x4 is the pre-computed mask file each for an image.
    * The file name ending with ".r" and ".java" are source files.
    * "labels.labels" is the class label file.
    * "partition[1-5].par" are the training and test partition used in the cross validation.



### 3) Execute the code ###

#### On a Windows machine ####
  1. Double click the file ".RData".
  1. Type the command and follow the instructions to download the dependent libraries.
> > ```R

install.packages("gplots")
install.packages("Matrix")
install.packages("tseries")
```
  1. Type the command in the opened console.
> > ```R

#load the source packages
source("http://learning2tile.googlecode.com/svn/trunk/costfun_lib.r")
source("http://learning2tile.googlecode.com/svn/trunk/tiling_lib.r")
source("http://learning2tile.googlecode.com/svn/trunk/bow_lib.r")
############ Mask = Rectangle_4x4 ####################
spbowdir <- "rectangle_mask_4x4\\"
report_filename <- "rectangle_4x4_report.pdf"   #name of the final report
para_lambda <- 0.0012    #the regularization parameter in the cost function
t1 <- proc.time()
#the fold number in cross validation
foldnum = 5
#the number of centroids in clustering = the size of the vocabulary
DIM=1024
#load the cross validation partition
partations <- list()
for(i in 1:foldnum) {
partations[[i]] <- read.table(paste("partation",i,".par",sep=""))
}
#load the label file
label.table <- read.table("labels.labels")
label.binmatrix <- ToBinaryLabel(label.table)
#load the spbow (mask tilings)
spbow.list = list()
for(i in 1:nrow(label.table)) {
spbow.list[[i]] = as.vector(read.matrix(paste(spbowdir, label.table[i,1],".spbow",sep="")))
}
#load the condidate tilings
tilingfuns <- read.matrix("http://learning2tile.googlecode.com/svn/wiki/tilingfuns/rectangle_4x4_equal_tiling.txt")
t2 <- proc.time()
#compute the fitness (minus cost) for each candidate tiling
fitness <- FindBestFSTiling(spbow.list, partations, label.binmatrix, tilingfuns, dim=DIM, lambda = para_lambda)
t3 <- proc.time()
#generate the final pdf report
report <- GenTilingReport(tilingfuns,fitness, tiling_style = "square", tiling_paras=c(4,4), save.filename=report_filename, lambda = para_lambda)
#print the run-time information
print(paste("Learning finished. Please find the learned tilings in ", report_filename, ".", sep=""))
print(paste("Run time : ", t3[1]-t1[1], "s", sep=""))
print(paste("I/O time : ", t2[1]-t1[1], "s", sep=""))
print(paste("Learning time : ", t3[1]-t2[1], "s", sep=""))
```
  1. Once finished, it will generates a report **[(preview\_link)](https://docs.google.com/viewer?url=http%3A%2F%2Flearning2tile.googlecode.com%2Fsvn%2Fwiki%2Fdataset%2F15scene%2Frectangle_4x4_report.pdf)** and print the running time for both I/O and learning process.
    * The first page presents the configuration information of the task.
    * The second page lists the estimated promising tilings with different number of tiles. You may select the one with the maximum fitness score or try all of them.
    * In the following pages, the promising tilings are plotted with its name printed on the top.
  1. The learning to tile is finished and read here to see how to generate the final Spatial Bag-of-Word representation using the script.

#### On a Linux machine ####

  1. Open the R console, where "/data/MM2/workspace/R" is the folder where you installed R.
> > `/data/MM2/workspace/R/bin/R`
  1. Change the directory where you unziped and stored the example archive [here](https://code.google.com/p/learning2tile/downloads/detail?name=learning2tile_v1.1_example.zip&can=2&q=#makechanges). Here say it is in "/data/MM2/temp".
> > ```R
setwd("/data/MM2/temp")```
  1. Same as STEP 3-6 on Windows.

**Note the results are learned tilings from the given mask. The fusion of them yields the best results.**

### Discussion ###

Add your content here.  Format your content with:
  * Text in **bold** or _italic_
  * Headings, paragraphs, and lists
  * Automatic links to other wiki pages