########################################
## Date: 02/17/2016
## Zack W. Almquist
## Assistant Professor
## Department of Sociology 
## School of Statistics
## University of Minnesota
########################################

## Data from
#https://snap.stanford.edu/data/com-DBLP.html
#https://snap.stanford.edu/data/bigdata/communities/com-dblp.top5000.cmty.txt.gz

library("R.utils")
library(sna)
	address<-"https://snap.stanford.edu/data/bigdata/communities/"
	zipFile<-"com-dblp.top5000.cmty.txt.gz"
	fileName<-paste(address,zipFile,sep="") ##full address of zip file
	zipdir <- tempfile() ### Create temp file
	dir.create(zipdir) ### Create a folder in the temp file
	## Download the zip file
	download.file(fileName,destfile=paste(zipdir,zipFile,sep="/")) 
	### Extract the zip file
	gunzip(paste(zipdir,zipFile,sep="/"), exdir=zipdir) 
	## Get the name (assumes it is the second file after the zip)
	files <- list.files(zipdir)  
	### read in the file and output data.frame
mylist <- strsplit(readLines(paste(zipdir,"com-dblp.top5000.cmty.txt",sep="/")), "\t")
mymat<-lapply(mylist,function(x){temp<-x[2:length(x)];cbind(rep(x[1],length(temp)),temp)})
el<-do.call("rbind",mymat)
el<-el[1:floor(.01*NROW(el)),]
head(el)

vnames<-unique(c(el))
m1<-match(el[,1],vnames)
m2<-match(el[,2],vnames)
el_sna<-cbind(m1,m2)
attr(el_sna,"n")<-length(vnames)
attr(el_sna,"vnames")<-vnames

pdf("coauthorship.pdf")
par(mar=c(0, 0, 1, 0) + 0.1)
gplot(el_sna)
title("1% of the DBLP co-authorship network communities (Top 5,000)")
dev.off()