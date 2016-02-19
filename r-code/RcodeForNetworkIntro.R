########################################
## Date: 02/17/2016
## Zack W. Almquist
## Assistant Professor
## Department of Sociology 
## School of Statistics
## University of Minnesota
########################################


#########################################
## R Code for the Slide Deck
#########################################

## From CRAN
## install.packages("packagename")

## statnet
suppressPackageStartupMessages(library(ergm))
suppressPackageStartupMessages(library(sna))
suppressPackageStartupMessages(library(network))

## Other packages
library(devtools)
library(ggplot2)
library(gridExtra)
library(xtable)


## From github
## 
## library(devtools)
## install_github("zalmquist/networkdata")
## install_github("zalmquist/netowrkMethods")
library(networkMethods)
library(networkdata)


##################################
### Network Data 
### and Network Plots examples
##################################

data(mids_1993)
gplot(mids_1993, 
boxed.labels=FALSE, 
label.cex=0.5, 
edge.col=rgb(0,0,0,.4),
label=network.vertex.names(mids_1993),
displayisolates=FALSE)  
title("1993 militarized interstate disputes (MIDs)",sub="Correlates of War Project")

load("data/IRSmig01.rda")
### Build Match between IRS and US County data
diag(IRSmig01)<-0
s<-quantile(as.vector(IRSmig01)[as.vector(IRSmig01)>0],probs = seq(.9,.99,.01))
IRSmig01[IRSmig01<s["99%"]]<-0
IRSmig01[IRSmig01>=s["99%"]]<-1
gplot(IRSmig01,displayisolates=FALSE,edge.col=rgb(0,0,0,.1))
title("IRS Migration Data, 2000-2001",sub="Threshold at 99%")
rm(IRSmig01)

data(emon)   #Load the emon data set
#Plot the EMONs
par(mfrow=c(2,4),mar=c(0, 0, .6, 0) + 0.1)
for(i in 1:length(emon)){
  plot(emon[[i]],edge.lwd="Frequency",edge.col=rgb(0,0,0,.4))
	title(main=names(emon)[i],outer=FALSE)
}


load("data/relevent_sunbelt_2014.Rdata")
WTCPoliceNet <- as.sociomatrix.eventlist(WTCPoliceCalls,37) 
gplot(WTCPoliceNet,edge.lwd=WTCPoliceNet^0.75, vertex.col=2+WTCPoliceIsICR,vertex.cex=1.25,edge.col=rgb(0,0,0,.5))
title(main="Data set coded by Butts et al. (2007)")



data(krack)
kfr.conc<-consensus(krack$krackfr,method="romney.batchelder",verbose=FALSE)
##Plot
gplot(kfr.conc,displaylabels=TRUE)

load("data/rdNets.rda")

index.nna<-sapply(rdNets,is.network)
timeAgg<-rdNets[[1]][,]
for(i in 2:length(rdNets)){
	if(index.nna[i]){
		timeAgg<-timeAgg+rdNets[[i]][,]
	}
}

ec<-matrix(rgb(0,0,0,timeAgg/sum(timeAgg)*150),nc=network.size(rdNets[[1]]),nr=network.size(rdNets[[1]]))
cv<-rep(NA,network.size(rdNets[[1]]))
cv[(rdNets[[1]]%v%"dnc")==1]<-"blue"
cv[(rdNets[[1]]%v%"rnc")==1]<-"red"
gplot(timeAgg,edge.col=ec,vertex.col=cv,vertex.cex=.7)
title("2004 RNC/DNC Credentialed Blogs Citation Network")


data(faux.desert.high)
plot(faux.desert.high,vertex.col="grade",edge.col=rgb(0,0,0,.3),displayisolates=FALSE)


data(emon)
i<-4
plot(emon[[i]],
main=names(emon)[i],
edge.lwd="Frequency",
displayisolates=FALSE, 
vertex.col=rainbow(8),displaylabels=FALSE)
legend("bottomright",legend=(emon[[i]]%v%"vertex.names")[degree(emon[[i]])!=0],pch=19,col=rainbow(8),bty="n",cex=.7)


sf<-function(x){x}
mat<-emon[[i]][degree(emon[[i]])!=0,degree(emon[[i]])!=0]
col<-rainbow(8)
nam<-paste("\\color[HTML]{",substr(col,2,7),"}","{",1:8,"}",sep="")
rownames(mat)<-colnames(mat)<-nam
print(xtable(mat,caption="Mt. Si SAR EMON, Confirmed Ties"),size="tiny",sanitize.rownames.function=sf,
sanitize.colnames.function=sf)


load("data/topic.Rda")
net<-network(topic,bipartite=TRUE)
col<-rep(NA,network.size(net))
col[1:NROW(topic)]<-"blue"
col[(NROW(topic)+1):length(col)]<-"red"

plot(net,label="vertex.names",label.cex=.4, 
edge.col=rgb(0,0,0,.2), 
vertex.col=col)

## Save as png
png(filename = "umnDept.png", width = 2*480, height = 2*480)
par(mar=c(0, 0, 0, 0) + 0.1)
plot(net,label="vertex.names",label.cex=.9, 
edge.col=rgb(0,0,0,.3), 
vertex.col=col,vertex.cex=.5)
dev.off()

#####################
## NLIs
#####################

star<-network(cbind(rep(1,7), matrix(rep(0,times=6,each=7), nc=6,nr=7)), directed=FALSE)

plot(star,main="Undirected Star", label=1:7, edge.col=rgb(0,0,0,.3))

set.seed(9902211)
gplot(net<-rgraph(5),main="directed",label=1:5,edge.col=rgb(0,0,0,.3))


set.seed(9902211)
gplot(symmetrize(net[,]),main="directed", label=1:5,edge.col=rgb(0,0,0,.3))
sna::geodist(symmetrize(net[,]))

set.seed(9902211)
plot(star,main="Undirected Star", label=1:7, edge.col=rgb(0,0,0,.3))
gplot(symmetrize(net[,]),main="directed", label=1:5,edge.col=rgb(0,0,0,.3))

#################
## Extra R-Code for Centrality
#################

data(mids_1993)
data(contig_1993)
#
#Basic centrality indices: degree, betweenness, and closeness-------------------
#
# We begin with the simplest case: degree
degree(mids_1993)                                        # Default: total degree
ideg <- degree(mids_1993, cmode="indegree")              # Indegree for MIDs
odeg <- degree(mids_1993, cmode="outdegree")             # Outdegree for MIDs
all(degree(mids_1993) == ideg+odeg)                      # In + out = total?

# Once centrality scores are computed, we can handle them using standard R
# methods:
plot(ideg, odeg, type="n", xlab="Incoming MIDs", ylab="Outgoing MIDs")
abline(0, 1, lty=3)
text(jitter(ideg), jitter(odeg), network.vertex.names(contig_1993), cex=0.75,
     col=2)   #Plot index by odeg


#Plot simple histograms of the degree distribution:
pdf("simpleHistIndOut.pdf")
par(mfrow=c(2,2))                                       # Set up a 2x2 display
hist(ideg, xlab="Indegree", main="Indegree Distribution", prob=TRUE)
hist(odeg, xlab="Outdegree", main="Outdegree Distribution", prob=TRUE)
hist(ideg+odeg, xlab="Total Degree", main="Total Degree Distribution",
     prob=TRUE)
dev.off()



# Centrality scores can also be used with other sna routines, e.g., gplot
gplot(mids_1993, vertex.cex=(ideg+odeg)^0.5/2, vertex.sides=50,
      boxed.labels=FALSE,label.cex=0.4,
      vertex.col=rgb(odeg/max(odeg),0,ideg/max(ideg)),
      label=network.vertex.names(mids_1993))

# Betweenness and closeness are also popular measures
bet <- betweenness(contig_1993, gmode="graph")       # Geographic betweenness
bet
gplot(contig_1993, vertex.cex=sqrt(bet)/25, gmode="graph")   # Use w/gplot
clo <- closeness(contig_1993)                        # Geographic closeness
clo                                                  # A large world after all?

# Can use sna routines to explore alternatives to the common measures....
closeness2 <- function(x){            # Create an alternate closeness function!
  geo <- 1/geodist(x)$gdist         # Get the matrix of 1/geodesic distance
  diag(geo) <- 0                    # Define self-ties as 0
  apply(geo, 1, sum)                # Return sum(1/geodist) for each vertex
}
clo2 <- closeness2(contig_1993)       # Use our new function on contiguity data
hist(clo2, xlab="Alt. Closeness", prob=TRUE)    # Much better behaved!
cor(clo2, bet)                                  # Correlate with betweenness
plot(clo2, bet)                            # Plot the bivariate relationship

#For more information....
?betweenness
?bonpow
?closeness
?degree
?evcent
?graphcent
?infocent
?prestige
?stresscent
#################
## Extra R-Code for Centrality
#################


#####################
## GLIs
#####################
undirected<-rgraph(10,mode="graph")
directed<-rgraph(10,mode="digraph")
gden(undirected,mode="graph")
2*sum(undirected[upper.tri(undirected)])/(NROW(directed)*(NROW(directed)-1))
gden(directed,mode="digraph")
sum(directed)/(NROW(directed)*(NROW(directed)-1))


#####################
## Density compared to mean degree for the add health networks
#####################
data(addhealth)
data<-data.frame(size=sapply(addhealth,network.size),
density=sapply(addhealth,gden))
data$meandegree<-data$density*(data$size-1)

p1<-ggplot(data, aes(size,density)) + geom_point() + geom_smooth()
p2<-ggplot(data, aes(size,meandegree)) + geom_point() + geom_smooth()
grid.arrange(p1, p2, ncol=1)


#####################
## Visualization of Mutual, Asymmetric and Null dyads
#####################
asym<-matrix(0,nc=4,nr=4)
asym[1,4]<-1
asym[3,2]<-1
mut<-matrix(c(0,1,1,0),nc=2)
null<-matrix(c(0,0,0,0),nc=2)
par(mfrow=c(1,3))
gplot(mut,mode="circle",main="Mutual")
box()
gplot(asym,mode="circle",main="Asymmetric")
box()
gplot(null,mode="circle",main="Null")
box()

#####################
## Example of reciprocity measure
#####################
graph<-rgraph(15)
gplot(graph)
dyad.census(graph)
grecip(graph,measure ="edgewise")
grecip(graph,measure ="dyadic")


#####################
## Example of hierarchy measure
#####################
graph<-rgraph(15,tprob=.1)
gplot(graph)
hierarchy(graph,measure ="reciprocity")
hierarchy(graph,measure ="krackhardt")


#####################
## Example of centralization measures
#####################
graph<-rgraph(15,tprob=.5,mode="graph")
star<-matrix(0,nc=10,nr=10); star[1,]<-1; star[,1]<-1

par(mfrow=c(1,2),mar=c(0, 0, 0, 0))
gplot(graph)
gplot(star)

cbind(centralization(graph,g=1,degree),centralization(graph,g=1,closeness))
cbind(centralization(star,g=1,degree),centralization(star,g=1,closeness))

#####################
## Example of dyad census
#####################
g<-rgraph(10)
gplot(g)
dyad.census(g)

#####################
## Example of triad census
#####################
g<-rgraph(10,mode="graph")
gplot(g)
triad.census(g)
triad.classify(g)

g<-rgraph(10)
gplot(g)
triad.census(g)


#####################
## Examples of CUG Tests
#####################

## By hand
baselineDensity<-sapply(1:1000,function(x){gden(rgraph(20,mode="graph"))})
save<-hist(baselineDensity,
main="Baseline Density Distribution, N=20",
xlab="Density",probability=TRUE,breaks=10)

qt<-quantile(baselineDensity,prob=seq(0,1,.025))[c("2.5%","10%","90%","97.5%")]
col<-rep(rgb(0,1,0,1),length(save$breaks))
col[save$breaks<=qt[1]]<-col[save$breaks>=qt[4]]<-rgb(1,0,0,1)
col[save$breaks<=qt[2] & save$breaks>qt[1]]<-col[save$breaks>=qt[3] & save$breaks<qt[4] ]<-rgb(1,0,0,.7)
hist(baselineDensity, main="Baseline Density Distribution, N=20", xlab="Density",probability=TRUE,breaks=10,col=col)


## using cug.test function in sna
args(cug.test)

data(trade)
cden<-cug.test(trade$MANUFACTURED_GOODS,gden)
cden
plot(cden)


crecip<-cug.test(trade$MANUFACTURED_GOODS,grecip,cmode="edges")
crecip
plot(crecip)

ctrans<-cug.test(trade$MANUFACTURED_GOODS,gtrans,cmode="dyad")
ctrans
plot(ctrans)

c_cent_ind<-cug.test(trade$MANUFACTURED_GOODS ,centralization,cmode="dyad", FUN.arg=list(FUN=degree,cmode="indegree"))
c_cent_ind
plot(c_cent_ind)



