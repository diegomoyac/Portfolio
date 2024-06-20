##Case 3##

#download the data
setwd("~/Fall/Fall B/Analytics design and application/Case 3")
surveyData = read.csv('case3_datafile.csv')

kmeansColumns = surveyData[,2:12] #chose columns for k-means

## consider normalizing scale of data
kmeansColumnsNorm = apply(kmeansColumns,2,function(x) {(x - mean(x))/sd(x)})

nClust = 4 #set number of clusters
set.seed(123456)   #set random number seed before doing cluster analysis
km = kmeans(kmeansColumnsNorm,nClust,iter.max = 100, nstart=20) #perform k-means analysis
str(km)

#look at centers of clusters
ctrs = as.data.frame(km$centers)
ctrs$size = km$size/sum(km$size)
ctrs$segment = 1:nrow(ctrs)
ctrs[order(-ctrs$size), ]

## tranform centers back to original units
ctrsRescaled = matrix(data = NA, nrow = nClust, ncol = ncol(km$centers))
for (i in 1:ncol(km$centers)){
    ctrsRescaled[ ,i] = km$centers[, i] * sd(kmeansColumns[, i]) + mean(kmeansColumns[, i])
}
colnames(ctrsRescaled) = colnames(km$centers)

#add cluster membership to original university data
surveyData$cluster = km$cluster

#pie chart with cluster percentages
percsize = paste(1:ncol(km$centers)," = ",format(km$size/sum(km$size)*100,digits=2),"%",sep="")
pie(km$size,labels=percsize,col=1:ncol(km$centers))

##plot that indicates cluster definitions against principal components
## we will cover principal components later in GBA 424
install.packages('factoextra')
library(factoextra)
fviz_cluster(km, geom = "point", data = kmeansColumnsNorm) + ggtitle(paste("k =",nClust))

##barplot of the cluster means
par(mar=c(11,4,4,4))
axis = range(km$centers) + (range(km$centers)[2] - range(km$centers)[1]) * c(-.1,.1)
bm = barplot(km$centers,col=1:nClust,beside=TRUE,las=2,main="Cluster Means",ylim = axis)
#text(bm,km$centers + .05*ifelse(km$centers>0,1,-1),formatC(km$centers,format="f",digits=2))

##choosing the number of clusters
## WSS plot - look for "elbow" in curve
fviz_nbclust(kmeansColumnsNorm,kmeans,method="wss",iter.max=100,nstart=20,k.max=15)

## Avg silhouette width - find max
fviz_nbclust(kmeansColumnsNorm,kmeans,method="silhouette",iter.max=100,nstart=20,k.max=15) 




