library(stabledist)
library(beepr)
library(Rdpack)
library(rdist)
library(caret)
library(hardhat)
library(ModelMetrics)
library(openxlsx)
library(gower)
library(rdist)
library(pdist)
library(naspaclust)
library(cluster)
library(MASS)
library(ggplot2)
library(ggpubr)
library(tibble)
library(geosphere)
library(PreProcess)
library(rgdal)
library(terra)
library(rgeos)
library(sp)
library(sf)
library(readxl)
options(max.print=999999)
##Input data kelurahan dan ketegoti permasalahan
data_merge <- read_excel("D:/BATAS KEKELURAHAN/data_merge.xlsx")

head(data_merge)

pop<- data_merge$population

#input data shp
jakarta <- "D:/BATAS KEKELURAHAN/Batas Kelurahan copy 3sept.shp"
map2<-readOGR(jakarta)
ind <- sapply(data_merge$ID_KEL,function(x) which(x==map2$ID_KEL))

#create distance
centroid2 <- gCentroid(map2, byid = T)
distance2 <- as.matrix(spDists(centroid2, longlat = TRUE))
distance2 <- distance2[ind,ind]

# PCA
#pca1 <- prcomp(scale(data_merge[,-c(1:3)]))
#summary(pca1)

#minmax scaler
data_risk <- scale(data_merge[,-c(1:3)])
prepro <- preProcess(data_merge[,-c(1:3)],method=c('range'))
data_risk <- predict(prepro, data_merge[,-c(1:3)])
head(data_risk)


##menggunakan list
res_abc <- lapply(2:10, function(j) {
  lapply(1:5, function(k){
    param_fgwc <- c(kind='v',ncluster=j,m=2,distance='minkowski',order=3,
                    alpha=1,a=1.2,b=1.2,max.iter=50,error=1e-6,randomN=k)
    abc_param <- c(vi.dist='normal',npar=5,pso=FALSE,same=15,n.onlooker=5,limit=5)
    res_abc <- fgwc(data_risk,pop, distance2,'abc',param_fgwc,abc_param)
  })
})



clus <- c()
for (o in 1:9) clus <- c(clus, res_abc[[o]][[1]]$cluster)
clus

objf <- c()
for (i in 1:9) objf <- c(objf,res_abc[[i]][[1]]$f_obj)
objf

val <- c()
for (j in 1:9) val <- c(val, res_abc[[j]][[1]]$validation)
val

iter <- c()
for (k in 1:9) iter <- c(iter, res_abc[[k]][[1]]$iteration)
iter

time <- c()
for (l in 1:9) time <- c(time, res_abc[[l]][[1]]$time)
time


member <- c()
for (m in 1:9) member <- c(member, res_abc[[m]][[1]]$membership)
member

final <- c()
for (l in 1:9) final <- c(final, res_abc[[l]][[1]]$finaldata)
final

#####
library(rJava)
library("xlsx")
mean_abc <- aggregate(.~cluster,res_abc[[3]][[1]]$finaldata,mean)
mean_abc
write.xlsx(mean_abc, file = "cluster_mean_abc.xlsx",
           sheetName = "hasilcluster", append = FALSE)


finaldata <- cbind.data.frame(data_merge,
                              cluster = res_abc[[3]][[1]]$cluster)
finaldata

##Write file excel from final data

write.xlsx(finaldata, file = "finaldata_abc.xlsx",
           sheetName = "hasilcluster", append = FALSE)
