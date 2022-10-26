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


# Particle Swarm Optimization  

  # First way
  #res1 <- psofgwc(data_risk,pop,distance2,3,2,'minkowski',4,npar=10)
  # Second way
  # initiate parameter
  #param_fgwc <- c(kind='v',ncluster=3,m=2,distance='minkowski',order=3,
                  #alpha=1,a=1.2,b=1.2,max.iter=50,error=1e-6,randomN=10)
  ## tune the PSO parameter
  #pso_param <- c(vi.dist='uniform',npar=15,
                 #vmax=0.8, pso.same=10, c1=0.7, c2=0.6, type='chaotic',
                 #wmax=0.8,wmin=0.3,map=0.3)
  ##FGWC with PSO
  #res_pso <- fgwc(data_risk,pop, distance2,'pso',param_fgwc,pso_param)
 
  ##menggunakan list
  res_pso <- lapply(2:10, function(j) {
              lapply(1:5, function(k){
                param_fgwc <- c(kind='v',ncluster=j,m=2,distance='minkowski',order=3,
                                alpha=1,a=1.2,b=1.2,max.iter=50,error=1e-6,randomN=k)
                pso_param <- c(vi.dist='uniform',npar=15,
                                vmax=0.8, pso.same=10, c1=0.7, c2=0.6,w.inert = "sim.annealing",
                                wmax=0.8,wmin=0.3,map=0.3)
                res_pso <- fgwc(data_risk,pop, distance2,'pso',param_fgwc,pso_param)
              })
            })

  
  
  clus <- c()
  for (o in 1:9) clus <- c(clus, res_pso[[o]][[1]]$cluster)
  clus
  res_pso[[2]][[1]]$cluster
  objf <- c()
  for (i in 1:9) objf <- c(objf,res_pso[[i]][[1]]$f_obj)
  objf
  
  val <- c()
  for (j in 1:9) val <- c(val, res_pso[[j]][[1]]$validation)
  val
  
  iter <- c()
  for (k in 1:9) iter <- c(iter, res_pso[[k]][[1]]$iteration)
  iter
  
  time <- c()
  for (l in 1:9) time <- c(time, res_pso[[l]][[1]]$time)
  time
  
  member <- c()
  for (m in 1:9) member <- c(member, res_pso[[m]][[1]]$membership)
  member
  
  final <- c()
  for (l in 1:9) final <- c(final, res_pso[[l]][[1]]$finaldata)
  final
  

  ##kelompok yang optimum dinyatakan dengan nilai PC yang maksimum.
  ##kelompok yang optimum dinyatakan dengan nilai indeks CE yang minimum.
  ##Partisi yang optimum dinyatakan dengan nilai indeks SC yang minimum.
  ##Jumlah kelompok yang optimum dinyatakan dengan nilai indeks S yang minimum.
  ##Jumlah kelompok yang optimal dinyatakan dengan nilai XB yang minimum.
  ##Ketika nilai IFV maksimum maka kualitas cluster semakin baik.
  ##Ketika nilai Kwon minimum maka kualitas cluster semakin baik.

  clusplot(data_risk,res[[2]][[1]]$cluster)
  
  mean_pso <- aggregate(.~cluster,res_pso[[2]][[1]]$finaldata,mean)
  mean_pso
  write.xlsx(mean_pso, file = "cluster_mean_pso.xlsx",
             sheetName = "hasilcluster", append = FALSE)
  
  
  finaldata <- cbind.data.frame(data_merge,
                                cluster = res_pso[[2]][[1]]$cluster)
  finaldata
  
##Write file excel from final data
  library(rJava)
  library("xlsx")
  write.xlsx(finaldata, file = "finaldata.xlsx",
             sheetName = "hasilcluster", append = FALSE)
 
  data_shp <- "D:/BATAS KEKELURAHAN/Batas Kelurahan copy 3sept.shp" ## shapefile directory
  
  datamap <- datamap <- st_read(data_shp)
  datamap <- merge(datamap,finaldata[finaldata$Cluster,],
                   by.x='ID_KEL',by.y='ID_KEL',all = TRUE)
  make_map <- function(datax,var_concern,legend_title,main){
    return(ggplot() +
             geom_sf(data = finaldata,aes_string(fill=var_concern),color='black')+
             scale_fill_brewer(palette = "PuBu",
                               name = legend_title,direction=-1)+
             labs(title=main)+theme_void()+theme(plot.title = element_text(size=20)))
  }
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  