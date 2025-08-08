iucn<-read.csv("E:/cuiyu/function_erosion/assessments.csv")
iucn[,3]<-gsub(" ","_",iucn[,3])
library(BAT)
mammal_spdis<-read.csv("E:/cuiyu/function_erosion/spdis/mammal_all.csv")
mammal_spdis<-mammal_spdis[,-1]
mammal_trait_pca<-read.csv("E:/cuiyu/function_erosion/imputation/mammal/mammal_pca.csv")
mammal_spdis[,1]<-gsub(" ","_",mammal_spdis[,1])
fu<-mammal_trait_pca[,1]
n<-as.numeric()
for (i in 1:length(fu)) {
  pos3<-which(mammal_spdis[,1]==fu[i])
  n<-c(n,pos3)
}
mammal_spdis<-mammal_spdis[n,]
n1<-unique(mammal_spdis[,1])
n2<-intersect(n1,iucn[,3])
pos<-which(iucn[,3]%in%n2)
iucn_mammal<-iucn[pos,]

pos1<-which(iucn_mammal[,4]=="Vulnerable")
pos2<-which(iucn_mammal[,4]=="Endangered")
pos3<-which(iucn_mammal[,4]=="Critically Endangered")
pos4<-which(iucn_mammal[,4]=="Extinct")
pos5<-which(iucn_mammal[,4]=="Extinct in the Wild")
pos6<-which(iucn_mammal[,4]=="Data Deficient")

threat.name<-c(iucn_mammal[pos1,3],iucn_mammal[pos2,3],iucn_mammal[pos3,3],iucn_mammal[pos4,3],iucn_mammal[pos5,3])
dd_name <- iucn_mammal[pos6,3]

n<-as.numeric()
for (i in 1:length(threat.name)) {
  pos3<-which(mammal_spdis[,1]==threat.name[i])
  n<-c(n,pos3)
}
d1<-mammal_spdis[-n,]
n<-as.numeric()
for (i in 1:length(dd_name)) {
  pos3<-which(d1[,1]==dd_name[i])
  n<-c(n,pos3)
}
d2<-d1[-n,]

sp.rich <- tapply(mammal_spdis[,1],mammal_spdis[,2], FUN=length)
grid<-unique(d1[,2])
landgrid<-grid
library(foreach)
library(doParallel)
mc <- makeCluster(80, type = "PSOCK")
doSNOW::registerDoSNOW(cl = mc)
n <- length(grid)
pb <- txtProgressBar(min = 1, max = n, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)

result<-foreach(i = 1:length(grid),.packages=c("BAT","stats","hypervolume"),.options.snow = list(progress = progress),.export = c("grid","d1","mammal_trait_pca"),.combine = rbind
) %dopar% 
  {
    pos<-which(d1[,2]==grid[i])
    assemblage<-d1[pos,1]
    
    if(length(assemblage)<=3){
      k<-1
    } else {
      comm<-matrix(data=1,ncol=length(assemblage),nrow=1)
      colnames(comm)<-assemblage
      comm<-as.data.frame(comm)
      subset_rows <- mammal_trait_pca[mammal_trait_pca[,1] %in% assemblage, ]
      subset_rows <-subset_rows [,2:5]
      a<-as.numeric()
      d<-as.numeric()
      for (j in 1:50) {
        hv<-kernel.build(comm, subset_rows, method.hv = "gaussian", abund = F,kde.bandwidth =estimate_bandwidth(mammal_trait_pca[,2:5]))
        f<-kernel.alpha(hv)
        di<-kernel.dispersion(hv)
        a<-c(a,f[[1]])
        d<-c(d,di[[1]])
      }
      fric<-median(a)
      disp<-median(d)
      g<-cbind(grid[i],fric,disp)
      return(g)
    }
    
  }
close(pb); stopCluster(cl = mc)
write.csv(result,"E:/cuiyu/function_erosion/kernel_alpha/mammal_nothreat_dd0.csv")

grid<-unique(d2[,2])
landgrid<-grid
library(foreach)
library(doParallel)
mc <- makeCluster(80, type = "PSOCK")
doSNOW::registerDoSNOW(cl = mc)
n <- length(grid)
pb <- txtProgressBar(min = 1, max = n, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)

result<-foreach(i = 1:length(grid),.packages=c("BAT","stats","hypervolume"),.options.snow = list(progress = progress),.export = c("grid","d2","mammal_trait_pca"),.combine = rbind
) %dopar% 
  {
    pos<-which(d2[,2]==grid[i])
    assemblage<-d2[pos,1]
    
    if(length(assemblage)<=3){
      k<-1
    } else {
      comm<-matrix(data=1,ncol=length(assemblage),nrow=1)
      colnames(comm)<-assemblage
      comm<-as.data.frame(comm)
      subset_rows <- mammal_trait_pca[mammal_trait_pca[,1] %in% assemblage, ]
      subset_rows <-subset_rows [,2:5]
      a<-as.numeric()
      d<-as.numeric()
      for (j in 1:50) {
        hv<-kernel.build(comm, subset_rows, method.hv = "gaussian", abund = F,kde.bandwidth =estimate_bandwidth(mammal_trait_pca[,2:5]))
        f<-kernel.alpha(hv)
        di<-kernel.dispersion(hv)
        a<-c(a,f[[1]])
        d<-c(d,di[[1]])
      }
      fric<-median(a)
      disp<-median(d)
      g<-cbind(grid[i],fric,disp)
      return(g)
    }
    
  }
close(pb); stopCluster(cl = mc)
write.csv(result,"E:/cuiyu/function_erosion/kernel_alpha/mammal_nothreat_dd100.csv")

seed=100
iucn<-read.csv("E:/cuiyu/function_erosion/assessments.csv")
iucn[,3]<-gsub(" ","_",iucn[,3])
library(BAT)
mammal_spdis<-read.csv("E:/cuiyu/function_erosion/spdis/mammal_all.csv")
mammal_spdis<-mammal_spdis[,-1]
mammal_trait_pca<-read.csv("E:/cuiyu/function_erosion/imputation/mammal/mammal_pca.csv")
mammal_spdis[,1]<-gsub(" ","_",mammal_spdis[,1])
fu<-mammal_trait_pca[,1]
n<-as.numeric()
for (i in 1:length(fu)) {
  pos3<-which(mammal_spdis[,1]==fu[i])
  n<-c(n,pos3)
}
mammal_spdis<-mammal_spdis[n,]
n1<-unique(mammal_spdis[,1])
n2<-intersect(n1,iucn[,3])
pos<-which(iucn[,3]%in%n2)
iucn_mammal<-iucn[pos,]

pos1<-which(iucn_mammal[,4]=="Vulnerable")
pos2<-which(iucn_mammal[,4]=="Endangered")
pos3<-which(iucn_mammal[,4]=="Critically Endangered")
pos4<-which(iucn_mammal[,4]=="Extinct")
pos5<-which(iucn_mammal[,4]=="Extinct in the Wild")
pos6<-which(iucn_mammal[,4]=="Data Deficient")

threat.name<-c(iucn_mammal[pos1,3],iucn_mammal[pos2,3],iucn_mammal[pos3,3],iucn_mammal[pos4,3],iucn_mammal[pos5,3])
dd_name <- iucn_mammal[pos6,3]


n<-as.numeric()
for (i in 1:length(threat.name)) {
  pos3<-which(mammal_spdis[,1]==threat.name[i])
  n<-c(n,pos3)
}
d1<-mammal_spdis[-n,]


sp.rich <- tapply(d1[,1],d1[,2], FUN=length)
sp.rich<-as.data.frame(sp.rich)
pos<-which(sp.rich[,1]>2)
landgrid<-as.numeric(rownames(sp.rich)[pos]) 

library(foreach)
library(doParallel)
mc <- makeCluster(90, type = "PSOCK")
doSNOW::registerDoSNOW(cl = mc)
n <- length(landgrid)
pb <- txtProgressBar(min = 1, max = n, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)

result<-foreach(i = 1:length(landgrid),.packages=c("BAT","stats","hypervolume"),.options.snow = list(progress = progress),.export = c("iucn_mammal","landgrid","d1","mammal_trait_pca"),.combine = rbind
) %dopar% 
  {
    pos1<-which(iucn_mammal[,4]=="Vulnerable")
    pos2<-which(iucn_mammal[,4]=="Endangered")
    pos3<-which(iucn_mammal[,4]=="Critically Endangered")
    pos4<-which(iucn_mammal[,4]=="Extinct")
    pos5<-which(iucn_mammal[,4]=="Extinct in the Wild")
    pos6<-which(iucn_mammal[,4]=="Data Deficient")
    
    p<-(length(pos1)+length(pos2)+length(pos3)+length(pos4)+length(pos5))/(length(iucn_mammal[,3])-length(pos6))
    a<-as.numeric()
    dis<-as.numeric()
    for (k in 1:50) {
      DD.threat<-sample(pos6,p*length(pos6))
      threat.name<-c(iucn_mammal[DD.threat,3])
      n<-as.numeric()
      for (l in 1:length(threat.name)) {
        pos3<-which(d1[,1]==threat.name[l])
        n<-c(n,pos3)
      }
      d2<-d1[-n,]
      pos<-which(d2[,2]==landgrid[i])
      assemblage<-d2[pos,1]
      
      if(length(assemblage)<=3){
        f<-0
        d<-0
      } else {
        comm<-matrix(data=1,ncol=length(assemblage),nrow=1)
        colnames(comm)<-assemblage
        comm<-as.data.frame(comm)
        subset_rows <- mammal_trait_pca[mammal_trait_pca[,1] %in% assemblage, ]
        subset_rows <-subset_rows [,2:5]
        hv<-kernel.build(comm, subset_rows, method.hv = "gaussian", abund = F,kde.bandwidth =estimate_bandwidth(mammal_trait_pca[,2:5]))
        f<-kernel.alpha(hv)
        d<-kernel.dispersion(hv)
      } 
      a<-c(a,f)
      dis<-c(dis,d)
    }
    fric<-median(a)
    disp<-median(dis)
    g<-cbind(landgrid[i],fric,disp)
    return(g)
  }

close(pb); stopCluster(cl = mc)
write.csv(result,"E:/cuiyu/function_erosion/kernel_alpha/mammal_nothreat_ddsample.csv")
###NJ tree
seed=100
iucn<-read.csv("E:/cuiyu/function_erosion/assessments.csv")
iucn[,3]<-gsub(" ","_",iucn[,3])
library(BAT)
mammal_spdis<-read.csv("E:/cuiyu/function_erosion/spdis/mammal_all.csv")
mammal_spdis<-mammal_spdis[,-1]
mammal_trait_pca<-read.csv("E:/cuiyu/function_erosion/imputation/mammal/mammal_pca.csv")
mammal_spdis[,1]<-gsub(" ","_",mammal_spdis[,1])
fu<-mammal_trait_pca[,1]
n<-as.numeric()
for (i in 1:length(fu)) {
  pos3<-which(mammal_spdis[,1]==fu[i])
  n<-c(n,pos3)
}
mammal_spdis<-mammal_spdis[n,]
d1<-mammal_spdis
landgrid<-unique(mammal_spdis[,2])

mammal_trait<-read.csv("E:/cuiyu/function_erosion/imputation/mammal/mammalsTraitsImputed.csv")
rownames(mammal_trait)<-mammal_trait[,1]
mammal_trait<-mammal_trait[,-1]
mammal_trait<-scale(mammal_trait)
am_dis<-dist(mammal_trait)
am_dis<-as.matrix(am_dis)
nj_tree <- tree.build(am_dis,
                      func = "nj")
nj_tree <- ape:::as.phylo(nj_tree)


library(foreach)
library(doParallel)
mc <- makeCluster(90, type = "PSOCK")
doSNOW::registerDoSNOW(cl = mc)
n <- length(landgrid)
pb <- txtProgressBar(min = 1, max = n, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)

result<-foreach(i = 1:length(landgrid),.packages=c("BAT","stats","hypervolume","picante"),.options.snow = list(progress = progress),.export = c("landgrid","d1","nj_tree"),.combine = rbind
) %dopar% 
  {
    
    pos<-which(d1[,2]==landgrid[i])
    assemblage<-d1[pos,1]
    comm<-matrix(data=1,ncol=length(assemblage),nrow=1)
    colnames(comm)<-assemblage
    comm<-as.data.frame(comm)
    up_alpha <-picante::pd(comm, tree = nj_tree,
                           include.root = FALSE)[1,1]
    disp<-dispersion(comm = comm,tree = nj_tree,abund = FALSE, 
                     relative = FALSE)
    g<-cbind(landgrid[i],up_alpha,disp)
    return(g)
  }

close(pb); stopCluster(cl = mc)
write.csv(result,"E:/cuiyu/function_erosion/nj_tree/mammal_all_njtree.csv")

mammal_spdis<-read.csv("E:/cuiyu/function_erosion/spdis/mammal_all.csv")
mammal_spdis<-mammal_spdis[,-1]
mammal_trait_pca<-read.csv("E:/cuiyu/function_erosion/imputation/mammal/mammal_pca.csv")
mammal_spdis[,1]<-gsub(" ","_",mammal_spdis[,1])
fu<-mammal_trait_pca[,1]
n<-as.numeric()
for (i in 1:length(fu)) {
  pos3<-which(mammal_spdis[,1]==fu[i])
  n<-c(n,pos3)
}
mammal_spdis<-mammal_spdis[n,]
n1<-unique(mammal_spdis[,1])
n2<-intersect(n1,iucn[,3])
pos<-which(iucn[,3]%in%n2)
iucn_mammal<-iucn[pos,]

pos1<-which(iucn_mammal[,4]=="Vulnerable")
pos2<-which(iucn_mammal[,4]=="Endangered")
pos3<-which(iucn_mammal[,4]=="Critically Endangered")
pos4<-which(iucn_mammal[,4]=="Extinct")
pos5<-which(iucn_mammal[,4]=="Extinct in the Wild")
pos6<-which(iucn_mammal[,4]=="Data Deficient")

threat.name<-c(iucn_mammal[pos1,3],iucn_mammal[pos2,3],iucn_mammal[pos3,3],iucn_mammal[pos4,3],iucn_mammal[pos5,3])
dd_name <- iucn_mammal[pos6,3]


n<-as.numeric()
for (i in 1:length(threat.name)) {
  pos3<-which(mammal_spdis[,1]==threat.name[i])
  n<-c(n,pos3)
}
d1<-mammal_spdis[-n,]
n<-as.numeric()
for (i in 1:length(dd_name)) {
  pos3<-which(d1[,1]==dd_name[i])
  n<-c(n,pos3)
}
d2<-d1[-n,]

sp.rich <- tapply(mammal_spdis[,1],mammal_spdis[,2], FUN=length)
grid<-unique(d1[,2])
landgrid<-grid
library(foreach)
library(doParallel)
mc <- makeCluster(80, type = "PSOCK")
doSNOW::registerDoSNOW(cl = mc)
n <- length(grid)
pb <- txtProgressBar(min = 1, max = n, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)

result<-foreach(i = 1:length(landgrid),.packages=c("BAT","stats","hypervolume","picante"),.options.snow = list(progress = progress),.export = c("landgrid","d1","nj_tree"),.combine = rbind
) %dopar% 
  {
    
    pos<-which(d1[,2]==landgrid[i])
    assemblage<-d1[pos,1]
    comm<-matrix(data=1,ncol=length(assemblage),nrow=1)
    colnames(comm)<-assemblage
    comm<-as.data.frame(comm)
    up_alpha <-picante::pd(comm, tree = nj_tree,
                           include.root = FALSE)[1,1]
    disp<-dispersion(comm = comm,tree = nj_tree,abund = FALSE, 
                     relative = FALSE)
    g<-cbind(landgrid[i],up_alpha,disp)
    return(g)
  }

close(pb); stopCluster(cl = mc)
write.csv(result,"E:/cuiyu/function_erosion/nj_tree/mammal_nothreat_njtreedd0.csv")

grid<-unique(d2[,2])
landgrid<-grid
library(foreach)
library(doParallel)
mc <- makeCluster(80, type = "PSOCK")
doSNOW::registerDoSNOW(cl = mc)
n <- length(grid)
pb <- txtProgressBar(min = 1, max = n, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)

result<-foreach(i = 1:length(landgrid),.packages=c("BAT","stats","hypervolume","picante"),.options.snow = list(progress = progress),.export = c("landgrid","d2","nj_tree"),.combine = rbind
) %dopar% 
  {
    
    pos<-which(d2[,2]==landgrid[i])
    assemblage<-d2[pos,1]
    comm<-matrix(data=1,ncol=length(assemblage),nrow=1)
    colnames(comm)<-assemblage
    comm<-as.data.frame(comm)
    up_alpha <-picante::pd(comm, tree = nj_tree,
                           include.root = FALSE)[1,1]
    disp<-dispersion(comm = comm,tree = nj_tree,abund = FALSE, 
                     relative = FALSE)
    g<-cbind(landgrid[i],up_alpha,disp)
    return(g)
  }

close(pb); stopCluster(cl = mc)
write.csv(result,"E:/cuiyu/function_erosion/nj_tree/mammal_nothreat_njtreedd100.csv")
