
library(missForest)
library(ape)
library(stats)
library(picante)
library(phytools)
###
mammalTraits<-mammalTraits[-10325,]
mammaln<-intersect(mammalTraits[,6],iucn_name[,2])
n<-as.numeric()
for (i in 1:length(mammaln)) {
  pos<-which(mammalTraits[,6]==mammaln[i])
  n<-c(n,pos)
}
mammalTraits<-mammalTraits[n,]
mammalTraits<-mammalTraits[,-c(1,2,3,4)]
write.csv(mammalTraits,"E:/cuiyu/function_erosion/trait/final_trait/trait_reported/mammal.csv")
######
mammalTraits <- read.csv("E:/cuiyu/function_erosion/trait/final_trait/trait_reported/mammal.csv")

mammalTraits[,6]<-gsub(mammalTraits[,6],pattern = " ", replacement= "_")
rownames(mammalTraits) <- mammalTraits[,6]

### pd
nsample <- 1000
phylogeniesToSample <- sample(1:50000, nsample)



tree_files <- list.files(path = "E:/cuiyu/function_erosion/mammaltree", pattern = "\\.tre$", full.names = TRUE)
tree_files<-tree_files[2]
tree_list <- lapply(tree_files[100], read.tree)
write.tree(tree_list, file = "E:/cuiyu/function_erosion/phlo/mammal_trees_combined.tre")


pdmammals <- read.tree("E:/cuiyu/function_erosion/phlo/mammal_trees_combined.tre")
identical(pdmammals[[1]]$tip.label, pdmammals[[2]]$tip.label)

pd_name <- pdmammals[[1]]$tip.label
pd_nrow <- length(pd_name)
stand_name <- as.data.frame(matrix(NA, nrow = length(pd_name), ncol=1,
                                   dimnames=list(pd_name, "Name")))
pd_name <-gsub(pd_name ,pattern = "_", replacement= " ")

iucn_name<-read.csv("E:/cuiyu/name/mammal_iucn_name.csv")



s_name<-read.csv("E:/cuiyu/name/mammal_sname_final.csv")
s_name<-s_name[,-1]
s_name<-unique(s_name)

i1<-intersect(pd_name,iucn_name[,2])
i2<-intersect(pd_name,s_name[,1])
length(i1)+length(i2)

n<-as.numeric()
for (i in 1:length(i2)) {
  pos<-which(s_name[,1]==i2[i])
  n<-c(n,pos)
}
i3<-unique(s_name[n,2])

n<-as.numeric()
for (i in 1:length(i2)) {
  pos1<-which(s_name[,1]==i2[i])
  pos2<-which(pd_name[]==i2[i])
  if(length(pos1)==1){
    pd_name[pos2]<-s_name[pos1,2]
  }else{
    n<-c(n,i)
  }
}


pd_name <-gsub(pd_name ,pattern = " ", replacement= "_")


stand_name[, "Name"] <- pd_name

nsample <- 1000
phylogeniesToSample <- sample(1:length(pdmammals), nsample)

library(ape)
library(stats)
library(picante)
library(phytools)
library(tidyr)
library(foreach)
library(doParallel)

pdTraits <- pdmammals[[phylogeniesToSample[1]]]
pdNames <-  pdTraits$tip.label
pdNewNames <- stand_name[pdNames,]
pdTraits$tip.label <- pdNewNames
tipsToDrop <- which(is.na(pdTraits$tip.label))
pdTraits <- drop.tip(pdTraits, tipsToDrop)
tipsToAdd <- which(! rownames(mammalTraits) %in% pdTraits$tip.label)
namesToAdd <- rownames(mammalTraits)[tipsToAdd]
genusToAdd <- unlist(strsplit(rownames(mammalTraits), "_"))[which(1:length(strsplit(rownames(mammalTraits), "_")) %% 2 == 1)]
pdTraitsAdd <- pdTraits
if (!is.ultrametric(pdTraitsAdd )) {
  pdTraitsAdd  <- force.ultrametric(pdTraitsAdd , method = "extend")  
}
for(add in 1:length(namesToAdd)){
  pdTraitsAdd <- add.species.to.genus(tree = pdTraitsAdd,
                                      species = namesToAdd[add], genus=genusToAdd[add], where="root")
}
phylDiss <- sqrt(cophenetic(pdTraitsAdd))
pdTraits <- NULL


mc <- makeCluster(50, type = "PSOCK")
doSNOW::registerDoSNOW(cl = mc)
n <- length(phylogeniesToSample)
pb <- txtProgressBar(min = 1, max = n, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)

result<-foreach(i = 2:length(phylogeniesToSample),.packages=c("ape","stats","hypervolume","picante","phytools"),.options.snow = list(progress = progress),.export = c("phylogeniesToSample","pdmammals","mammalTraits","stand_name","phylDiss")
) %dopar% 
  {
    
    pdTraits <- pdmammals[[phylogeniesToSample[i]]]
    pdNames <-  pdTraits$tip.label
    pdNewNames <- stand_name[pdNames,]
    pdTraits$tip.label <- pdNewNames
    tipsToDrop <- which(is.na(pdTraits$tip.label))
    pdTraits <- drop.tip(pdTraits, tipsToDrop)
    tipsToAdd <- which(! rownames(mammalTraits) %in% pdTraits$tip.label)
    namesToAdd <- rownames(mammalTraits)[tipsToAdd]
    genusToAdd <- unlist(strsplit(rownames(mammalTraits), "_"))[which(1:length(strsplit(rownames(mammalTraits), "_")) %% 2 == 1)]
    pdTraitsAdd <- pdTraits
    if (!is.ultrametric(pdTraitsAdd )) {
      pdTraitsAdd  <- force.ultrametric(pdTraitsAdd , method = "extend")  # 或用 chronos()
    }
    for(add in 1:length(namesToAdd)){
      pdTraitsAdd <- add.species.to.genus(tree = pdTraitsAdd,
                                          species = namesToAdd[add], genus=genusToAdd[add], where="root")
    }
    
    phylDissAux <- sqrt(cophenetic(pdTraitsAdd))
    
    pdTraits <- NULL
    return( phylDissAux)
  }
close(pb); stopCluster(cl = mc)

for(i in 1:999){
  phylDiss<-phylDiss+result[[i]]
}
phylDiss <- phylDiss/nsample
hist(phylDiss)
pcoaPhyl <- cmdscale(phylDiss, k=10) #
colnames(pcoaPhyl) <- paste0("Eigen.", 1:ncol(pcoaPhyl))
write.table(pcoaPhyl, "E:/cuiyu/function_erosion/pddis/pcoapdmammal.txt")
write.csv(pcoaPhyl, "E:/cuiyu/function_erosion/pddis/pcoapdmammal.csv")

traitsInPhyl <- rownames(mammalTraits)[which(rownames(mammalTraits) %in% rownames(pcoaPhyl))]
phylInTraits <- rownames(pcoaPhyl)[which(rownames(pcoaPhyl) %in% rownames(mammalTraits))]
commonSpecies <- unique(c(traitsInPhyl, phylInTraits))

pcoaPhylWithTraits <- pcoaPhyl[commonSpecies, ]
traitsWithPhyl <- mammalTraits[commonSpecies, ]
traitsAndPCOA <- cbind(traitsWithPhyl, pcoaPhylWithTraits)
write.csv(traitsAndPCOA, "E:/cuiyu/function_erosion/imputation/mammal/traitsWithPCOA.csv")
set.seed(100)

mc <- makeCluster(30, type = "PSOCK")
doSNOW::registerDoSNOW(cl = mc)
n <- 100
pb <- txtProgressBar(min = 1, max = n, style = 3)
progress <- function(n) setTxtProgressBar(pb, n)

result<-foreach(i = 1:100,.packages=c("missForest","stats"),.options.snow = list(progress = progress),.export = c("mammalData","columnsTraits"),.combine = rbind
) %dopar% 
  {
    traitcol<-mammalData[,1:6]
    
    complete_rows <- which(complete.cases(traitcol))
    incomplete_rows <- which(!complete.cases(traitcol))
    missing_patterns <- lapply(incomplete_rows, function(i) which(is.na(traitcol[i, ])))
    
    sample_size <- ceiling(length(complete_rows) * 0.1)
    sampled_rows <- sample(complete_rows, sample_size)
    
    traitcol_new <- traitcol
    for (i in sampled_rows) {
      pattern <- sample(missing_patterns, 1)[[1]]
      traitcol_new[i, pattern] <- NA
    }
    
    mammalData_new<-mammalData
    mammalData_new[,1:6]<-traitcol_new
    # imputation
    columsImputation <- 1:(which(colnames(mammalData_new) == "Eigen.10")) 
    mammalTraitslogImputed<-as.matrix(missForest(xmis= mammalData_new[, columsImputation])$ximp)
    mammalTraitsImputed <- mammalData_new
    mammalTraitsImputed[,columnsTraits] <- mammalTraitslogImputed[,columnsTraits]
    mammalTraitsImputed <- mammalTraitsImputed[, columnsTraits]
    
    ###NMSR
    mammal<-read.csv("F:/cuiyu/function_erosion/imputation/mammal/mammalsTraitsImputed.csv")
    rownames(mammal)<-mammal[,1]
    mammal<-mammal[,-1]
    pca<-prcomp(mammal,scale. = TRUE)
    
    scores_full <- predict(pca, newdata = mammal)
    scores_true <- scores_full[sampled_rows, ]
    scores_pred <- predict(pca, newdata = mammalTraitsImputed)
    scores_fake <- scores_pred[sampled_rows, ]  
    
    nrmse <- function(obs, pred) {
      rmse <- tcrt(mean((obs - pred)^2))
      range_y <- max(obs) - min(obs)
      return(rmse / range_y)
    }
    
    nrmse_pc1<-nrmse(scores_true[,1], scores_fake[,1])
    nrmse_pc2<-nrmse(scores_true[,2], scores_fake[,2])
    nrmse_pc3<-nrmse(scores_true[,3], scores_fake[,3])
    nrmse_pc4<-nrmse(scores_true[,4], scores_fake[,4])
    
    nrm<-c(nrmse_pc1,nrmse_pc2,nrmse_pc3,nrmse_pc4)
    return(nrm)
  }
close(pb); stopCluster(cl = mc)
write.csv(result,"F:/cuiyu/function_erosion/imputation/mammal/NRMSE.csv")
