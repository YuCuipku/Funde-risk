#### calculate function diversity
###remove species of EW&EX
r<-read.csv("C:/Users/13663/Desktop/AM/sp_range")
k<-unique(r)

iucn<-read.csv("F:/FREDGE/AMPHIBIANS_IUCN")##IUCN conservation status
iucn<-iucn[,2:3]
iucn<-unique(iucn)
pos1<-which(iucn[,2]=="EW")
pos2<-which(iucn[,2]=="EX")
ex.name<-c(iucn[pos1,1],iucn[pos2,1])
n<-as.numeric()
for (i in 1:length(ex.name)) {
  pos3<-which(k[,1]==ex.name[i])
  n<-c(n,pos3)
}
k<-k[-n,]

##remove species range with the presence code 4–6 and the origin code 3–5.
out<-read.csv("C:/Users/13663/Desktop/AM_origin.csv")
ex<-read.csv("C:/Users/13663/Desktop/AM_presence.csv")
out<-out[,c(4,30)]
ex<-ex[,c(4,30)]
clean<-rbind(ex,out)
clean<-unique(clean)
clean[,2]<-paste("g",clean[,2],sep="")
colnames(clean)<-c("spname","grid50")
k<-anti_join(k, clean)

## calculate function diversity
data <- readRDS("F:/cuiyu/Null Model/traitSpace_YuCui.rds")##species trait data (pca axes)
sp_trait<-rownames(data$Amphibians)
fu<-intersect(sp_trait,k[,1])##species with range and trait data
length(fu)
n<-as.numeric()
for (i in 1:length(fu)) {
  pos3<-which(k[,1]==fu[i])
  n<-c(n,pos3)
}
k<-k[n,]

sp.rich <- tapply(k[,1],k[,2], FUN=length)

#####threat FR 
data <- readRDS("F:/cuiyu/Null Model/traitSpace_YuCui.rds")
library(geometry)
b <- gsub(" ", "_", k[,1])
k[,1]<-b
for (q in 2:100) {
  ##randomly selected DD species as threatened species based on the probability and repeated this process 100 times
  pos1<-which(iucn[,2]=="VU")
  pos2<-which(iucn[,2]=="EN")
  pos3<-which(iucn[,2]=="CR")
  pos4<-which(iucn[,2]=="DD")
  p<-(length(pos1)+length(pos2)+length(pos3))/(length(iucn[,1])-length(pos4))
  p*length(pos4)
  DD.threat<-sample(pos4,p*length(pos4))
  threat.name<-c(iucn[pos1,1],iucn[pos2,1],iucn[pos3,1],iucn[DD.threat,1])
  threat.name <- gsub(" ", "_",  threat.name)
  threat<-intersect(threat.name,k[,1])
  n<-as.numeric()
  for (i in 1:length(threat)) {
    pos3<-which(k[,1]==threat[i])
    n<-c(n,pos3)
  }
  d1<-k[n,]###only threat species
  d2<-k[-n,]##no threat species
  #threat species FRic
  grid<-unique(d1[,2])
  
  fd.pattern1<-matrix(data=NA,nrow=length(grid),ncol=2)
  fd.pattern1<-as.data.frame(fd.pattern1)
  fd.pattern1[,1]<-grid
  for(i in 1:length(grid)){
    pos<-which(d1[,2]==grid[i])
    assemblage<-d1[pos,1]
    coord<-matrix(data=NA,nrow=length(assemblage),ncol=2)
    for (l in 1:length(assemblage)) {
      pos.fd<-which(rownames(data$Amphibians)[]==assemblage[l])
      coord[l,1]<-data$Amphibians[pos.fd,1]
      coord[l,2]<-data$Amphibians[pos.fd,2]
    }
    coord<-unique(coord)
    if(length(coord[,2])>2){
      ConVexHull<-convhulln(coord,"FA")
      FRic<-ConVexHull$vol}
    else {FRic<-0}
    fd.pattern1[i,2]<-FRic
    
  }
  ###remove threat species FRic
  grid<-unique(d2[,2])
  
  fd.pattern2<-matrix(data=NA,nrow=length(grid),ncol=2)
  fd.pattern2<-as.data.frame(fd.pattern2)
  fd.pattern2[,1]<-grid
  for(i in 1:length(grid)){
    pos<-which(d2[,2]==grid[i])
    assemblage<-d2[pos,1]
    coord<-matrix(data=NA,nrow=length(assemblage),ncol=2)
    for (l in 1:length(assemblage)) {
      pos.fd<-which(rownames(data$Amphibians)[]==assemblage[l])
      coord[l,1]<-data$Amphibians[pos.fd,1]
      coord[l,2]<-data$Amphibians[pos.fd,2]
    }
    coord<-unique(coord)
    if(length(coord[,2])>2){
      ConVexHull<-convhulln(coord,"FA")
      FRic<-ConVexHull$vol}
    else {FRic<-0}
    fd.pattern2[i,2]<-FRic
    
  }
  
  colnames(fd.pattern1)<-c("fd","grid")
  colnames(fd.pattern2)<-c("fd","grid")
  f1<-rbind(f1,fd.pattern1)
  f2<-rbind(f2,fd.pattern2)
  print(q)
}



f1_mean<-tapply(f1[,2], f1[,1], mean)
f2_mean<-tapply(f2[,2], f2[,1], mean)

write.csv(f1_mean,"F:/Function/AM_threat_FRic_DDmean.csv")
write.csv(f2_mean,"F:/Function/AM_ex_threat_FRic_DDmean.csv")

#####All Fric
data <- readRDS("F:/traitSpace_YuCui.rds")
library(geometry)
grid<-unique(k[,2])
b <- gsub(" ", "_", k[,1])
k[,1]<-b
fd.pattern<-matrix(data=NA,nrow=length(grid),ncol=2)
fd.pattern<-as.data.frame(fd.pattern)
fd.pattern[,1]<-grid
for(i in 1:length(grid)){
  pos<-which(k[,2]==grid[i])
  assemblage<-k[pos,1]
  coord<-matrix(data=NA,nrow=length(assemblage),ncol=2)
  for (l in 1:length(assemblage)) {
    pos.fd<-which(rownames(data$Amphibians)[]==assemblage[l])
    coord[l,1]<-data$Amphibians[pos.fd,1]
    coord[l,2]<-data$Amphibians[pos.fd,2]
  }
  coord<-unique(coord)
  if(length(coord[,2])>2){
    ConVexHull<-convhulln(coord,"FA")
    FRic<-ConVexHull$vol}
  else {FRic<-0}
  fd.pattern[i,2]<-FRic
  print(i)
}
write.csv(fd.pattern,"F:/Function/AM_all_FRic.csv")

##caculate the erosion of funcion diversity
AM_all<-read.csv("F:/Function/AM_all_FRic.csv")
AM_all<-AM_all[,-1]
AM_ex_threat<-read.csv("F:/Function/AM_ex_threat_FRic_DDmean.csv")

for (i in 1:length(AM_all[,1])) {
  pos<-which(AM_ex_threat[,1]==AM_all[i,1])
  if(length(pos)==0){
    AM_all[i,2]<-AM_all[i,2]} 
  else {AM_all[i,2]<-(AM_all[i,2]-AM_ex_threat[pos,2])}
}
write.csv(AM_all,"F:/Function/AM_change_FRic.csv")

all<-read.csv("F:/Function/AM_all_FRic.csv")
all<-all[,-1]
colnames(all)<-c("OBJECTID","AM_all_FRic_new")
change_threat<-read.csv("F:/Function/AM_change_FRic.csv")
change_threat<-change_threat[,-1]
colnames(change_threat)<-c("OBJECTID","AM_change_FRic")

all[,2]<-change_threat[,2]/all[,2]
write.csv(all,"F:/Function/AM_propotion_FRicloss")
