###plot
library(sp)
library(broom)
library(dplyr)
library(plyr)
library(ggplot2)
library(maps)
library(mapdata)
library(spData)
library(rgdal)
library(maptools)
library(rgeos)
d<-readOGR( "C:/Users/13663/Desktop/GIS","cuiyu")
land<-readOGR( "C:/Users/13663/Desktop/grid50","landgridnew")
land@data[,1]<-paste("g",land@data[,1],sep="")
x <- land@data
xs <- data.frame(x,id=seq(0:18241)-1)
rownames(land@data)<-land@data[,1]
colnames(land@data)[2] <- 'long'
grid501 <- tidy(land,region = "FID_grid50") 
grid50_data <- join(grid501, xs, type = "full",by="id")

##Fig1
####TM
ojo<-read.csv("F:/Function/TM_all_FRic.csv")
ojo<-ojo[,-1]
colnames(ojo)<-c("X","x")
o<-read.csv("C:/Users/13663/Desktop/landgrid")
o<-o[,2:3]
a<-intersect(ojo[,1],o[,1])
n<-as.numeric()
for (i in 1:length(a)) {
  pos3<-which(o[,1]==a[i])
  n<-c(n,pos3)
}
o<-o[-n,]
o[,2]<-0
m<-rbind(o,ojo)
ojo<-m
o<-read.csv("C:/Users/13663/Desktop/landgrid")
o<-o[,2:3]
n<-as.numeric()
for (i in 1:length(o[,2])) {
  pos3<-which(ojo[,1]==o[i,1])
  n<-c(n,pos3)
}
ojo<-ojo[n,]
o1<-ojo
max475<-sort(ojo[,2],decreasing=T)[1:length(ojo[,2])]
max475<-unique(max475)
wg<-as.numeric()
for (i in 1:length(ojo[,2])) {
  pos<-which(ojo[,2]==max475[i])
  wg<-c(wg,pos)
}
wg1<-wg[1:456]###2.5%
wg2<-wg[457:912]###5%
wg3<-wg[913:1824]###10%
wg4<-wg[1825:3101]###17%
wg5<-wg[3102:5473]###30%
wg6<-wg[5474:9121]###50%
wg7<-wg[9122:13682]###75%
wg8<-wg[13682:length(ojo[,2])]###100%


ojo[wg1,2]<-1
ojo[wg2,2]<-2
ojo[wg3,2]<-3
ojo[wg4,2]<-4
ojo[wg5,2]<-5
ojo[wg6,2]<-6
ojo[wg7,2]<-7
ojo[wg8,2]<-8
colnames(ojo)<-c("id","number")
data <- join(grid50_data, ojo, type = "full")  
data$number<-as.factor(data$number)
p1<-ggplot(data, aes(x = long, y = lat, group = group,fill = number)) +
  geom_polygon(colour="transparent") +
  scale_fill_manual(values =c("#F46D43","#FDAE61","#FEE090","#FFFFBF","#E0F3F8","#ABD9E9","#74ADD1","#4575B4"),na.value = "white",breaks = c("1","2","3","4","5","6","7","8"),labels=c("2.5%", "5%", "10%","17%","30%","50%","70%","100%")) +  
  labs(fill="")+
  theme(               
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
  )+
  geom_polygon(data = d,aes(x=long, y = lat, group = group),fill = "transparent", colour="gray", size=0.2) +
  theme(               
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
  )+
  theme(text=element_text(size=10, family="sans"))+
  guides(fill = guide_legend(keywidth = 1, keyheight = 1))


##################Bird

ojo<-read.csv("F:/Function/Bird_all_FRic.csv")
ojo<-ojo[,-1]
colnames(ojo)<-c("X","x")
o<-read.csv("C:/Users/13663/Desktop/landgrid")
o<-o[,2:3]
a<-intersect(ojo[,1],o[,1])
n<-as.numeric()
for (i in 1:length(a)) {
  pos3<-which(o[,1]==a[i])
  n<-c(n,pos3)
}
o<-o[-n,]
o[,2]<-0
m<-rbind(o,ojo)
ojo<-m
o<-read.csv("C:/Users/13663/Desktop/landgrid")
o<-o[,2:3]
n<-as.numeric()
for (i in 1:length(o[,2])) {
  pos3<-which(ojo[,1]==o[i,1])
  n<-c(n,pos3)
}
ojo<-ojo[n,]
o1<-ojo
max475<-sort(ojo[,2],decreasing=T)[1:length(ojo[,2])]
max475<-unique(max475)
wg<-as.numeric()
for (i in 1:length(ojo[,2])) {
  pos<-which(ojo[,2]==max475[i])
  wg<-c(wg,pos)
}
wg1<-wg[1:456]###2.5%
wg2<-wg[457:913]###5%
wg3<-wg[914:1826]###10%
wg4<-wg[1827:3101]###17%
wg5<-wg[3102:5473]###30%
wg6<-wg[5474:9145]###50%
wg7<-wg[9146:13564]###75%
wg8<-wg[13565:length(ojo[,2])]###100%

ojo[wg1,2]<-1
ojo[wg2,2]<-2
ojo[wg3,2]<-3
ojo[wg4,2]<-4
ojo[wg5,2]<-5
ojo[wg6,2]<-6
ojo[wg7,2]<-7
ojo[wg8,2]<-8
colnames(ojo)<-c("id","number")
data <- join(grid50_data, ojo, type = "full")  

data$number<-as.factor(data$number)
b1<-ggplot(data, aes(x = long, y = lat, group = group,fill = number)) +
  geom_polygon(colour="transparent") +
  scale_fill_manual(values =c("#F46D43","#FDAE61","#FEE090","#FFFFBF","#E0F3F8","#ABD9E9","#74ADD1","#4575B4"),na.value = "white",breaks = c("1","2","3","4","5","6","7","8"),labels=c("2.5%", "5%", "10%","17%","30%","50%","70%","100%")) +  
  labs(fill="")+
  theme(               
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
  )+
  geom_polygon(data = d,aes(x=long, y = lat, group = group),fill = "transparent", colour="gray", size=0.2) +
  theme(               
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
  )+
  theme(text=element_text(size=10, family="sans"))+
  guides(fill = guide_legend(keywidth = 1, keyheight = 1))


#######AM

ojo<-read.csv("F:/Function/AM_all_FRic.csv")
ojo<-ojo[,-1]
max475<-sort(ojo[,2],decreasing=T)[1:length(ojo[,2])]
max475<-unique(max475)
wg<-as.numeric()
for (i in 1:length(ojo[,2])) {
  pos<-which(ojo[,2]==max475[i])
  wg<-c(wg,pos)
}
wg1<-wg[1:321]###2.5%
wg2<-wg[322:678]###5%
wg3<-wg[679:1335]###10%
wg4<-wg[1336:2269]###17%
wg5<-wg[2270:4004]###30%
wg6<-wg[4005:6653]###50%
wg7<-wg[6654:9902]###75%
wg8<-wg[9903:13239]###100%

ojo[wg1,2]<-1
ojo[wg2,2]<-2
ojo[wg3,2]<-3
ojo[wg4,2]<-4
ojo[wg5,2]<-5
ojo[wg6,2]<-6
ojo[wg7,2]<-7
ojo[wg8,2]<-8
colnames(ojo)<-c("id","number")
data <- join(grid50_data, ojo, type = "full")  

data$number<-as.factor(data$number)
a1<-ggplot(data, aes(x = long, y = lat, group = group,fill = number)) +
  geom_polygon(colour="transparent") +
  scale_fill_manual(values =c("#F46D43","#FDAE61","#FEE090","#FFFFBF","#E0F3F8","#ABD9E9","#74ADD1","#4575B4"),na.value = "white",breaks = c("1","2","3","4","5","6","7","8"),labels=c("2.5%", "5%", "10%","17%","30%","50%","70%","100%")) +  
  labs(fill="")+
  theme(               
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
  )+
  geom_polygon(data = d,aes(x=long, y = lat, group = group),fill = "transparent", colour="gray", size=0.2) +
  theme(               
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
  )+
  theme(text=element_text(size=10, family="sans"))+
  guides(fill = guide_legend(keywidth = 1, keyheight = 1))


###############RE

ojo<-read.csv("C:/Users/13663/Desktop/Function/RE_all_FRic.csv")
ojo<-ojo[,-1]
ojo<-ojo[-12160,]
max475<-sort(ojo[,2],decreasing=T)[1:length(ojo[,2])]
max475<-unique(max475)
wg<-as.numeric()
for (i in 1:length(ojo[,2])) {
  pos<-which(ojo[,2]==max475[i])
  wg<-c(wg,pos)
}
wg1<-wg[1:355]###2.5%
wg2<-wg[356:768]###5%
wg3<-wg[769:1441]###10%
wg4<-wg[1442:2426]###17%
wg5<-wg[2427:4323]###30%
wg6<-wg[4324:7215]###50%
wg7<-wg[7216:10816]###75%
wg8<-wg[10817:length(ojo[,2])]###100%

ojo[wg1,2]<-1
ojo[wg2,2]<-2
ojo[wg3,2]<-3
ojo[wg4,2]<-4
ojo[wg5,2]<-5
ojo[wg6,2]<-6
ojo[wg7,2]<-7
ojo[wg8,2]<-8
colnames(ojo)<-c("id","number")
data <- join(grid50_data, ojo, type = "full")  

data$number<-as.factor(data$number)
r1<-ggplot(data, aes(x = long, y = lat, group = group,fill = number)) +
  geom_polygon(colour="transparent") +
  scale_fill_manual(values =c("#F46D43","#FDAE61","#FEE090","#FFFFBF","#E0F3F8","#ABD9E9","#74ADD1","#4575B4"),na.value = "white",breaks = c("1","2","3","4","5","6","7","8"),labels=c("2.5%", "5%", "10%","17%","30%","50%","70%","100%")) +  
  labs(fill="")+
  theme(               
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
  )+
  geom_polygon(data = d,aes(x=long, y = lat, group = group),fill = "transparent", colour="gray", size=0.2) +
  theme(               
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
  )+
  theme(text=element_text(size=10, family="sans"))+
  guides(fill = guide_legend(keywidth = 1, keyheight = 1))


library(ggpubr)
p<-ggarrange(p1, b1, a1,r1, ncol = 2, nrow = 2,
             labels = c("a","b","c","d"),
             font.label = list(size = 12, face = "bold"), common.legend = T,legend="left")
library(grid)
library(magick)
library(cowplot)
img1 <- image_read("F:/Function/fig_new/Felis silvestris catus.png")
img2 <- image_read("F:/Function/fig_new/Eremopterix australis.png")
img3 <- image_read("F:/Function/fig_new/Rheobatrachus vitellinus.png")
img4 <- image_read("F:/Function/fig_new/Saurodactylus brosseti.png")

img_grob1 <- rasterGrob(img1, interpolate=TRUE)
img_grob2 <- rasterGrob(img2, interpolate=TRUE)
img_grob3 <- rasterGrob(img3, interpolate=TRUE)
img_grob4 <- rasterGrob(img4, interpolate=TRUE)

ggdraw() +
  draw_plot(p) +
  draw_grob(img_grob1, x = 0.07, y = 0.6, width = 0.12, height = 0.12)+
  draw_grob(img_grob2, x = 0.54, y = 0.6, width = 0.1, height = 0.1)+
  draw_grob(img_grob3, x = 0.07, y = 0.1,width = 0.1, height = 0.1)+
  draw_grob(img_grob4, x = 0.54, y = 0.1, width = 0.1, height = 0.1)

###Fig 2
###ddsample
ojo<-read.csv("F:/Function/kernel_alpha/mammal_allspecies.csv")
ojo<-ojo[,-1]
ojo<-ojo[,-3]
ojo<-na.omit(ojo)
pos<-which(ojo[,2]==1)
ojo<-ojo[-pos,]
ojo[,1]<-paste("g",ojo[,1],sep="")
ojo1<-ojo
ojo<-read.csv("F:/Function/kernel_alpha/mammal_change_FRic_ddsample.csv")
ojo<-ojo[,-1]
ojo<-ojo[,-3]
ojo<-na.omit(ojo)
ojo2<-ojo
for (i in 1:length(ojo1[,1])) {
  pos<-which(ojo2[,1]==ojo1[i,1])
  
  if(length(pos)==0) {ojo1[i,2]<-1}
  else {
    ojo1[i,2]<-ojo2[pos,2]/ojo1[i,2]
  }
  
}
ojo<-ojo1
colnames(ojo)<-c("X","x")
o<-read.csv("C:/Users/13663/Desktop/landgrid")
o<-o[,2:3]
a<-intersect(ojo[,1],o[,1])
n<-as.numeric()
for (i in 1:length(a)) {
  pos3<-which(ojo[,1]==a[i])
  n<-c(n,pos3)
}
ojo<-ojo[n,]
ojo<-na.omit(ojo)
colnames(ojo)<-c("X","TM")
oo<-read.csv("F:/Function/spdis/mammal_threat&ddgrid.csv")
oo[,3]<-0
oo<-oo[,-1]
colnames(oo)<-c("X","TM")

a<-intersect(ojo[,1],oo[,1])
n<-as.numeric()
for (i in 1:length(a)) {
  pos3<-which(ojo[,1]==a[i])
  n<-c(n,pos3)
}
ojo<-ojo[n,]
o1<-ojo


ojo<-read.csv("F:/Function/kernel_alpha/bird_allspecies.csv")
ojo<-ojo[,-1]
ojo<-ojo[,-3]
ojo<-na.omit(ojo)
ojo[,1]<-paste("g",ojo[,1],sep="")
ojo1<-ojo
ojo<-read.csv("F:/Function/kernel_alpha/bird_change_FRic_ddsample.csv")
ojo<-ojo[,-1]
ojo<-ojo[,-3]
ojo<-na.omit(ojo)
ojo2<-ojo
for (i in 1:length(ojo1[,1])) {
  pos<-which(ojo2[,1]==ojo1[i,1])
  
  if(length(pos)==0) {ojo1[i,2]<-1}
  else {
    ojo1[i,2]<-ojo2[pos,2]/ojo1[i,2]
  }
  
}
ojo<-ojo1
colnames(ojo)<-c("X","x")
o<-read.csv("C:/Users/13663/Desktop/landgrid")
o<-o[,2:3]
a<-intersect(ojo[,1],o[,1])
n<-as.numeric()
for (i in 1:length(a)) {
  pos3<-which(ojo[,1]==a[i])
  n<-c(n,pos3)
}
ojo<-ojo[n,]
ojo<-na.omit(ojo)
colnames(ojo)<-c("X","Bird")
oo<-read.csv("F:/Function/spdis/bird_threat&ddgrid.csv")
oo[,3]<-0
oo<-oo[,-1]
colnames(oo)<-c("X","Bird")
o<-read.csv("C:/Users/13663/Desktop/landgrid")
o<-o[,2:3]
a<-intersect(oo[,1],o[,1])
n<-as.numeric()
for (i in 1:length(a)) {
  pos3<-which(oo[,1]==a[i])
  n<-c(n,pos3)
}
oo<-oo[n,]

a<-intersect(ojo[,1],oo[,1])
n<-as.numeric()
for (i in 1:length(a)) {
  pos3<-which(ojo[,1]==a[i])
  n<-c(n,pos3)
}
ojo<-ojo[n,]
o2<-ojo


ojo<-read.csv("F:/Function/kernel_alpha/amphibian_allspecies.csv")
ojo<-ojo[,-1]
ojo<-ojo[,-3]
ojo<-na.omit(ojo)
ojo[,1]<-paste("g",ojo[,1],sep="")
ojo1<-ojo
ojo<-read.csv("F:/Function/kernel_alpha/amphibian_change_FRic_ddsample.csv")
ojo<-ojo[,-1]
ojo<-ojo[,-3]
ojo<-na.omit(ojo)
ojo2<-ojo
for (i in 1:length(ojo1[,1])) {
  pos<-which(ojo2[,1]==ojo1[i,1])
  
  if(length(pos)==0) {ojo1[i,2]<-1}
  else {
    ojo1[i,2]<-ojo2[pos,2]/ojo1[i,2]
  }
  
}
ojo<-ojo1
colnames(ojo)<-c("X","AM")
o<-read.csv("C:/Users/13663/Desktop/landgrid")
o<-o[,2:3]
a<-intersect(ojo[,1],o[,1])
n<-as.numeric()
for (i in 1:length(a)) {
  pos3<-which(ojo[,1]==a[i])
  n<-c(n,pos3)
}
ojo<-ojo[n,]
ojo<-na.omit(ojo)
oo<-read.csv("F:/Function/spdis/amphibian_threat&ddgrid.csv")
oo[,3]<-0
oo<-oo[,-1]
colnames(oo)<-c("X","AM")

a<-intersect(ojo[,1],oo[,1])
n<-as.numeric()
for (i in 1:length(a)) {
  pos3<-which(ojo[,1]==a[i])
  n<-c(n,pos3)
}
ojo<-ojo[n,]
o3<-ojo


ojo<-read.csv("F:/Function/kernel_alpha/reptile_allspecies.csv")
ojo<-ojo[,-1]
ojo<-ojo[,-3]
ojo<-na.omit(ojo)
ojo[,1]<-paste("g",ojo[,1],sep="")
ojo1<-ojo
ojo<-read.csv("F:/Function/kernel_alpha/reptile_change_FRic_ddsample.csv")
ojo<-ojo[,-1]
ojo<-ojo[,-3]
ojo<-na.omit(ojo)
ojo2<-ojo
for (i in 1:length(ojo1[,1])) {
  pos<-which(ojo2[,1]==ojo1[i,1])
  
  if(length(pos)==0) {ojo1[i,2]<-1}
  else {
    ojo1[i,2]<-ojo2[pos,2]/ojo1[i,2]
  }
  
}
ojo<-ojo1
colnames(ojo)<-c("X","RE")
o<-read.csv("C:/Users/13663/Desktop/landgrid")
o<-o[,2:3]
a<-intersect(ojo[,1],o[,1])
n<-as.numeric()
for (i in 1:length(a)) {
  pos3<-which(ojo[,1]==a[i])
  n<-c(n,pos3)
}
ojo<-ojo[n,]
ojo<-na.omit(ojo)
oo<-read.csv("F:/Function/spdis/reptile_threat&ddgrid.csv")
oo[,3]<-0
oo<-oo[,-1]
colnames(oo)<-c("X","RE")

a<-intersect(ojo[,1],oo[,1])
n<-as.numeric()
for (i in 1:length(a)) {
  pos3<-which(ojo[,1]==a[i])
  n<-c(n,pos3)
}
ojo<-ojo[n,]
o4<-ojo


o1[,2]<-(o1[,2]-min(o1[,2]))/(max(o1[,2])-min(o1[,2]))
o2[,2]<-(o2[,2]-min(o2[,2]))/(max(o2[,2])-min(o2[,2]))
o3[,2]<-(o3[,2]-min(o3[,2]))/(max(o3[,2])-min(o3[,2]))
o4[,2]<-(o4[,2]-min(o4[,2]))/(max(o4[,2])-min(o4[,2]))

o<-join(o1,o2)
o<-join(o,o3)
o<-join(o,o4)
o[is.na(o[,1]),1]<-0
o[is.na(o[,2]),2]<-0
o[is.na(o[,3]),3]<-0
o[is.na(o[,4]),4]<-0
o[is.na(o[,5]),5]<-0
o[,6]<-(o[,2]+o[,3]+o[,4]+o[,5])/4

ojo<-o[,c(1,6)]
max475<-sort(ojo[,2],decreasing=T)[1:length(ojo[,2])]
max475<-unique(max475)
wg<-as.numeric()
for (i in 1:length(ojo[,2])) {
  pos<-which(ojo[,2]==max475[i])
  wg<-c(wg,pos)
}
wg1<-wg[1:456]###2.5%
wg2<-wg[457:912]###5%
wg3<-wg[913:1824]###10%
wg4<-wg[1825:3101]###17%
wg5<-wg[3102:5473]###30%
wg6<-wg[5474:9121]###50%
wg7<-wg[9122:13682]###75%
wg8<-wg[13682:length(ojo[,2])]###100%


ojo[wg1,2]<-1
ojo[wg2,2]<-2
ojo[wg3,2]<-3
ojo[wg4,2]<-4
ojo[wg5,2]<-5
ojo[wg6,2]<-6
ojo[wg7,2]<-7
ojo[wg8,2]<-8
colnames(ojo)<-c("id","number")
data <- join(grid50_data, ojo, type = "full")  
data$number<-as.factor(data$number)
p<-ggplot(data, aes(x = long, y = lat, group = group,fill = number)) +
  geom_polygon(colour="transparent") +
  scale_fill_manual(values =c("#F46D43","#FDAE61","#FEE090","#FFFFBF","#E0F3F8","#ABD9E9","#74ADD1","#4575B4"),na.value = "white",breaks = c("1","2","3","4","5","6","7","8"),labels=c("2.5%", "5%", "10%","17%","30%","50%","75%","100%")) +  
  labs(fill="")+
  theme(               
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
  )+
  geom_polygon(data = d,aes(x=long, y = lat, group = group),fill = "transparent", colour="gray", size=0.2) +
  theme(               
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
  )+
  theme(text=element_text(size=10, family="sans"))+
  guides(fill = guide_legend(keywidth = 1, keyheight = 1))
#mammal
ojo<-read.csv("F:/Function/kernel_alpha/mammal_allspecies.csv")
ojo<-ojo[,-1]
ojo<-ojo[,-3]
ojo<-na.omit(ojo)
pos<-which(ojo[,2]==1)
ojo<-ojo[-pos,]
ojo[,1]<-paste("g",ojo[,1],sep="")
ojo1<-ojo
ojo<-read.csv("F:/Function/kernel_alpha/mammal_change_FRic_ddsample.csv")
ojo<-ojo[,-1]
ojo<-ojo[,-3]
ojo<-na.omit(ojo)
ojo2<-ojo
for (i in 1:length(ojo1[,1])) {
  pos<-which(ojo2[,1]==ojo1[i,1])
  
  if(length(pos)==0) {ojo1[i,2]<-1}
  else {
    ojo1[i,2]<-ojo2[pos,2]/ojo1[i,2]
  }
  
}
ojo<-ojo1
colnames(ojo)<-c("X","x")
o<-read.csv("C:/Users/13663/Desktop/landgrid")
o<-o[,2:3]
a<-intersect(ojo[,1],o[,1])
n<-as.numeric()
for (i in 1:length(a)) {
  pos3<-which(ojo[,1]==a[i])
  n<-c(n,pos3)
}
ojo<-ojo[n,]
ojo<-na.omit(ojo)
colnames(ojo)<-c("X","TM")
oo<-read.csv("F:/Function/spdis/mammal_threat&ddgrid.csv")
oo[,3]<-0
oo<-oo[,-1]
colnames(oo)<-c("X","TM")

a<-intersect(ojo[,1],oo[,1])
n<-as.numeric()
for (i in 1:length(a)) {
  pos3<-which(ojo[,1]==a[i])
  n<-c(n,pos3)
}
ojo<-ojo[n,]
o1<-ojo


ojo<-read.csv("F:/Function/kernel_alpha/bird_allspecies.csv")
ojo<-ojo[,-1]
ojo<-ojo[,-3]
ojo<-na.omit(ojo)
ojo[,1]<-paste("g",ojo[,1],sep="")
ojo1<-ojo
ojo<-read.csv("F:/Function/kernel_alpha/bird_change_FRic_ddsample.csv")
ojo<-ojo[,-1]
ojo<-ojo[,-3]
ojo<-na.omit(ojo)
ojo2<-ojo
for (i in 1:length(ojo1[,1])) {
  pos<-which(ojo2[,1]==ojo1[i,1])
  
  if(length(pos)==0) {ojo1[i,2]<-1}
  else {
    ojo1[i,2]<-ojo2[pos,2]/ojo1[i,2]
  }
  
}
ojo<-ojo1
colnames(ojo)<-c("X","x")
o<-read.csv("C:/Users/13663/Desktop/landgrid")
o<-o[,2:3]
a<-intersect(ojo[,1],o[,1])
n<-as.numeric()
for (i in 1:length(a)) {
  pos3<-which(ojo[,1]==a[i])
  n<-c(n,pos3)
}
ojo<-ojo[n,]
ojo<-na.omit(ojo)
colnames(ojo)<-c("X","Bird")
oo<-read.csv("F:/Function/spdis/bird_threat&ddgrid.csv")
oo[,3]<-0
oo<-oo[,-1]
colnames(oo)<-c("X","Bird")
o<-read.csv("C:/Users/13663/Desktop/landgrid")
o<-o[,2:3]
a<-intersect(oo[,1],o[,1])
n<-as.numeric()
for (i in 1:length(a)) {
  pos3<-which(oo[,1]==a[i])
  n<-c(n,pos3)
}
oo<-oo[n,]

a<-intersect(ojo[,1],oo[,1])
n<-as.numeric()
for (i in 1:length(a)) {
  pos3<-which(ojo[,1]==a[i])
  n<-c(n,pos3)
}
ojo<-ojo[n,]
o2<-ojo


ojo<-read.csv("F:/Function/kernel_alpha/amphibian_allspecies.csv")
ojo<-ojo[,-1]
ojo<-ojo[,-3]
ojo<-na.omit(ojo)
ojo[,1]<-paste("g",ojo[,1],sep="")
ojo1<-ojo
ojo<-read.csv("F:/Function/kernel_alpha/amphibian_change_FRic_ddsample.csv")
ojo<-ojo[,-1]
ojo<-ojo[,-3]
ojo<-na.omit(ojo)
ojo2<-ojo
for (i in 1:length(ojo1[,1])) {
  pos<-which(ojo2[,1]==ojo1[i,1])
  
  if(length(pos)==0) {ojo1[i,2]<-1}
  else {
    ojo1[i,2]<-ojo2[pos,2]/ojo1[i,2]
  }
  
}
ojo<-ojo1
colnames(ojo)<-c("X","AM")
o<-read.csv("C:/Users/13663/Desktop/landgrid")
o<-o[,2:3]
a<-intersect(ojo[,1],o[,1])
n<-as.numeric()
for (i in 1:length(a)) {
  pos3<-which(ojo[,1]==a[i])
  n<-c(n,pos3)
}
ojo<-ojo[n,]
ojo<-na.omit(ojo)
oo<-read.csv("F:/Function/spdis/amphibian_threat&ddgrid.csv")
oo[,3]<-0
oo<-oo[,-1]
colnames(oo)<-c("X","AM")

a<-intersect(ojo[,1],oo[,1])
n<-as.numeric()
for (i in 1:length(a)) {
  pos3<-which(ojo[,1]==a[i])
  n<-c(n,pos3)
}
ojo<-ojo[n,]
o3<-ojo


ojo<-read.csv("F:/Function/kernel_alpha/reptile_allspecies.csv")
ojo<-ojo[,-1]
ojo<-ojo[,-3]
ojo<-na.omit(ojo)
ojo[,1]<-paste("g",ojo[,1],sep="")
ojo1<-ojo
ojo<-read.csv("F:/Function/kernel_alpha/reptile_change_FRic_ddsample.csv")
ojo<-ojo[,-1]
ojo<-ojo[,-3]
ojo<-na.omit(ojo)
ojo2<-ojo
for (i in 1:length(ojo1[,1])) {
  pos<-which(ojo2[,1]==ojo1[i,1])
  
  if(length(pos)==0) {ojo1[i,2]<-1}
  else {
    ojo1[i,2]<-ojo2[pos,2]/ojo1[i,2]
  }
  
}
ojo<-ojo1
colnames(ojo)<-c("X","RE")
o<-read.csv("C:/Users/13663/Desktop/landgrid")
o<-o[,2:3]
a<-intersect(ojo[,1],o[,1])
n<-as.numeric()
for (i in 1:length(a)) {
  pos3<-which(ojo[,1]==a[i])
  n<-c(n,pos3)
}
ojo<-ojo[n,]
ojo<-na.omit(ojo)
oo<-read.csv("F:/Function/spdis/reptile_threat&ddgrid.csv")
oo[,3]<-0
oo<-oo[,-1]
colnames(oo)<-c("X","RE")

a<-intersect(ojo[,1],oo[,1])
n<-as.numeric()
for (i in 1:length(a)) {
  pos3<-which(ojo[,1]==a[i])
  n<-c(n,pos3)
}
ojo<-ojo[n,]
o4<-ojo

ojo<-o1
max475<-sort(ojo[,2],decreasing=T)[1:length(ojo[,2])]
max475<-unique(max475)
wg<-as.numeric()
for (i in 1:length(ojo[,2])) {
  pos<-which(ojo[,2]==max475[i])
  wg<-c(wg,pos)
}
wg1<-wg[1:round(length(ojo[,2])*0.025)]###2.5%
wg2<-wg[(round(length(ojo[,2])*0.025)+1):round(length(ojo[,2])*0.05)]###5%
wg3<-wg[(round(length(ojo[,2])*0.05)+1):round(length(ojo[,2])*0.1)]###10%
wg4<-wg[(round(length(ojo[,2])*0.1)+1):round(length(ojo[,2])*0.17)]###17%
wg5<-wg[(round(length(ojo[,2])*0.17)+1):round(length(ojo[,2])*0.3)]###30%
wg6<-wg[(round(length(ojo[,2])*0.3)+1):round(length(ojo[,2])*0.5)]###50%
wg7<-wg[(round(length(ojo[,2])*0.5)+1):round(length(ojo[,2])*0.75)]###75%
wg8<-wg[(round(length(ojo[,2])*0.75)+1):length(ojo[,2])]###100%


ojo[wg1,2]<-1
ojo[wg2,2]<-2
ojo[wg3,2]<-3
ojo[wg4,2]<-4
ojo[wg5,2]<-5
ojo[wg6,2]<-6
ojo[wg7,2]<-7
ojo[wg8,2]<-8
colnames(ojo)<-c("id","number")
data <- join(grid50_data, ojo, type = "full")  
data$number<-as.factor(data$number)
p1<-ggplot(data, aes(x = long, y = lat, group = group,fill = number)) +
  geom_polygon(colour="transparent") +
  scale_fill_manual(values =c("#F46D43","#FDAE61","#FEE090","#FFFFBF","#E0F3F8","#ABD9E9","#74ADD1","#4575B4"),na.value = "white",breaks = c("1","2","3","4","5","6","7","8"),labels=c("2.5%", "5%", "10%","17%","30%","50%","75%","100%")) +  
  labs(fill="")+
  theme(               
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
  )+
  geom_polygon(data = d,aes(x=long, y = lat, group = group),fill = "transparent", colour="gray", size=0.2) +
  theme(               
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
  )+
  theme(text=element_text(size=10, family="sans"))+
  guides(fill = guide_legend(keywidth = 1, keyheight = 1))
#bird
ojo<-o2
max475<-sort(ojo[,2],decreasing=T)[1:length(ojo[,2])]
max475<-unique(max475)
wg<-as.numeric()
for (i in 1:length(ojo[,2])) {
  pos<-which(ojo[,2]==max475[i])
  wg<-c(wg,pos)
}
wg1<-wg[1:round(length(ojo[,2])*0.025)]###2.5%
wg2<-wg[(round(length(ojo[,2])*0.025)+1):round(length(ojo[,2])*0.05)]###5%
wg3<-wg[(round(length(ojo[,2])*0.05)+1):round(length(ojo[,2])*0.1)]###10%
wg4<-wg[(round(length(ojo[,2])*0.1)+1):round(length(ojo[,2])*0.17)]###17%
wg5<-wg[(round(length(ojo[,2])*0.17)+1):round(length(ojo[,2])*0.3)]###30%
wg6<-wg[(round(length(ojo[,2])*0.3)+1):round(length(ojo[,2])*0.5)]###50%
wg7<-wg[(round(length(ojo[,2])*0.5)+1):round(length(ojo[,2])*0.75)]###75%
wg8<-wg[(round(length(ojo[,2])*0.75)+1):length(ojo[,2])]###100%


ojo[wg1,2]<-1
ojo[wg2,2]<-2
ojo[wg3,2]<-3
ojo[wg4,2]<-4
ojo[wg5,2]<-5
ojo[wg6,2]<-6
ojo[wg7,2]<-7
ojo[wg8,2]<-8
colnames(ojo)<-c("id","number")
data <- join(grid50_data, ojo, type = "full")  
data$number<-as.factor(data$number)
p2<-ggplot(data, aes(x = long, y = lat, group = group,fill = number)) +
  geom_polygon(colour="transparent") +
  scale_fill_manual(values =c("#F46D43","#FDAE61","#FEE090","#FFFFBF","#E0F3F8","#ABD9E9","#74ADD1","#4575B4"),na.value = "white",breaks = c("1","2","3","4","5","6","7","8"),labels=c("2.5%", "5%", "10%","17%","30%","50%","75%","100%")) +  
  labs(fill="")+
  theme(               
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
  )+
  geom_polygon(data = d,aes(x=long, y = lat, group = group),fill = "transparent", colour="gray", size=0.2) +
  theme(               
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
  )+
  theme(text=element_text(size=10, family="sans"))+
  guides(fill = guide_legend(keywidth = 1, keyheight = 1))
##am
ojo<-o3
max475<-sort(ojo[,2],decreasing=T)[1:length(ojo[,2])]
max475<-unique(max475)
wg<-as.numeric()
for (i in 1:length(ojo[,2])) {
  pos<-which(ojo[,2]==max475[i])
  wg<-c(wg,pos)
}
wg1<-wg[1:round(length(ojo[,2])*0.025)]###2.5%
wg2<-wg[(round(length(ojo[,2])*0.025)+1):round(length(ojo[,2])*0.05)]###5%
wg3<-wg[(round(length(ojo[,2])*0.05)+1):round(length(ojo[,2])*0.1)]###10%
wg4<-wg[(round(length(ojo[,2])*0.1)+1):round(length(ojo[,2])*0.17)]###17%
wg5<-wg[(round(length(ojo[,2])*0.17)+1):round(length(ojo[,2])*0.3)]###30%
wg6<-wg[(round(length(ojo[,2])*0.3)+1):round(length(ojo[,2])*0.5)]###50%
wg7<-wg[(round(length(ojo[,2])*0.5)+1):round(length(ojo[,2])*0.75)]###75%
wg8<-wg[(round(length(ojo[,2])*0.75)+1):length(ojo[,2])]###100%


ojo[wg1,2]<-1
ojo[wg2,2]<-2
ojo[wg3,2]<-3
ojo[wg4,2]<-4
ojo[wg5,2]<-5
ojo[wg6,2]<-6
ojo[wg7,2]<-7
ojo[wg8,2]<-8
colnames(ojo)<-c("id","number")
data <- join(grid50_data, ojo, type = "full")  
data$number<-as.factor(data$number)
p3<-ggplot(data, aes(x = long, y = lat, group = group,fill = number)) +
  geom_polygon(colour="transparent") +
  scale_fill_manual(values =c("#F46D43","#FDAE61","#FEE090","#FFFFBF","#E0F3F8","#ABD9E9","#74ADD1","#4575B4"),na.value = "white",breaks = c("1","2","3","4","5","6","7","8"),labels=c("2.5%", "5%", "10%","17%","30%","50%","75%","100%")) +  
  labs(fill="")+
  theme(               
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
  )+
  geom_polygon(data = d,aes(x=long, y = lat, group = group),fill = "transparent", colour="gray", size=0.2) +
  theme(               
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
  )+
  theme(text=element_text(size=10, family="sans"))+
  guides(fill = guide_legend(keywidth = 1, keyheight = 1))
##re
ojo<-o4
max475<-sort(ojo[,2],decreasing=T)[1:length(ojo[,2])]
max475<-unique(max475)
wg<-as.numeric()
for (i in 1:length(ojo[,2])) {
  pos<-which(ojo[,2]==max475[i])
  wg<-c(wg,pos)
}
wg1<-wg[1:round(length(ojo[,2])*0.025)]###2.5%
wg2<-wg[(round(length(ojo[,2])*0.025)+1):round(length(ojo[,2])*0.05)]###5%
wg3<-wg[(round(length(ojo[,2])*0.05)+1):round(length(ojo[,2])*0.1)]###10%
wg4<-wg[(round(length(ojo[,2])*0.1)+1):round(length(ojo[,2])*0.17)]###17%
wg5<-wg[(round(length(ojo[,2])*0.17)+1):round(length(ojo[,2])*0.3)]###30%
wg6<-wg[(round(length(ojo[,2])*0.3)+1):round(length(ojo[,2])*0.5)]###50%
wg7<-wg[(round(length(ojo[,2])*0.5)+1):round(length(ojo[,2])*0.75)]###75%
wg8<-wg[(round(length(ojo[,2])*0.75)+1):length(ojo[,2])]###100%


ojo[wg1,2]<-1
ojo[wg2,2]<-2
ojo[wg3,2]<-3
ojo[wg4,2]<-4
ojo[wg5,2]<-5
ojo[wg6,2]<-6
ojo[wg7,2]<-7
ojo[wg8,2]<-8
colnames(ojo)<-c("id","number")
data <- join(grid50_data, ojo, type = "full")  
data$number<-as.factor(data$number)
p4<-ggplot(data, aes(x = long, y = lat, group = group,fill = number)) +
  geom_polygon(colour="transparent") +
  scale_fill_manual(values =c("#F46D43","#FDAE61","#FEE090","#FFFFBF","#E0F3F8","#ABD9E9","#74ADD1","#4575B4"),na.value = "white",breaks = c("1","2","3","4","5","6","7","8"),labels=c("2.5%", "5%", "10%","17%","30%","50%","75%","100%")) +  
  labs(fill="")+
  theme(               
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
  )+
  geom_polygon(data = d,aes(x=long, y = lat, group = group),fill = "transparent", colour="gray", size=0.2) +
  theme(               
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
  )+
  theme(text=element_text(size=10, family="sans"))+
  guides(fill = guide_legend(keywidth = 1, keyheight = 1))
library("cowplot")
library(ggpubr)
library(grid)
library(magick)
library(cowplot)
fig<-ggarrange(p,                                                
               ggarrange(p1,p2,p3,p4, ncol = 2, nrow = 2,
                         labels = c("b","c","d","e"),
                         font.label = list(size = 12, face = "bold"),legend="bottom",common.legend = T), # 第二行为箱线图和点图
               nrow = 2, 
               labels = "a" , 
               legend="none",
               font.label = list(size = 12, face = "bold")
) 


img1 <- image_read("F:/Function/fig_new/Felis silvestris catus.png")
img2 <- image_read("F:/Function/fig_new/Eremopterix australis.png")
img3 <- image_read("F:/Function/fig_new/Rheobatrachus vitellinus.png")
img4 <- image_read("F:/Function/fig_new/Saurodactylus brosseti.png")

img_grob1 <- rasterGrob(img1, interpolate=TRUE)
img_grob2 <- rasterGrob(img2, interpolate=TRUE)
img_grob3 <- rasterGrob(img3, interpolate=TRUE)
img_grob4 <- rasterGrob(img4, interpolate=TRUE)

ggdraw() +
  draw_plot(fig) +
  draw_grob(img_grob1, x = 0.07, y = 0.3, width = 0.05, height = 0.05)+
  draw_grob(img_grob2, x = 0.57, y = 0.3, width = 0.05, height = 0.05)+
  draw_grob(img_grob3, x = 0.07, y = 0.1,width = 0.05, height =0.05)+
  draw_grob(img_grob4, x = 0.57, y = 0.1, width = 0.05, height = 0.05)




###Fig 3

library(gratia)
library(ggplot2)

d1<-data1
d2<-data2
d3<-data3
d4<-data4
d1[,"group"]<-c("Mammal")
d2[,"group"]<-c("Bird")
d3[,"group"]<-c("Amphibian")
d4[,"group"]<-c("Reptile")
colnames(d1)[9]<-c("fe")
colnames(d2)[9]<-c("fe")
colnames(d3)[9]<-c("fe")
colnames(d4)[9]<-c("fe")
df<-rbind(d1,d2)
df<-rbind(df,d3)
df<-rbind(df,d4)



mat_seq <- seq(min(df$MAT), max(df$MAT), length.out =300)


coords <- list(
  Mammal    = c(lon = mean(data1$lon),    lat = mean(data1$lat)),
  Bird      = c(lon = mean(data2$lon),      lat = mean(data2$lat)),
  Amphibian = c(lon = mean(data3$lon), lat = mean(data3$lat)),
  Reptile   = c(lon = mean(data4$lon),   lat = mean(data4$lat))
)

models <- list(
  Mammal    = model_mammal1,
  Bird      = model_bird1,
  Amphibian = model_amphibian1,
  Reptile   = model_reptile1
)


pred_all <- bind_rows(lapply(names(models), function(g) {
  newdata <- data.frame(
    lon = coords[[g]]["lon"],
    lat = coords[[g]]["lat"],
    MAT = mat_seq
  )
  
  predictions(
    models[[g]],
    newdata = newdata,
    type = "response",
    exclude = "s(lon,lat)"
  ) %>%
    mutate(group = g)
}))
pred_all <- pred_all %>%
  mutate(group = factor(group, levels = c("Mammal", "Bird", "Amphibian", "Reptile")))
p1<-ggplot(pred_all, aes(x = MAT, y = estimate, color = group, fill = group)) +
  geom_line(size = 0.3) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
  labs(
    x = "MAT",
    y = "Erosion of functional diversity",
    color = "Group",
    fill = "Group"
  ) +
  theme_minimal() +
  theme(legend.position = "top")+
  theme(
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    axis.ticks   = element_line(color = "black")
  )

MAP_seq <- seq(min(df$MAP), max(df$MAP), length.out =300)


coords <- list(
  Mammal    = c(lon = mean(data1$lon),    lat = mean(data1$lat)),
  Bird      = c(lon = mean(data2$lon),      lat = mean(data2$lat)),
  Amphibian = c(lon = mean(data3$lon), lat = mean(data3$lat)),
  Reptile   = c(lon = mean(data4$lon),   lat = mean(data4$lat))
)

models <- list(
  Mammal    = model_mammal2,
  Bird      = model_bird2,
  Amphibian = model_amphibian2,
  Reptile   = model_reptile2
)


pred_all <- bind_rows(lapply(names(models), function(g) {
  newdata <- data.frame(
    lon = coords[[g]]["lon"],
    lat = coords[[g]]["lat"],
    MAP = MAP_seq
  )
  
  predictions(
    models[[g]],
    newdata = newdata,
    type = "response",
    exclude = "s(lon,lat)"
  ) %>%
    mutate(group = g)
}))
pred_all <- pred_all %>%
  mutate(group = factor(group, levels = c("Mammal", "Bird", "Amphibian", "Reptile")))
p2<-ggplot(pred_all, aes(x = MAP, y = estimate, color = group, fill = group)) +
  geom_line(size = 0.3) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
  labs(
    x = "MAP",
    y = "",
    color = "Group",
    fill = "Group"
  ) +
  theme_minimal() +
  theme(legend.position = "top")+
  theme(
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    axis.ticks   = element_line(color = "black")
  )

TSN_seq <- seq(min(df$TSN), max(df$TSN), length.out =300)


coords <- list(
  Mammal    = c(lon = mean(data1$lon),    lat = mean(data1$lat)),
  Bird      = c(lon = mean(data2$lon),      lat = mean(data2$lat)),
  Amphibian = c(lon = mean(data3$lon), lat = mean(data3$lat)),
  Reptile   = c(lon = mean(data4$lon),   lat = mean(data4$lat))
)

models <- list(
  Mammal    = model_mammal3,
  Bird      = model_bird3,
  Amphibian = model_amphibian3,
  Reptile   = model_reptile3
)


pred_all <- bind_rows(lapply(names(models), function(g) {
  newdata <- data.frame(
    lon = coords[[g]]["lon"],
    lat = coords[[g]]["lat"],
    TSN = TSN_seq
  )
  
  predictions(
    models[[g]],
    newdata = newdata,
    type = "response",
    exclude = "s(lon,lat)"
  ) %>%
    mutate(group = g)
}))
pred_all <- pred_all %>%
  mutate(group = factor(group, levels = c("Mammal", "Bird", "Amphibian", "Reptile")))
p3<-ggplot(pred_all, aes(x = TSN, y = estimate, color = group, fill = group)) +
  geom_line(size = 0.3) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
  labs(
    x = "TSN",
    y = "",
    color = "Group",
    fill = "Group"
  ) +
  theme_minimal() +
  theme(legend.position = "top")+
  theme(
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    axis.ticks   = element_line(color = "black")
  )

PSN_seq <- seq(min(df$PSN), max(df$PSN), length.out =300)


coords <- list(
  Mammal    = c(lon = mean(data1$lon),    lat = mean(data1$lat)),
  Bird      = c(lon = mean(data2$lon),      lat = mean(data2$lat)),
  Amphibian = c(lon = mean(data3$lon), lat = mean(data3$lat)),
  Reptile   = c(lon = mean(data4$lon),   lat = mean(data4$lat))
)

models <- list(
  Mammal    = model_mammal4,
  Bird      = model_bird4,
  Amphibian = model_amphibian4,
  Reptile   = model_reptile4
)


pred_all <- bind_rows(lapply(names(models), function(g) {
  newdata <- data.frame(
    lon = coords[[g]]["lon"],
    lat = coords[[g]]["lat"],
    PSN = PSN_seq
  )
  
  predictions(
    models[[g]],
    newdata = newdata,
    type = "response",
    exclude = "s(lon,lat)"
  ) %>%
    mutate(group = g)
}))
pred_all <- pred_all %>%
  mutate(group = factor(group, levels = c("Mammal", "Bird", "Amphibian", "Reptile")))
p4<-ggplot(pred_all, aes(x = PSN, y = estimate, color = group, fill = group)) +
  geom_line(size = 0.3) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
  labs(
    x = "PSN",
    y = "Erosion of functional diversity",
    color = "Group",
    fill = "Group"
  ) +
  theme_minimal() +
  theme(legend.position = "top")+
  theme(
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    axis.ticks   = element_line(color = "black")
  )

HFP_seq <- seq(min(df$HFP), max(df$HFP), length.out =300)


coords <- list(
  Mammal    = c(lon = mean(data1$lon),    lat = mean(data1$lat)),
  Bird      = c(lon = mean(data2$lon),      lat = mean(data2$lat)),
  Amphibian = c(lon = mean(data3$lon), lat = mean(data3$lat)),
  Reptile   = c(lon = mean(data4$lon),   lat = mean(data4$lat))
)

models <- list(
  Mammal    = model_mammal5,
  Bird      = model_bird5,
  Amphibian = model_amphibian5,
  Reptile   = model_reptile5
)


pred_all <- bind_rows(lapply(names(models), function(g) {
  newdata <- data.frame(
    lon = coords[[g]]["lon"],
    lat = coords[[g]]["lat"],
    HFP = HFP_seq
  )
  
  predictions(
    models[[g]],
    newdata = newdata,
    type = "response",
    exclude = "s(lon,lat)"
  ) %>%
    mutate(group = g)
}))
pred_all <- pred_all %>%
  mutate(group = factor(group, levels = c("Mammal", "Bird", "Amphibian", "Reptile")))
p5<-ggplot(pred_all, aes(x = HFP, y = estimate, color = group, fill = group)) +
  geom_line(size = 0.3) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
  labs(
    x = "HFP",
    y = "",
    color = "Group",
    fill = "Group"
  ) +
  theme_minimal() +
  theme(legend.position = "top")+
  theme(
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    axis.ticks   = element_line(color = "black")
  )

HFP_change_seq <- seq(min(df$HFP_change), max(df$HFP_change), length.out =300)


coords <- list(
  Mammal    = c(lon = mean(data1$lon),    lat = mean(data1$lat)),
  Bird      = c(lon = mean(data2$lon),      lat = mean(data2$lat)),
  Amphibian = c(lon = mean(data3$lon), lat = mean(data3$lat)),
  Reptile   = c(lon = mean(data4$lon),   lat = mean(data4$lat))
)

models <- list(
  Mammal    = model_mammal6,
  Bird      = model_bird6,
  Amphibian = model_amphibian6,
  Reptile   = model_reptile6
)

pred_all <- bind_rows(lapply(names(models), function(g) {
  newdata <- data.frame(
    lon = coords[[g]]["lon"],
    lat = coords[[g]]["lat"],
    HFP_change = HFP_change_seq
  )
  
  predictions(
    models[[g]],
    newdata = newdata,
    type = "response",
    exclude = "s(lon,lat)"
  ) %>%
    mutate(group = g)
}))
pred_all <- pred_all %>%
  mutate(group = factor(group, levels = c("Mammal", "Bird", "Amphibian", "Reptile")))
p6<-ggplot(pred_all, aes(x = HFP_change, y = estimate, color = group, fill = group)) +
  geom_line(size = 0.3) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2, color = NA) +
  labs(
    x = "HFP_change",
    y = "",
    color = "Group",
    fill = "Group"
  ) +
  theme_minimal() +
  theme(legend.position = "top")+
  theme(
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "black"),
    axis.ticks   = element_line(color = "black")
  )

library(ggpubr)
p<- ggarrange(p1,p2,p3,p4,p5,p6, ncol = 3, nrow = 2,
              labels = c("a","b","c","d","e","f"), 
              font.label = list(size = 12, face = "bold"),legend="bottom",common.legend = T)
ggsave("F:/Function/fig_revision/gam_kernel_Fric_ddsample.png", plot = p, width = 11, height = 6.5, dpi = 600,bg = "white")


###Fig 4
data1<-read.csv("C:/Users/13663/Desktop/hfp2022.csv")
data1<-data1[,c(2,8)]
data1[,1]<-paste("g",data1[,1],sep="")


data2<-read.csv("F:/Function/climatechange_chelsa585.csv")
data2<-data2[,-1]
data2[,1]<-paste("g",data2[,1],sep="")

data3<-read.csv("F:/Function/climatechange_chelsa126.csv")
data3<-data3[,-1]
data3[,1]<-paste("g",data3[,1],sep="")

ojo<-data1
max475<-sort(ojo[,2],decreasing=T)[1:length(ojo[,2])]
max475<-unique(max475)
wg<-as.numeric()
for (i in 1:length(ojo[,2])) {
  pos<-which(ojo[,2]==max475[i])
  wg<-c(wg,pos)
}
wg1<-wg[1:1843]###10%
hfphot<-ojo[wg1,1]

ojo<-data2
max475<-sort(ojo[,2],decreasing=T)[1:length(ojo[,2])]
max475<-unique(max475)
wg<-as.numeric()
for (i in 1:length(ojo[,2])) {
  pos<-which(ojo[,2]==max475[i])
  wg<-c(wg,pos)
}
wg1<-wg[1:1843]###10%
cchot585<-ojo[wg1,1]

ojo<-data3
max475<-sort(ojo[,2],decreasing=T)[1:length(ojo[,2])]
max475<-unique(max475)
wg<-as.numeric()
for (i in 1:length(ojo[,2])) {
  pos<-which(ojo[,2]==max475[i])
  wg<-c(wg,pos)
}
wg1<-wg[1:1843]###10%
cchot126<-ojo[wg1,1]

ojo<-read.csv("F:/Function/kernel_alpha/mammal_allspecies.csv")
ojo<-ojo[,-1]
ojo<-ojo[,-3]
ojo<-na.omit(ojo)
pos<-which(ojo[,2]==1)
ojo<-ojo[-pos,]
ojo[,1]<-paste("g",ojo[,1],sep="")
ojo1<-ojo
ojo<-read.csv("F:/Function/kernel_alpha/mammal_change_Fric_ddsample.csv")
ojo<-ojo[,-1]
ojo<-ojo[,-3]
ojo<-na.omit(ojo)
ojo2<-ojo
for (i in 1:length(ojo1[,1])) {
  pos<-which(ojo2[,1]==ojo1[i,1])
  
  if(length(pos)==0) {ojo1[i,2]<-1}
  else {
    ojo1[i,2]<-ojo2[pos,2]/ojo1[i,2]
  }
  
}
ojo<-ojo1
colnames(ojo)<-c("X","x")
o<-read.csv("C:/Users/13663/Desktop/landgrid")
o<-o[,2:3]
a<-intersect(ojo[,1],o[,1])
n<-as.numeric()
for (i in 1:length(a)) {
  pos3<-which(ojo[,1]==a[i])
  n<-c(n,pos3)
}
ojo<-ojo[n,]
ojo<-na.omit(ojo)
colnames(ojo)<-c("X","TM")
oo<-read.csv("F:/Function/spdis/mammal_threat&ddgrid.csv")
oo[,3]<-0
oo<-oo[,-1]
colnames(oo)<-c("X","TM")

a<-intersect(ojo[,1],oo[,1])
n<-as.numeric()
for (i in 1:length(a)) {
  pos3<-which(ojo[,1]==a[i])
  n<-c(n,pos3)
}
ojo<-ojo[n,]
o1<-ojo


ojo<-read.csv("F:/Function/kernel_alpha/bird_allspecies.csv")
ojo<-ojo[,-1]
ojo<-ojo[,-3]
ojo<-na.omit(ojo)
ojo[,1]<-paste("g",ojo[,1],sep="")
ojo1<-ojo
ojo<-read.csv("F:/Function/kernel_alpha/bird_change_Fric_ddsample.csv")
ojo<-ojo[,-1]
ojo<-ojo[,-3]
ojo<-na.omit(ojo)
ojo2<-ojo
for (i in 1:length(ojo1[,1])) {
  pos<-which(ojo2[,1]==ojo1[i,1])
  
  if(length(pos)==0) {ojo1[i,2]<-1}
  else {
    ojo1[i,2]<-ojo2[pos,2]/ojo1[i,2]
  }
  
}
ojo<-ojo1
colnames(ojo)<-c("X","x")
o<-read.csv("C:/Users/13663/Desktop/landgrid")
o<-o[,2:3]
a<-intersect(ojo[,1],o[,1])
n<-as.numeric()
for (i in 1:length(a)) {
  pos3<-which(ojo[,1]==a[i])
  n<-c(n,pos3)
}
ojo<-ojo[n,]
ojo<-na.omit(ojo)
colnames(ojo)<-c("X","Bird")
oo<-read.csv("F:/Function/spdis/bird_threat&ddgrid.csv")
oo[,3]<-0
oo<-oo[,-1]
colnames(oo)<-c("X","Bird")
o<-read.csv("C:/Users/13663/Desktop/landgrid")
o<-o[,2:3]
a<-intersect(oo[,1],o[,1])
n<-as.numeric()
for (i in 1:length(a)) {
  pos3<-which(oo[,1]==a[i])
  n<-c(n,pos3)
}
oo<-oo[n,]

a<-intersect(ojo[,1],oo[,1])
n<-as.numeric()
for (i in 1:length(a)) {
  pos3<-which(ojo[,1]==a[i])
  n<-c(n,pos3)
}
ojo<-ojo[n,]
o2<-ojo


ojo<-read.csv("F:/Function/kernel_alpha/amphibian_allspecies.csv")
ojo<-ojo[,-1]
ojo<-ojo[,-3]
ojo<-na.omit(ojo)
ojo[,1]<-paste("g",ojo[,1],sep="")
ojo1<-ojo
ojo<-read.csv("F:/Function/kernel_alpha/amphibian_change_Fric_ddsample.csv")
ojo<-ojo[,-1]
ojo<-ojo[,-3]
ojo<-na.omit(ojo)
ojo2<-ojo
for (i in 1:length(ojo1[,1])) {
  pos<-which(ojo2[,1]==ojo1[i,1])
  
  if(length(pos)==0) {ojo1[i,2]<-1}
  else {
    ojo1[i,2]<-ojo2[pos,2]/ojo1[i,2]
  }
  
}
ojo<-ojo1
colnames(ojo)<-c("X","AM")
o<-read.csv("C:/Users/13663/Desktop/landgrid")
o<-o[,2:3]
a<-intersect(ojo[,1],o[,1])
n<-as.numeric()
for (i in 1:length(a)) {
  pos3<-which(ojo[,1]==a[i])
  n<-c(n,pos3)
}
ojo<-ojo[n,]
ojo<-na.omit(ojo)
oo<-read.csv("F:/Function/spdis/amphibian_threat&ddgrid.csv")
oo[,3]<-0
oo<-oo[,-1]
colnames(oo)<-c("X","AM")

a<-intersect(ojo[,1],oo[,1])
n<-as.numeric()
for (i in 1:length(a)) {
  pos3<-which(ojo[,1]==a[i])
  n<-c(n,pos3)
}
ojo<-ojo[n,]
o3<-ojo


ojo<-read.csv("F:/Function/kernel_alpha/reptile_allspecies.csv")
ojo<-ojo[,-1]
ojo<-ojo[,-3]
ojo<-na.omit(ojo)
ojo[,1]<-paste("g",ojo[,1],sep="")
ojo1<-ojo
ojo<-read.csv("F:/Function/kernel_alpha/reptile_change_Fric_ddsample.csv")
ojo<-ojo[,-1]
ojo<-ojo[,-3]
ojo<-na.omit(ojo)
ojo2<-ojo
for (i in 1:length(ojo1[,1])) {
  pos<-which(ojo2[,1]==ojo1[i,1])
  
  if(length(pos)==0) {ojo1[i,2]<-1}
  else {
    ojo1[i,2]<-ojo2[pos,2]/ojo1[i,2]
  }
  
}
ojo<-ojo1
colnames(ojo)<-c("X","RE")
o<-read.csv("C:/Users/13663/Desktop/landgrid")
o<-o[,2:3]
a<-intersect(ojo[,1],o[,1])
n<-as.numeric()
for (i in 1:length(a)) {
  pos3<-which(ojo[,1]==a[i])
  n<-c(n,pos3)
}
ojo<-ojo[n,]
ojo<-na.omit(ojo)
oo<-read.csv("F:/Function/spdis/reptile_threat&ddgrid.csv")
oo[,3]<-0
oo<-oo[,-1]
colnames(oo)<-c("X","RE")

a<-intersect(ojo[,1],oo[,1])
n<-as.numeric()
for (i in 1:length(a)) {
  pos3<-which(ojo[,1]==a[i])
  n<-c(n,pos3)
}
ojo<-ojo[n,]
o4<-ojo

library(terra)
pa<-rast("F:/Function/PA_all.tif")
pos<-which(values(pa)[]>1000)
pagrid<-paste("g",pos,sep="")
land<-read.csv("F:/Function/spdis/mammal_allgrid.csv")
land_pa<-intersect(land[,2],pagrid)
####585
ojo<-o1
ojo<-na.omit(ojo)
max475<-sort(ojo[,2],decreasing=T)[1:length(ojo[,2])]
max475<-unique(max475)
wg<-as.numeric()
for (i in 1:length(ojo[,2])) {
  pos<-which(ojo[,2]==max475[i])
  wg<-c(wg,pos)
}
wg1<-wg[1:1843]###10%
erosionhot<-ojo[wg1,1]

pa_grid<-intersect(land_pa,erosionhot)
no_pa<-setdiff(erosionhot,pa_grid)

inte_all<-intersect(hfphot,no_pa)
inte_all<-intersect(inte_all,cchot585)
inte1<-intersect(hfphot,no_pa)
inte1<-setdiff(inte1,inte_all)
inte2<-intersect(cchot585,no_pa)
inte2<-setdiff(inte2,inte_all)

length(pa_grid)/1843
(length(inte1)+length(inte_all))/length(no_pa)
(length(inte2)+length(inte_all))/length(no_pa)

p<-as.numeric()
for (i in 1:length(erosionhot)) {
  pos<-which(ojo[,1]==erosionhot[i])
  p<-c(p,pos)
}
pos1<-as.numeric()
for (i in 1:length(inte_all)) {
  pos<-which(ojo[,1]==inte_all[i])
  pos1<-c(pos1,pos)
}
pos2<-as.numeric()
for (i in 1:length(inte1)) {
  pos<-which(ojo[,1]==inte1[i])
  pos2<-c(pos2,pos)
}
pos3<-as.numeric()
for (i in 1:length(inte2)) {
  pos<-which(ojo[,1]==inte2[i])
  pos3<-c(pos3,pos)
}
pos4<-as.numeric()
for (i in 1:length(pa_grid)) {
  pos<-which(ojo[,1]==pa_grid[i])
  pos4<-c(pos4,pos)
}


ojo[,2]<-1
ojo[pos1,2]<-2
ojo[pos2,2]<-3
ojo[pos3,2]<-4
ojo[pos4,2]<-5
ojo<-ojo[p,]
colnames(ojo)<-c("id","number")
data <- join(grid50_data, ojo, type = "full")  

data$number<-as.factor(data$number)
p1<-ggplot(data, aes(x = long, y = lat, group = group,fill = number)) +
  geom_polygon(colour="transparent") +
  scale_fill_manual(values =c("#4575B4","#ABD9E9","#FEE090","#FDAE61","#F46D43"),na.value = "white",breaks = c("5","1","3","4","2"),labels=c("Protected areas","Functional erosion hotspots", "Human footprint hotspots", "Climate change hotspots","Human footprint&Climate change")) +  
  labs(fill="")+
  theme(               
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
  )+
  geom_polygon(data = d,aes(x=long, y = lat, group = group),fill = "transparent", colour="gray", size=0.2) +
  theme(               
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
  )

ojo<-o2
ojo<-na.omit(ojo)
max475<-sort(ojo[,2],decreasing=T)[1:length(ojo[,2])]
max475<-unique(max475)
wg<-as.numeric()
for (i in 1:length(ojo[,2])) {
  pos<-which(ojo[,2]==max475[i])
  wg<-c(wg,pos)
}
wg1<-wg[1:1843]###2.5%
erosionhot<-ojo[wg1,1]

pa_grid<-intersect(land_pa,erosionhot)
no_pa<-setdiff(erosionhot,pa_grid)

inte_all<-intersect(hfphot,no_pa)
inte_all<-intersect(inte_all,cchot585)
inte1<-intersect(hfphot,no_pa)
inte1<-setdiff(inte1,inte_all)
inte2<-intersect(cchot585,no_pa)
inte2<-setdiff(inte2,inte_all)



p<-as.numeric()
for (i in 1:length(erosionhot)) {
  pos<-which(ojo[,1]==erosionhot[i])
  p<-c(p,pos)
}
pos1<-as.numeric()
for (i in 1:length(inte_all)) {
  pos<-which(ojo[,1]==inte_all[i])
  pos1<-c(pos1,pos)
}
pos2<-as.numeric()
for (i in 1:length(inte1)) {
  pos<-which(ojo[,1]==inte1[i])
  pos2<-c(pos2,pos)
}
pos3<-as.numeric()
for (i in 1:length(inte2)) {
  pos<-which(ojo[,1]==inte2[i])
  pos3<-c(pos3,pos)
}
pos4<-as.numeric()
for (i in 1:length(pa_grid)) {
  pos<-which(ojo[,1]==pa_grid[i])
  pos4<-c(pos4,pos)
}


ojo[,2]<-1

ojo[pos2,2]<-3
ojo[pos3,2]<-4
ojo[pos4,2]<-5
ojo<-ojo[p,]
colnames(ojo)<-c("id","number")
data <- join(grid50_data, ojo, type = "full")  

data$number<-as.factor(data$number)
p2<-ggplot(data, aes(x = long, y = lat, group = group,fill = number)) +
  geom_polygon(colour="transparent") +
  scale_fill_manual(values =c("#4575B4","#ABD9E9","#FEE090","#FDAE61","#F46D43"),na.value = "white",breaks = c("5","1","3","4","2"),labels=c("Protected areas","Functional erosion hotspots", "Human footprint hotspots", "Climate change hotspots","Human footprint&Climate change")) +  
  labs(fill="")+
  theme(               
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
  )+
  geom_polygon(data = d,aes(x=long, y = lat, group = group),fill = "transparent", colour="gray", size=0.2) +
  theme(               
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
  )

ojo<-o3
ojo<-na.omit(ojo)
max475<-sort(ojo[,2],decreasing=T)[1:length(ojo[,2])]
max475<-unique(max475)
wg<-as.numeric()
for (i in 1:length(ojo[,2])) {
  pos<-which(ojo[,2]==max475[i])
  wg<-c(wg,pos)
}
wg1<-wg[1:1843]###2.5%
erosionhot<-ojo[wg1,1]

pa_grid<-intersect(land_pa,erosionhot)
no_pa<-setdiff(erosionhot,pa_grid)

inte_all<-intersect(hfphot,no_pa)
inte_all<-intersect(inte_all,cchot585)
inte1<-intersect(hfphot,no_pa)
inte1<-setdiff(inte1,inte_all)
inte2<-intersect(cchot585,no_pa)
inte2<-setdiff(inte2,inte_all)


p<-as.numeric()
for (i in 1:length(erosionhot)) {
  pos<-which(ojo[,1]==erosionhot[i])
  p<-c(p,pos)
}
pos1<-as.numeric()
for (i in 1:length(inte_all)) {
  pos<-which(ojo[,1]==inte_all[i])
  pos1<-c(pos1,pos)
}
pos2<-as.numeric()
for (i in 1:length(inte1)) {
  pos<-which(ojo[,1]==inte1[i])
  pos2<-c(pos2,pos)
}
pos3<-as.numeric()
for (i in 1:length(inte2)) {
  pos<-which(ojo[,1]==inte2[i])
  pos3<-c(pos3,pos)
}
pos4<-as.numeric()
for (i in 1:length(pa_grid)) {
  pos<-which(ojo[,1]==pa_grid[i])
  pos4<-c(pos4,pos)
}


ojo[,2]<-1
ojo[pos1,2]<-2
ojo[pos2,2]<-3
ojo[pos3,2]<-4
ojo[pos4,2]<-5
ojo<-ojo[p,]
colnames(ojo)<-c("id","number")
data <- join(grid50_data, ojo, type = "full")  

data$number<-as.factor(data$number)
p3<-ggplot(data, aes(x = long, y = lat, group = group,fill = number)) +
  geom_polygon(colour="transparent") +
  scale_fill_manual(values =c("#4575B4","#ABD9E9","#FEE090","#FDAE61","#F46D43"),na.value = "white",breaks = c("5","1","3","4","2"),labels=c("Protected areas","Functional erosion hotspots", "Human footprint hotspots", "Climate change hotspots","Human footprint&Climate change")) +  
  labs(fill="")+
  theme(               
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
  )+
  geom_polygon(data = d,aes(x=long, y = lat, group = group),fill = "transparent", colour="gray", size=0.2) +
  theme(               
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
  )

ojo<-o4
ojo<-na.omit(ojo)
max475<-sort(ojo[,2],decreasing=T)[1:length(ojo[,2])]
max475<-unique(max475)
wg<-as.numeric()
for (i in 1:length(ojo[,2])) {
  pos<-which(ojo[,2]==max475[i])
  wg<-c(wg,pos)
}
wg1<-wg[1:1843]###2.5%
erosionhot<-ojo[wg1,1]

pa_grid<-intersect(land_pa,erosionhot)
no_pa<-setdiff(erosionhot,pa_grid)

inte_all<-intersect(hfphot,no_pa)
inte_all<-intersect(inte_all,cchot585)
inte1<-intersect(hfphot,no_pa)
inte1<-setdiff(inte1,inte_all)
inte2<-intersect(cchot585,no_pa)
inte2<-setdiff(inte2,inte_all)


p<-as.numeric()
for (i in 1:length(erosionhot)) {
  pos<-which(ojo[,1]==erosionhot[i])
  p<-c(p,pos)
}
pos1<-as.numeric()
for (i in 1:length(inte_all)) {
  pos<-which(ojo[,1]==inte_all[i])
  pos1<-c(pos1,pos)
}
pos2<-as.numeric()
for (i in 1:length(inte1)) {
  pos<-which(ojo[,1]==inte1[i])
  pos2<-c(pos2,pos)
}
pos3<-as.numeric()
for (i in 1:length(inte2)) {
  pos<-which(ojo[,1]==inte2[i])
  pos3<-c(pos3,pos)
}
pos4<-as.numeric()
for (i in 1:length(pa_grid)) {
  pos<-which(ojo[,1]==pa_grid[i])
  pos4<-c(pos4,pos)
}


ojo[,2]<-1
ojo[pos1,2]<-2
ojo[pos2,2]<-3
ojo[pos3,2]<-4
ojo[pos4,2]<-5
ojo<-ojo[p,]
colnames(ojo)<-c("id","number")
data <- join(grid50_data, ojo, type = "full")  

data$number<-as.factor(data$number)
p4<-ggplot(data, aes(x = long, y = lat, group = group,fill = number)) +
  geom_polygon(colour="transparent") +
  scale_fill_manual(values =c("#4575B4","#ABD9E9","#FEE090","#FDAE61","#F46D43"),na.value = "white",breaks = c("5","1","3","4","2"),labels=c("Protected areas","Functional erosion hotspots", "Human footprint hotspots", "Climate change hotspots","Human footprint&Climate change")) +  
  labs(fill="")+
  theme(               
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
  )+
  geom_polygon(data = d,aes(x=long, y = lat, group = group),fill = "transparent", colour="gray", size=0.2) +
  theme(               
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
  )
library(ggpubr)
p<-ggarrange(p1, p2, p3,p4, ncol = 2, nrow = 2,
             labels = c("a","b","c","d"), 
             font.label = list(size = 12, face = "bold"),common.legend = T, legend="left")

library(grid)
library(magick)
library(cowplot)
img1 <- image_read("F:/Function/fig_new/Felis silvestris catus.png")
img2 <- image_read("F:/Function/fig_new/Eremopterix australis.png")
img3 <- image_read("F:/Function/fig_new/Rheobatrachus vitellinus.png")
img4 <- image_read("F:/Function/fig_new/Saurodactylus brosseti.png")

img_grob1 <- rasterGrob(img1, interpolate=TRUE)
img_grob2 <- rasterGrob(img2, interpolate=TRUE)
img_grob3 <- rasterGrob(img3, interpolate=TRUE)
img_grob4 <- rasterGrob(img4, interpolate=TRUE)

ggdraw() +
  draw_plot(p) +
  draw_grob(img_grob1, x = 0.17, y = 0.6, width = 0.12, height = 0.12)+
  draw_grob(img_grob2, x = 0.57, y = 0.6, width = 0.1, height = 0.1)+
  draw_grob(img_grob3, x = 0.17, y = 0.1,width = 0.1, height = 0.1)+
  draw_grob(img_grob4, x = 0.57, y = 0.1, width = 0.1, height = 0.1)


###Fig 5
library(terra)
pa<-rast("F:/Function/PA_all.tif")
pos<-which(values(pa)[]>1000)
pagrid<-paste("g",pos,sep="")
land<-read.csv("F:/Function/spdis/mammal_allgrid.csv")
land_pa<-intersect(land[,2],pagrid)
18430*0.3-3757
18430*0.5-3757
zonation<-rast("F:/Function/zonation_result/kernel_alpha_585/subregion_1/rankmap.tif")
values <- values(zonation)
values<-as.data.frame(values)
values[,2]<-rownames(values)
top_rows <- values[order(-values[[1]]), ][1:1772, ]
pa_30<-top_rows[,2]
pa_30<-paste("g",pa_30,sep="")



ojo<-read.csv("F:/Function/kernel_alpha/mammal_all_FRic.csv")
ojo<-ojo[,-1]
ojo<-ojo[,-3]
ojo[,1]<-paste("g",ojo[,1],sep="")

p<-as.numeric()
for (i in 1:length(land_pa)) {
  pos<-which(ojo[,1]==land_pa[i])
  p<-c(p,pos)
}
pos1<-as.numeric()
for (i in 1:length(pa_30)) {
  pos<-which(ojo[,1]==pa_30[i])
  pos1<-c(pos1,pos)
}

ojo[,2]<-1
ojo[pos1,2]<-2

ojo<-ojo[c(p,pos1),]
colnames(ojo)<-c("id","number")
data <- join(grid50_data, ojo, type = "full")  

data$number<-as.factor(data$number)

ggplot(data, aes(x = long, y = lat, group = group,fill = number)) +
  geom_polygon(colour="transparent") +
  scale_fill_manual(values =c("#4575B4","#F46D43"),na.value = "white",breaks = c("1","2"),labels=c("Current protected areas", "Priority areas for 30% protection target")) +  
  labs(fill="")+
  theme(               
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
  )+
  geom_polygon(data = d,aes(x=long, y = lat, group = group),fill = "transparent", colour="gray", size=0.2) +
  theme(               
    panel.grid = element_blank(),
    panel.background = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    axis.title = element_blank(),
  )+
  theme(
    legend.position = c(0.05, 0.1),     
    legend.justification = c(0, 0)
  )