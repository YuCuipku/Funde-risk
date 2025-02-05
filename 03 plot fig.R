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
library(tidyr)
ojo1<-read.csv("F:/Function/TM_all_FRic.csv")
ojo1<-ojo1[,-1]
ojo2<-read.csv("F:/Function/TM_change_FRic.csv")
ojo2<-ojo2[,-1]
for (i in 1:length(ojo1[,1])) {
  pos<-which(ojo2[,1]==ojo1[i,1])
  ojo1[i,2]<-ojo2[pos,2]/ojo1[i,2]
}
ojo<-ojo1
ojo<-na.omit(ojo)
colnames(ojo)<-c("id","pp")
ojo2<-read.csv("F:/Function/TM_change_FRic.csv")
ojo2<-ojo2[,-1]
colnames(ojo2)<-c("id","abs")
ojo<-join(ojo,ojo2)
pos<-which(ojo[,3]==0)
ojo<-ojo[-pos,]
o1<-ojo

quantiles_pp <- ojo %>%
  pull(pp) %>%
  quantile(probs = seq(0, 1, length.out = 11))

quantiles_abs <- ojo %>%
  pull(abs) %>%
  quantile(probs = seq(0, 1, length.out = 11))



bivariate_color_scale <- tibble(
  "1 - 10" ="#00FFFF","2 - 10" = "#11E6FD", "3 - 10" ="#23CDFB","4 - 10" = "#35B4FA","5 - 10" ="#479BF8","6 - 10"= "#5883F6","7 - 10" ="#6A6AF5","8 - 10" ="#7C51F3","9 - 10" ="#8E38F1","10 - 10" ="#A020F0",
  "1 - 9" = "#1BFDFB", "2 - 9" = "#2AE4F7", "3 - 9" = "#3ACCF4", "4 - 9" = "#4AB4F0","5 - 9" = "#5A9CED","6 - 9" = "#6A83E9","7 - 9" = "#7A6BE6","8 - 9" = "#8A53E2","9 - 9" = "#9A3BDF","10 - 9" = "#AA23DC",
  "1 - 8" = "#36FCF7", "2 - 8" = "#44E4F1", "3 - 8" = "#52CCEC", "4 - 8" = "#60B5E7","5 - 8" = "#6E9DE2","6 - 8" = "#7C85DC","7 - 8" = "#8A6ED7","8 - 8" = "#9856D2","9 - 8" = "#A63ECD","10 - 8" = "#B527C8",
  "1 - 7" = "#51FBF3", "2 - 7" = "#5DE3EC", "3 - 7" = "#69CCE5", "4 - 7" = "#75B5DE","5 - 7" = "#819ED7","6 - 7" = "#8E86D0","7 - 7" = "#9A6FC9","8 - 7" = "#A658C2","9 - 7" = "#B241BB","10 - 7" = "#BF2AB5",
  "1 - 6" = "#6CFAEF", "2 - 6" = "#76E3E6", "3 - 6" = "#80CCDD", "4 - 6" = "#8BB6D5","5 - 6" = "#959FCC","6 - 6" = "#A088C3","7 - 6" = "#AA72BB","8 - 6" = "#B55BB2","9 - 6" = "#BF44A9","10 - 6" = "#CA2EA1",
  "1 - 5" = "#88F9EB", "2 - 5" = "#90E2E0", "3 - 5" = "#98CCD6", "4 - 5" = "#A1B6CB","5 - 5" = "#A9A0C1","6 - 5" = "#B289B7","7 - 5" = "#BA73AD","8 - 5" = "#C35DA2","9 - 5" = "#CB4798","10 - 5" = "#D4318E",
  "1 - 4" = "#A3F8E7", "2 - 4" = "#A9E2DA", "3 - 4" = "#B0CCCE", "4 - 4" = "#B7B7C2","5 - 4" = "#BDA1B6","6 - 4" = "#C48BAA","7 - 4" = "#CB769E","8 - 4" = "#D16092","9 - 4" = "#D84A86","10 - 4" = "#DF357A",
  "1 - 3" = "#BEF7E3", "2 - 3" = "#C2E1D5", "3 - 3" = "#C7CCC7", "4 - 3" = "#CCB7B9","5 - 3" = "#D1A2AB","6 - 3" = "#D58C9E","7 - 3" = "#DA7790","8 - 3" = "#DF6282","9 - 3" = "#E44D74","10 - 3" = "#E93867",
  "1 - 2" = "#D9F6DF", "2 - 2" = "#DBE1CF", "3 - 2" = "#DFCCBF", "4 - 2" = "#E2B8B0","5 - 2" = "#E5A3A0","6 - 2" = "#E88E91","7 - 2" = "#EB7981","8 - 2" = "#EE6572","9 - 2" = "#F15062","10 - 2" = "#F43C53",
  "1 - 1" = "#F5F5DC", "2 - 1" = "#F6E0CA", "3 - 1" = "#F7CCB9", "4 - 1" = "#F8B8A8","5 - 1" = "#F9A496","6 - 1" = "#FA9085","7 - 1" = "#FB7C74","8 - 1" = "#FC6862","9 - 1" = "#FD5451","10 - 1" = "#FF4040",
  
) %>%
  gather("group", "fill")


ojox <- ojo %>%
  mutate(
    pp_quantiles = cut(pp, breaks = quantiles_pp, include.lowest = TRUE, labels = FALSE),
    abs_quantiles = cut(abs, breaks = quantiles_abs, include.lowest = TRUE, labels = FALSE)
  ) %>%

  mutate(
    group = paste(as.numeric(pp_quantiles), "-", as.numeric(abs_quantiles))
    #group = paste(as.numeric(entr_quantiles), "-", as.numeric(pop_quantiles))
  ) %>%
  
  left_join(bivariate_color_scale, by = "group") 
colnames(ojox)[6]<-c("g")
colnames(ojox)[7]<-c("color")
data <- join(grid50_data, ojox, type = "full")  


p1<-ggplot(data, aes(x = long, y = lat, group = group,fill = color)) +
  geom_polygon(colour="transparent") +
  scale_fill_identity() +  
  theme_minimal()+
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
    legend.position="none",
  )




####bird
ojo1<-read.csv("F:/Function/bird_all_FRic.csv")
ojo1<-ojo1[,-1]
ojo2<-read.csv("F:/Function/bird_change_FRic.csv")
ojo2<-ojo2[,-1]
for (i in 1:length(ojo1[,1])) {
  pos<-which(ojo2[,1]==ojo1[i,1])
  ojo1[i,2]<-ojo2[pos,2]/ojo1[i,2]
}
ojo<-ojo1
ojo<-na.omit(ojo)
colnames(ojo)<-c("id","pp")
ojo2<-read.csv("F:/Function/bird_change_FRic.csv")
ojo2<-ojo2[,-1]
colnames(ojo2)<-c("id","abs")
ojo<-join(ojo,ojo2)

pos<-which(ojo[,3]==0)
ojo<-ojo[-pos,]
o2<-ojo


quantiles_pp <- ojo %>%
  pull(pp) %>%
  quantile(probs = seq(0, 1, length.out = 11))

quantiles_abs <- ojo %>%
  pull(abs) %>%
  quantile(probs = seq(0, 1, length.out = 11))

ojox <- ojo %>%
  mutate(
    pp_quantiles = cut(pp, breaks = quantiles_pp, include.lowest = TRUE, labels = FALSE),
    abs_quantiles = cut(abs, breaks = quantiles_abs, include.lowest = TRUE, labels = FALSE)
  ) %>%
 
  mutate(
    group = paste(as.numeric(pp_quantiles), "-", as.numeric(abs_quantiles))
    #group = paste(as.numeric(entr_quantiles), "-", as.numeric(pop_quantiles))
  ) %>%

  left_join(bivariate_color_scale, by = "group")
colnames(ojox)[6]<-c("g")
colnames(ojox)[7]<-c("color")
data <- join(grid50_data, ojox, type = "full")  


p2<-ggplot(data, aes(x = long, y = lat, group = group,fill = color)) +
  geom_polygon(colour="transparent") +
  scale_fill_identity() +   
  theme_minimal()+
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
    legend.position="none",
  )

###AM
ojo1<-read.csv("F:/Function/AM_all_FRic.csv")
ojo1<-ojo1[,-1]
ojo2<-read.csv("F:/Function/AM_change_FRic.csv")
ojo2<-ojo2[,-1]
for (i in 1:length(ojo1[,1])) {
  pos<-which(ojo2[,1]==ojo1[i,1])
  ojo1[i,2]<-ojo2[pos,2]/ojo1[i,2]
}
ojo<-ojo1
ojo<-na.omit(ojo)
colnames(ojo)<-c("id","pp")
ojo2<-read.csv("F:/Function/AM_change_FRic.csv")
ojo2<-ojo2[,-1]
colnames(ojo2)<-c("id","abs")
ojo<-join(ojo,ojo2)

pos<-which(ojo[,3]==0)
ojo<-ojo[-pos,]
o3<-ojo


quantiles_pp <- ojo %>%
  pull(pp) %>%
  quantile(probs = seq(0, 1, length.out = 11))

quantiles_abs <- ojo %>%
  pull(abs) %>%
  quantile(probs = seq(0, 1, length.out = 11))

ojox <- ojo %>%
  mutate(
    pp_quantiles = cut(pp, breaks = quantiles_pp, include.lowest = TRUE, labels = FALSE),
    abs_quantiles = cut(abs, breaks = quantiles_abs, include.lowest = TRUE, labels = FALSE)
  ) %>%
  
  mutate(
    group = paste(as.numeric(pp_quantiles), "-", as.numeric(abs_quantiles))
    #group = paste(as.numeric(entr_quantiles), "-", as.numeric(pop_quantiles))
  ) %>%

  left_join(bivariate_color_scale, by = "group")
colnames(ojox)[6]<-c("g")
colnames(ojox)[7]<-c("color")
data <- join(grid50_data, ojox, type = "full")  


p3<-ggplot(data, aes(x = long, y = lat, group = group,fill = color)) +
  geom_polygon(colour="transparent") +
  scale_fill_identity() + 
  theme_minimal()+
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
    legend.position="none",
  )

###RE
ojo1<-read.csv("F:/Function/RE_all_FRic.csv")
ojo1<-ojo1[,-1]
ojo2<-read.csv("F:/Function/RE_change_FRic.csv")
ojo2<-ojo2[,-1]
for (i in 1:length(ojo1[,1])) {
  pos<-which(ojo2[,1]==ojo1[i,1])
  ojo1[i,2]<-ojo2[pos,2]/ojo1[i,2]
}
ojo<-ojo1
ojo<-na.omit(ojo)
colnames(ojo)<-c("id","pp")
ojo2<-read.csv("F:/Function/RE_change_FRic.csv")
ojo2<-ojo2[,-1]
colnames(ojo2)<-c("id","abs")
ojo<-join(ojo,ojo2)

pos<-which(ojo[,3]==0)
ojo<-ojo[-pos,]
o4<-ojo

quantiles_pp <- ojo %>%
  pull(pp) %>%
  quantile(probs = seq(0, 1, length.out = 11))

quantiles_abs <- ojo %>%
  pull(abs) %>%
  quantile(probs = seq(0, 1, length.out = 11))

ojox <- ojo %>%
  mutate(
    pp_quantiles = cut(pp, breaks = quantiles_pp, include.lowest = TRUE, labels = FALSE),
    abs_quantiles = cut(abs, breaks = quantiles_abs, include.lowest = TRUE, labels = FALSE)
  ) %>%

  mutate(
    group = paste(as.numeric(pp_quantiles), "-", as.numeric(abs_quantiles))
    #group = paste(as.numeric(entr_quantiles), "-", as.numeric(pop_quantiles))
  ) %>%
 
  left_join(bivariate_color_scale, by = "group")
colnames(ojox)[6]<-c("g")
colnames(ojox)[7]<-c("color")
data <- join(grid50_data, ojox, type = "full")  


p4<-ggplot(data, aes(x = long, y = lat, group = group,fill = color)) +
  geom_polygon(colour="transparent") +
  scale_fill_identity() +  
  theme_minimal()+
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
    legend.position="none",
  )


bivariate_color_scale <- bivariate_color_scale %>%
  separate(group, into = c("pp", "abs"), sep = " - ", convert = TRUE) %>%
  mutate(
    pp = as.integer(pp),
    abs = as.integer(abs)
  )



bivariate_color_scale[,1]<-bivariate_color_scale[,1]/10
bivariate_color_scale[,2]<-bivariate_color_scale[,2]/10
x_ticks <- seq(0.1, 1, by = 0.1)
x_tick_labels <- as.character(x_ticks)
y_ticks <- seq(0.1, 1, by = 0.1)
y_tick_labels <- as.character(y_ticks)


legend <- ggplot(data = bivariate_color_scale) +
  geom_tile(mapping = aes(x = pp, y = abs, fill = fill)) +
  scale_fill_identity() +
  labs(x = "Percentage of functional diversity erosion",
       y = "Value of functional diversity erosion") +
  theme_void() + 
  theme(
    axis.title.x = element_text(size = 8, angle = 0), 
    axis.title.y = element_text(size =8, angle = 90),  
    plot.margin = margin(t = 10, r = 10, b = 30, l = 10) 
  ) +
  annotate("segment", x = 0.1, xend = 1, y = 0.04, yend = 0.04, color = "black", size = 0.5)+#添加一条刻度线
  annotate("segment", x = x_ticks, xend = x_ticks, y = 0.04, yend = 0.03, color = "black") +
  
  annotate("text", x = x_ticks, y = 0.01, label = x_tick_labels, size =2.5) +
  annotate("segment", x =0.04, xend = 0.04, y = 0.1, yend = 1, color = "black", size = 0.5)+#添加一条刻度线
  annotate("segment", x = 0.04, xend = 0.03, y = y_ticks, yend = y_ticks, color = "black") +
 
  annotate("text", x = -0.01, y = y_ticks, label = y_tick_labels, size = 2.5) +
  coord_fixed() 

print(legend)

library(ggpubr)
p<-ggarrange(p1, p2, p3,p4, ncol = 2, nrow = 2,
             labels = c("a","b","c","d"), 
             font.label = list(size = 12, face = "bold"), legend="bottom",common.legend = T)


library(grid)
p1
vie<-viewport(width=0.4,height=0.4,x=0.2,y=0.2)
print(legend,vp=vie)

l<-plot_grid(legend, p, 
             ncol = 2, 
             rel_widths = c(0.2, 1))  
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
  draw_plot(l) +
  draw_grob(img_grob1, x = 0.18, y = 0.6, width = 0.1, height = 0.1)+
  draw_grob(img_grob2, x = 0.6, y = 0.6, width = 0.08, height = 0.08)+
  draw_grob(img_grob3, x = 0.18, y = 0.1,width = 0.08, height = 0.08)+
  draw_grob(img_grob4, x = 0.6, y = 0.1,width = 0.08, height = 0.08)





###Fig 3
####TM
data1<-data[,c("MAP","MAT","TSN","PSN","HFP","HFP_change","TM_propotion_FRicloss","Bird_propotion_FRicloss","AM_propotion_FRicloss","RE_propotion_FRicloss")]
data_long <- data1 %>%
  pivot_longer(
    cols = starts_with("TM_propotion_FRicloss"):starts_with("RE_propotion_FRicloss"),  
    names_to = "group",              
    values_to = "funde"       
  )%>%
  mutate(group = factor(group, levels = c("TM_propotion_FRicloss", "Bird_propotion_FRicloss", "AM_propotion_FRicloss", "RE_propotion_FRicloss")))

data_summary <- data_long %>%
  mutate(quan = ntile(MAT, 10)) %>%
  group_by(group, quan) %>%
  summarize(fundemean = mean(funde, na.rm = TRUE)) %>%
  na.omit()




p1<-ggplot(data_summary, aes(x = quan, y = fundemean, color = group, group = group)) +
  geom_line(size = 1) +  
  geom_point(size = 2) +  
  labs(x = "MAT", y = "Erosion of Funtion Diversity", color = "group",
       title = "") +
  scale_x_continuous(breaks = 1:10,labels = NULL) +
  scale_y_continuous(labels = number_format(accuracy = 0.01)) +  
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),  
    axis.text = element_text(color = "black")  
  )+
  scale_color_manual(
    values = c("TM_propotion_FRicloss" = "red", "Bird_propotion_FRicloss" = "#FEE090", 
               "AM_propotion_FRicloss" = "#ABD9E9", "RE_propotion_FRicloss" = "#4575B4"),
    labels = c("Mammal", "Bird", "Amphibian", "Reptile")  
  ) +
  guides(color = guide_legend(title = NULL)) 

data_summary <- data_long %>%
  mutate(quan = ntile(MAP, 10)) %>%
  group_by(group, quan) %>%
  summarize(fundemean = mean(funde, na.rm = TRUE)) %>%
  na.omit()




p2<-ggplot(data_summary, aes(x = quan, y = fundemean, color = group, group = group)) +
  geom_line(size = 1) +  
  geom_point(size = 2) +  
  labs(x = "MAP", y = "Erosion of Funtion Diversity", color = "group",
       title = "") +
  scale_x_continuous(breaks = 1:10,labels = NULL) +  
  scale_y_continuous(labels = number_format(accuracy = 0.01)) +  
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),   
    axis.line = element_line(color = "black"), 
    axis.ticks = element_line(color = "black"), 
    axis.text = element_text(color = "black")  
  )+
  scale_color_manual(
    values = c("TM_propotion_FRicloss" = "red", "Bird_propotion_FRicloss" = "#FEE090", 
               "AM_propotion_FRicloss" = "#ABD9E9", "RE_propotion_FRicloss" = "#4575B4"),
    labels = c("Mammal", "Bird", "Amphibian", "Reptile")  
  ) +
  guides(color = guide_legend(title = NULL)) 

data_summary <- data_long %>%
  mutate(quan = ntile(TSN, 10)) %>%
  group_by(group, quan) %>%
  summarize(fundemean = mean(funde, na.rm = TRUE)) %>%
  na.omit()




p3<-ggplot(data_summary, aes(x = quan, y = fundemean, color = group, group = group)) +
  geom_line(size = 1) +  
  geom_point(size = 2) +  
  labs(x = "TSN", y = "Erosion of Funtion Diversity", color = "group",
       title = "") +
  scale_x_continuous(breaks = 1:10,labels = NULL) +  
  scale_y_continuous(labels = number_format(accuracy = 0.01)) + 
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),   
    axis.line = element_line(color = "black"),  
    axis.ticks = element_line(color = "black"),  
    axis.text = element_text(color = "black")   
  )+
  scale_color_manual(
    values = c("TM_propotion_FRicloss" = "red", "Bird_propotion_FRicloss" = "#FEE090", 
               "AM_propotion_FRicloss" = "#ABD9E9", "RE_propotion_FRicloss" = "#4575B4"),
    labels = c("Mammal", "Bird", "Amphibian", "Reptile")  
  ) +
  guides(color = guide_legend(title = NULL)) 

data_summary <- data_long %>%
  mutate(quan = ntile(PSN, 10)) %>%
  group_by(group, quan) %>%
  summarize(fundemean = mean(funde, na.rm = TRUE)) %>%
  na.omit()




p4<-ggplot(data_summary, aes(x = quan, y = fundemean, color = group, group = group)) +
  geom_line(size = 1) +  
  geom_point(size = 2) +  
  labs(x = "PSN", y = "Erosion of Funtion Diversity", color = "group",
       title = "") +
  scale_x_continuous(breaks = 1:10,labels = NULL) + 
  scale_y_continuous(labels = number_format(accuracy = 0.01)) + 
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),   
    panel.grid.minor = element_blank(),   
    axis.line = element_line(color = "black"),  
    axis.ticks = element_line(color = "black"), 
    axis.text = element_text(color = "black")   
  )+
  scale_color_manual(
    values = c("TM_propotion_FRicloss" = "red", "Bird_propotion_FRicloss" = "#FEE090", 
               "AM_propotion_FRicloss" = "#ABD9E9", "RE_propotion_FRicloss" = "#4575B4"),
    labels = c("Mammal", "Bird", "Amphibian", "Reptile")  
  ) +
  guides(color = guide_legend(title = NULL)) 

data_summary <- data_long %>%
  mutate(quan = ntile(HFP, 10)) %>%
  group_by(group, quan) %>%
  summarize(fundemean = mean(funde, na.rm = TRUE)) %>%
  na.omit()




p5<-ggplot(data_summary, aes(x = quan, y = fundemean, color = group, group = group)) +
  geom_line(size = 1) +  
  geom_point(size = 2) +  
  labs(x = "HFP", y = "Erosion of Funtion Diversity", color = "group",
       title = "") +
  scale_x_continuous(breaks = 1:10,labels = NULL) +  
  scale_y_continuous(labels = number_format(accuracy = 0.01)) + 
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  
    panel.grid.minor = element_blank(),  
    axis.line = element_line(color = "black"), 
    axis.ticks = element_line(color = "black"), 
    axis.text = element_text(color = "black")  
  )+
  scale_color_manual(
    values = c("TM_propotion_FRicloss" = "red", "Bird_propotion_FRicloss" = "#FEE090", 
               "AM_propotion_FRicloss" = "#ABD9E9", "RE_propotion_FRicloss" = "#4575B4"),
    labels = c("Mammal", "Bird", "Amphibian", "Reptile")  
  ) +
  guides(color = guide_legend(title = NULL)) 

data_summary <- data_long %>%
  mutate(quan = ntile(HFP_change, 10)) %>%
  group_by(group, quan) %>%
  summarize(fundemean = mean(funde, na.rm = TRUE)) %>%
  na.omit()




p6<-ggplot(data_summary, aes(x = quan, y = fundemean, color = group, group = group)) +
  geom_line(size = 1) +  
  geom_point(size = 2) + 
  labs(x = "HFP_change", y = "Erosion of Funtion Diversity", color = "group",
       title = "") +
  scale_x_continuous(breaks = 1:10,labels = NULL) + 
  scale_y_continuous(labels = number_format(accuracy = 0.01)) +  
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),   
    panel.grid.minor = element_blank(),   
    axis.line = element_line(color = "black"),  
    axis.ticks = element_line(color = "black"), 
    axis.text = element_text(color = "black")   
  )+
  scale_color_manual(
    values = c("TM_propotion_FRicloss" = "red", "Bird_propotion_FRicloss" = "#FEE090", 
               "AM_propotion_FRicloss" = "#ABD9E9", "RE_propotion_FRicloss" = "#4575B4"),
    labels = c("Mammal", "Bird", "Amphibian", "Reptile") 
  ) +
  guides(color = guide_legend(title = NULL)) 

library(ggpubr)

p<-ggarrange(p1, p2, p3,p4,p5,p6, ncol = 3, nrow = 2,
             labels = c("a","b","c","d","e","f"), 
             font.label = list(size = 12, face = "bold"), legend="left",common.legend = T)



###Fig 4
data1<-read.csv("C:/Users/13663/Desktop/hfp.csv")
data1<-data1[,c(2,8)]
data1[,1]<-paste("g",data1[,1],sep="")

data2<-read.csv("C:/Users/13663/Desktop/climatechange_chelsa585")
data2<-data2[,-1]
data2[,1]<-paste("g",data2[,1],sep="")

data3<-read.csv("C:/Users/13663/Desktop/climatechange_chelsa126")
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
wg1<-wg[1:1824]###2.5%
hfphot<-ojo[wg1,1]

ojo<-data2
max475<-sort(ojo[,2],decreasing=T)[1:length(ojo[,2])]
max475<-unique(max475)
wg<-as.numeric()
for (i in 1:length(ojo[,2])) {
  pos<-which(ojo[,2]==max475[i])
  wg<-c(wg,pos)
}
wg1<-wg[1:1824]###2.5%
cchot585<-ojo[wg1,1]

ojo<-data3
max475<-sort(ojo[,2],decreasing=T)[1:length(ojo[,2])]
max475<-unique(max475)
wg<-as.numeric()
for (i in 1:length(ojo[,2])) {
  pos<-which(ojo[,2]==max475[i])
  wg<-c(wg,pos)
}
wg1<-wg[1:1824]###2.5%
cchot126<-ojo[wg1,1]

ojo1<-read.csv("F:/Function/TM_change_FRic.csv")
ojo1<-ojo1[,-1]

ojo<-ojo1
max475<-sort(ojo[,2],decreasing=T)[1:length(ojo[,2])]
max475<-unique(max475)
wg<-as.numeric()
for (i in 1:length(ojo[,2])) {
  pos<-which(ojo[,2]==max475[i])
  wg<-c(wg,pos)
}
wg1<-wg[1:1824]###2.5%
changehot<-ojo[wg1,1]


ojo1<-read.csv("F:/Function/TM_all_FRic.csv")
ojo1<-ojo1[,-1]
ojo2<-read.csv("F:/Function/TM_change_FRic.csv")
ojo2<-ojo2[,-1]
for (i in 1:length(ojo1[,1])) {
  pos<-which(ojo2[,1]==ojo1[i,1])
  ojo1[i,2]<-ojo2[pos,2]/ojo1[i,2]
}
ojo<-ojo1
ojo<-na.omit(ojo)
max475<-sort(ojo[,2],decreasing=T)[1:length(ojo[,2])]
max475<-unique(max475)
wg<-as.numeric()
for (i in 1:length(ojo[,2])) {
  pos<-which(ojo[,2]==max475[i])
  wg<-c(wg,pos)
}
wg1<-wg[1:1824]###2.5%
erosionhot<-ojo[wg1,1]

inte_all<-intersect(hfphot,erosionhot)
inte_all<-intersect(inte_all,cchot585)
inte1<-intersect(hfphot,erosionhot)
inte1<-setdiff(inte1,inte_all)
inte2<-intersect(cchot585,erosionhot)
inte2<-setdiff(inte2,inte_all)
inte3<-intersect(hfphot,cchot585)
inte3<-setdiff(inte3,inte_all)
erosionhot_only<-setdiff(erosionhot,inte_all)
erosionhot_only<-setdiff(erosionhot_only,inte1)
erosionhot_only<-setdiff(erosionhot_only,inte2)
erosionhot_only<-setdiff(erosionhot_only,inte3)
hfphot_only<-setdiff(hfphot,inte_all)
hfphot_only<-setdiff(hfphot_only,inte1)
hfphot_only<-setdiff(hfphot_only,inte2)
hfphot_only<-setdiff(hfphot_only,inte3)
cchot585_only<-setdiff(cchot585,inte_all)
cchot585_only<-setdiff(cchot585_only,inte1)
cchot585_only<-setdiff(cchot585_only,inte2)
cchot585_only<-setdiff(cchot585_only,inte3)

ojo<-read.csv("F:/Function/TM_all_FRic.csv")
ojo<-ojo[,-1]

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
for (i in 1:length(inte3)) {
  pos<-which(ojo[,1]==inte3[i])
  pos4<-c(pos4,pos)
}
pos5<-as.numeric()
for (i in 1:length(erosionhot_only)) {
  pos<-which(ojo[,1]==erosionhot_only[i])
  pos5<-c(pos5,pos)
}
pos6<-as.numeric()
for (i in 1:length(hfphot_only)) {
  pos<-which(ojo[,1]==hfphot_only[i])
  pos6<-c(pos6,pos)
}
pos7<-as.numeric()
for (i in 1:length(cchot585_only)) {
  pos<-which(ojo[,1]==cchot585_only[i])
  pos7<-c(pos7,pos)
}

ojo[,2]<-NA
ojo[pos1,2]<-1
ojo[pos2,2]<-2
ojo[pos3,2]<-3
ojo[pos4,2]<-4
ojo[pos5,2]<-5
ojo[pos6,2]<-6
ojo[pos7,2]<-7

colnames(ojo)<-c("id","number")
data <- join(grid50_data, ojo, type = "full")  

data$number<-as.factor(data$number)
p1<-ggplot(data, aes(x = long, y = lat, group = group,fill = number)) +
  geom_polygon(colour="transparent") +
  scale_fill_manual(values =c("#F46D43","#FDAE61","#FEE090","#FFFFBF","#ABD9E9","#74ADD1","#4575B4"),na.value = "white",breaks = c("1","2","3","4","5","6","7"),labels=c("All overlap", "Function erosion&Human footprint", "Function erosion&Climate change","Human footprint&Climate change","Function erosion","Human footprint","Climate change")) +  
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

ojo1<-read.csv("F:/Function/Bird_change_FRic.csv")
ojo1<-ojo1[,-1]

ojo<-ojo1
max475<-sort(ojo[,2],decreasing=T)[1:length(ojo[,2])]
max475<-unique(max475)
wg<-as.numeric()
for (i in 1:length(ojo[,2])) {
  pos<-which(ojo[,2]==max475[i])
  wg<-c(wg,pos)
}
wg1<-wg[1:1824]###2.5%
changehot<-ojo[wg1,1]


ojo1<-read.csv("F:/Function/Bird_all_FRic.csv")
ojo1<-ojo1[,-1]
ojo2<-read.csv("F:/Function/Bird_change_FRic.csv")
ojo2<-ojo2[,-1]
for (i in 1:length(ojo1[,1])) {
  pos<-which(ojo2[,1]==ojo1[i,1])
  ojo1[i,2]<-ojo2[pos,2]/ojo1[i,2]
}
ojo<-ojo1
ojo<-na.omit(ojo)
max475<-sort(ojo[,2],decreasing=T)[1:length(ojo[,2])]
max475<-unique(max475)
wg<-as.numeric()
for (i in 1:length(ojo[,2])) {
  pos<-which(ojo[,2]==max475[i])
  wg<-c(wg,pos)
}
wg1<-wg[1:1824]###2.5%
erosionhot<-ojo[wg1,1]

inte_all<-intersect(hfphot,erosionhot)
inte_all<-intersect(inte_all,cchot585)
inte1<-intersect(hfphot,erosionhot)
inte1<-setdiff(inte1,inte_all)
inte2<-intersect(cchot585,erosionhot)
inte2<-setdiff(inte2,inte_all)
inte3<-intersect(hfphot,cchot585)
inte3<-setdiff(inte3,inte_all)
erosionhot_only<-setdiff(erosionhot,inte_all)
erosionhot_only<-setdiff(erosionhot_only,inte1)
erosionhot_only<-setdiff(erosionhot_only,inte2)
erosionhot_only<-setdiff(erosionhot_only,inte3)
hfphot_only<-setdiff(hfphot,inte_all)
hfphot_only<-setdiff(hfphot_only,inte1)
hfphot_only<-setdiff(hfphot_only,inte2)
hfphot_only<-setdiff(hfphot_only,inte3)
cchot585_only<-setdiff(cchot585,inte_all)
cchot585_only<-setdiff(cchot585_only,inte1)
cchot585_only<-setdiff(cchot585_only,inte2)
cchot585_only<-setdiff(cchot585_only,inte3)

ojo<-read.csv("F:/Function/Bird_all_FRic.csv")
ojo<-ojo[,-1]

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
for (i in 1:length(inte3)) {
  pos<-which(ojo[,1]==inte3[i])
  pos4<-c(pos4,pos)
}
pos5<-as.numeric()
for (i in 1:length(erosionhot_only)) {
  pos<-which(ojo[,1]==erosionhot_only[i])
  pos5<-c(pos5,pos)
}
pos6<-as.numeric()
for (i in 1:length(hfphot_only)) {
  pos<-which(ojo[,1]==hfphot_only[i])
  pos6<-c(pos6,pos)
}
pos7<-as.numeric()
for (i in 1:length(cchot585_only)) {
  pos<-which(ojo[,1]==cchot585_only[i])
  pos7<-c(pos7,pos)
}

ojo[,2]<-NA
ojo[pos1,2]<-1
ojo[pos2,2]<-2
ojo[pos3,2]<-3
ojo[pos4,2]<-4
ojo[pos5,2]<-5
ojo[pos6,2]<-6
ojo[pos7,2]<-7

colnames(ojo)<-c("id","number")
data <- join(grid50_data, ojo, type = "full")  

data$number<-as.factor(data$number)
p2<-ggplot(data, aes(x = long, y = lat, group = group,fill = number)) +
  geom_polygon(colour="transparent") +
  scale_fill_manual(values =c("#F46D43","#FDAE61","#FEE090","#FFFFBF","#ABD9E9","#74ADD1","#4575B4"),na.value = "white",breaks = c("1","2","3","4","5","6","7"),labels=c("All overlap", "Function erosion&Human footprint", "Function erosion&Climate change","Human footprint&Climate change","Function erosion","Human footprint","Climate change")) +  
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

ojo1<-read.csv("F:/Function/AM_change_FRic.csv")
ojo1<-ojo1[,-1]
ojo<-ojo1
pos<-which(ojo[,2]==0)
ojo<-ojo[-pos,]
changehot<-ojo[,1]


ojo1<-read.csv("F:/Function/AM_all_FRic.csv")
ojo1<-ojo1[,-1]
ojo2<-read.csv("F:/Function/AM_change_FRic.csv")
ojo2<-ojo2[,-1]
for (i in 1:length(ojo1[,1])) {
  pos<-which(ojo2[,1]==ojo1[i,1])
  ojo1[i,2]<-ojo2[pos,2]/ojo1[i,2]
}
ojo<-ojo1
ojo<-na.omit(ojo)
pos<-which(ojo[,2]==0)
ojo<-ojo[-pos,]
erosionhot<-ojo[,1]

inte_all<-intersect(hfphot,erosionhot)
inte_all<-intersect(inte_all,cchot585)
inte1<-intersect(hfphot,erosionhot)
inte1<-setdiff(inte1,inte_all)
inte2<-intersect(cchot585,erosionhot)
inte2<-setdiff(inte2,inte_all)
inte3<-intersect(hfphot,cchot585)
inte3<-setdiff(inte3,inte_all)
erosionhot_only<-setdiff(erosionhot,inte_all)
erosionhot_only<-setdiff(erosionhot_only,inte1)
erosionhot_only<-setdiff(erosionhot_only,inte2)
erosionhot_only<-setdiff(erosionhot_only,inte3)
hfphot_only<-setdiff(hfphot,inte_all)
hfphot_only<-setdiff(hfphot_only,inte1)
hfphot_only<-setdiff(hfphot_only,inte2)
hfphot_only<-setdiff(hfphot_only,inte3)
cchot585_only<-setdiff(cchot585,inte_all)
cchot585_only<-setdiff(cchot585_only,inte1)
cchot585_only<-setdiff(cchot585_only,inte2)
cchot585_only<-setdiff(cchot585_only,inte3)

ojo<-read.csv("F:/Function/AM_all_FRic.csv")
ojo<-ojo[,-1]

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
for (i in 1:length(inte3)) {
  pos<-which(ojo[,1]==inte3[i])
  pos4<-c(pos4,pos)
}
pos5<-as.numeric()
for (i in 1:length(erosionhot_only)) {
  pos<-which(ojo[,1]==erosionhot_only[i])
  pos5<-c(pos5,pos)
}
pos6<-as.numeric()
for (i in 1:length(hfphot_only)) {
  pos<-which(ojo[,1]==hfphot_only[i])
  pos6<-c(pos6,pos)
}
pos7<-as.numeric()
for (i in 1:length(cchot585_only)) {
  pos<-which(ojo[,1]==cchot585_only[i])
  pos7<-c(pos7,pos)
}

ojo[,2]<-NA
ojo[pos1,2]<-1
ojo[pos2,2]<-2
ojo[pos3,2]<-3
ojo[pos4,2]<-4
ojo[pos5,2]<-5
ojo[pos6,2]<-6
ojo[pos7,2]<-7

colnames(ojo)<-c("id","number")
data <- join(grid50_data, ojo, type = "full")  

data$number<-as.factor(data$number)
p3<-ggplot(data, aes(x = long, y = lat, group = group,fill = number)) +
  geom_polygon(colour="transparent") +
  scale_fill_manual(values =c("#F46D43","#FDAE61","#FEE090","#FFFFBF","#ABD9E9","#74ADD1","#4575B4"),na.value = "white",breaks = c("1","2","3","4","5","6","7"),labels=c("All overlap", "Function erosion&Human footprint", "Function erosion&Climate change","Human footprint&Climate change","Function erosion","Human footprint","Climate change")) +  
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

ojo1<-read.csv("F:/Function/RE_change_FRic.csv")
ojo1<-ojo1[,-1]

ojo<-ojo1
max475<-sort(ojo[,2],decreasing=T)[1:length(ojo[,2])]
max475<-unique(max475)
wg<-as.numeric()
for (i in 1:length(ojo[,2])) {
  pos<-which(ojo[,2]==max475[i])
  wg<-c(wg,pos)
}
wg1<-wg[1:1824]###2.5%
changehot<-ojo[wg1,1]


ojo1<-read.csv("F:/Function/RE_all_FRic.csv")
ojo1<-ojo1[,-1]
ojo2<-read.csv("F:/Function/RE_change_FRic.csv")
ojo2<-ojo2[,-1]
for (i in 1:length(ojo1[,1])) {
  pos<-which(ojo2[,1]==ojo1[i,1])
  ojo1[i,2]<-ojo2[pos,2]/ojo1[i,2]
}
ojo<-ojo1
ojo<-na.omit(ojo)
max475<-sort(ojo[,2],decreasing=T)[1:length(ojo[,2])]
max475<-unique(max475)
wg<-as.numeric()
for (i in 1:length(ojo[,2])) {
  pos<-which(ojo[,2]==max475[i])
  wg<-c(wg,pos)
}
wg1<-wg[1:1824]###2.5%
erosionhot<-ojo[wg1,1]

inte_all<-intersect(hfphot,erosionhot)
inte_all<-intersect(inte_all,cchot585)
inte1<-intersect(hfphot,erosionhot)
inte1<-setdiff(inte1,inte_all)
inte2<-intersect(cchot585,erosionhot)
inte2<-setdiff(inte2,inte_all)
inte3<-intersect(hfphot,cchot585)
inte3<-setdiff(inte3,inte_all)
erosionhot_only<-setdiff(erosionhot,inte_all)
erosionhot_only<-setdiff(erosionhot_only,inte1)
erosionhot_only<-setdiff(erosionhot_only,inte2)
erosionhot_only<-setdiff(erosionhot_only,inte3)
hfphot_only<-setdiff(hfphot,inte_all)
hfphot_only<-setdiff(hfphot_only,inte1)
hfphot_only<-setdiff(hfphot_only,inte2)
hfphot_only<-setdiff(hfphot_only,inte3)
cchot585_only<-setdiff(cchot585,inte_all)
cchot585_only<-setdiff(cchot585_only,inte1)
cchot585_only<-setdiff(cchot585_only,inte2)
cchot585_only<-setdiff(cchot585_only,inte3)

ojo<-read.csv("F:/Function/RE_all_FRic.csv")
ojo<-ojo[,-1]

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
for (i in 1:length(inte3)) {
  pos<-which(ojo[,1]==inte3[i])
  pos4<-c(pos4,pos)
}
pos5<-as.numeric()
for (i in 1:length(erosionhot_only)) {
  pos<-which(ojo[,1]==erosionhot_only[i])
  pos5<-c(pos5,pos)
}
pos6<-as.numeric()
for (i in 1:length(hfphot_only)) {
  pos<-which(ojo[,1]==hfphot_only[i])
  pos6<-c(pos6,pos)
}
pos7<-as.numeric()
for (i in 1:length(cchot585_only)) {
  pos<-which(ojo[,1]==cchot585_only[i])
  pos7<-c(pos7,pos)
}

ojo[,2]<-NA
ojo[pos1,2]<-1
ojo[pos2,2]<-2
ojo[pos3,2]<-3
ojo[pos4,2]<-4
ojo[pos5,2]<-5
ojo[pos6,2]<-6
ojo[pos7,2]<-7

colnames(ojo)<-c("id","number")
data <- join(grid50_data, ojo, type = "full")  

data$number<-as.factor(data$number)
p4<-ggplot(data, aes(x = long, y = lat, group = group,fill = number)) +
  geom_polygon(colour="transparent") +
  scale_fill_manual(values =c("#F46D43","#FDAE61","#FEE090","#FFFFBF","#ABD9E9","#74ADD1","#4575B4"),na.value = "white",breaks = c("1","2","3","4","5","6","7"),labels=c("All overlap", "Function erosion&Human footprint", "Function erosion&Climate change","Human footprint&Climate change","Function erosion","Human footprint","Climate change")) +  
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
  draw_grob(img_grob1, x = 0.1, y = 0.6, width = 0.12, height = 0.12)+
  draw_grob(img_grob2, x = 0.57, y = 0.6, width = 0.1, height = 0.1)+
  draw_grob(img_grob3, x = 0.1, y = 0.1,width = 0.1, height = 0.1)+
  draw_grob(img_grob4, x = 0.57, y = 0.1, width = 0.1, height = 0.1)



###Fig 5
data<-read.csv("C:/Users/13663/Desktop/保护区和国界shp/PA_WDPA&CN.csv")
data<-data[,2:3]
data[,2]<-data[,2]/14400
ojo<-data
colnames(ojo)<-c("X","x")
o<-read.csv("C:/Users/13663/Desktop/陆地网格")
o<-o[,2:3]
n<-as.numeric()
for (i in 1:length(o[,2])) {
  pos3<-which(ojo[,1]==o[i,1])
  
  n<-c(n,pos3)
}
ojo<-ojo[n,]
for (i in 1:length(o[,2])) {
  pos3<-which(ojo[,1]==o[i,1])
  if(length(pos3)==0){
    o[i,2]<-0
  }else{
    o[i,2]<-ojo[pos3,2]
  }
}
pa<-o


ojo1<-read.csv("F:/Function/TM_change_FRic.csv")
ojo1<-ojo1[,-1]

ojo<-ojo1
max475<-sort(ojo[,2],decreasing=T)[1:length(ojo[,2])]
max475<-unique(max475)
wg<-as.numeric()
for (i in 1:length(ojo[,2])) {
  pos<-which(ojo[,2]==max475[i])
  wg<-c(wg,pos)
}
wg1<-wg[1:1824]###2.5%
hot<-ojo[wg1,1]

wg<-as.numeric()
for (i in 1:length(hot)) {
  pos<-which(pa[,1]==hot[i])
  wg<-c(wg,pos)
}

ojo<-pa[wg,]


max475<-sort(ojo[,2],decreasing=T)[1:length(ojo[,2])]
max475<-unique(max475)
wg<-as.numeric()
for (i in 1:length(ojo[,2])) {
  pos<-which(ojo[,2]==max475[i])
  wg<-c(wg,pos)
}
wg1<-wg[1:391]###2.5%
wg2<-wg[392:645]###5%
wg3<-wg[646:length(ojo[,2])]###10%
ojo[wg1,2]<-c("30%-100%")
ojo[wg2,2]<-c("17%-30%")
ojo[wg3,2]<-c("0-17%")

colnames(ojo)<-c("id","number")
data <- join(grid50_data, ojo, type = "full")  

data$number<-as.factor(data$number)
p1<-ggplot(data, aes(x = long, y = lat, group = group,fill = number)) +
  geom_polygon(colour="transparent") +
  scale_fill_manual(values =c("#F46D43","#FEE090","#74ADD1"),na.value = "white",breaks = c("30%-100%","17%-30%","0-17%","0"),labels=c("30%-100%","17%-30%","0-17%","Others")) +  
  labs(fill="Percentage of protection")+
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
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm"),
  )+
  theme(text=element_text(size=10, family="sans"))+
  guides(fill = guide_legend(keywidth = 1, keyheight = 1))

ojo1<-read.csv("F:/Function/Bird_change_FRic.csv")
ojo1<-ojo1[,-1]

ojo<-ojo1
max475<-sort(ojo[,2],decreasing=T)[1:length(ojo[,2])]
max475<-unique(max475)
wg<-as.numeric()
for (i in 1:length(ojo[,2])) {
  pos<-which(ojo[,2]==max475[i])
  wg<-c(wg,pos)
}
wg1<-wg[1:1824]###2.5%
hot<-ojo[wg1,1]

wg<-as.numeric()
for (i in 1:length(hot)) {
  pos<-which(pa[,1]==hot[i])
  wg<-c(wg,pos)
}

ojo<-pa[wg,]


max475<-sort(ojo[,2],decreasing=T)[1:length(ojo[,2])]
max475<-unique(max475)
wg<-as.numeric()
for (i in 1:length(ojo[,2])) {
  pos<-which(ojo[,2]==max475[i])
  wg<-c(wg,pos)
}
wg1<-wg[1:366]###2.5%
wg2<-wg[367:554]###5%
wg3<-wg[555:length(ojo[,2])]###10%
ojo[wg1,2]<-c("30%-100%")
ojo[wg2,2]<-c("17%-30%")
ojo[wg3,2]<-c("0-17%")

colnames(ojo)<-c("id","number")
data <- join(grid50_data, ojo, type = "full")  

data$number<-as.factor(data$number)
p2<-ggplot(data, aes(x = long, y = lat, group = group,fill = number)) +
  geom_polygon(colour="transparent") +
  scale_fill_manual(values =c("#F46D43","#FEE090","#74ADD1"),na.value = "white",breaks = c("30%-100%","17%-30%","0-17%","0"),labels=c("30%-100%","17%-30%","0-17%","Others")) +  
  labs(fill="Percentage of protection")+
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
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm"),
  )+
  theme(text=element_text(size=10, family="sans"))+
  guides(fill = guide_legend(keywidth = 1, keyheight = 1))

ojo1<-read.csv("F:/Function/AM_change_FRic.csv")
ojo1<-ojo1[,-1]
ojo<-ojo1
pos<-which(ojo[,2]==0)
ojo<-ojo[-pos,]
hot<-ojo[,1]

wg<-as.numeric()
for (i in 1:length(hot)) {
  pos<-which(pa[,1]==hot[i])
  wg<-c(wg,pos)
}

ojo<-pa[wg,]


max475<-sort(ojo[,2],decreasing=T)[1:length(ojo[,2])]
max475<-unique(max475)
wg<-as.numeric()
for (i in 1:length(ojo[,2])) {
  pos<-which(ojo[,2]==max475[i])
  wg<-c(wg,pos)
}
wg1<-wg[1:258]###2.5%
wg2<-wg[259:465]###5%
wg3<-wg[466:length(ojo[,2])]###10%
ojo[wg1,2]<-c("30%-100%")
ojo[wg2,2]<-c("17%-30%")
ojo[wg3,2]<-c("0-17%")

colnames(ojo)<-c("id","number")
data <- join(grid50_data, ojo, type = "full")  

data$number<-as.factor(data$number)
p3<-ggplot(data, aes(x = long, y = lat, group = group,fill = number)) +
  geom_polygon(colour="transparent") +
  scale_fill_manual(values =c("#F46D43","#FEE090","#74ADD1"),na.value = "white",breaks = c("30%-100%","17%-30%","0-17%","0"),labels=c("30%-100%","17%-30%","0-17%","Others")) +  
  labs(fill="Percentage of protection")+
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
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm"),
  )+
  theme(text=element_text(size=10, family="sans"))+
  guides(fill = guide_legend(keywidth = 1, keyheight = 1))

ojo1<-read.csv("F:/Function/RE_change_FRic.csv")
ojo1<-ojo1[,-1]

ojo<-ojo1
max475<-sort(ojo[,2],decreasing=T)[1:length(ojo[,2])]
max475<-unique(max475)
wg<-as.numeric()
for (i in 1:length(ojo[,2])) {
  pos<-which(ojo[,2]==max475[i])
  wg<-c(wg,pos)
}
wg1<-wg[1:1824]###2.5%
hot<-ojo[wg1,1]

wg<-as.numeric()
for (i in 1:length(hot)) {
  pos<-which(pa[,1]==hot[i])
  wg<-c(wg,pos)
}

ojo<-pa[wg,]


max475<-sort(ojo[,2],decreasing=T)[1:length(ojo[,2])]
max475<-unique(max475)
wg<-as.numeric()
for (i in 1:length(ojo[,2])) {
  pos<-which(ojo[,2]==max475[i])
  wg<-c(wg,pos)
}
wg1<-wg[1:226]###2.5%
wg2<-wg[227:435]###5%
wg3<-wg[436:length(ojo[,2])]###10%
ojo[wg1,2]<-c("30%-100%")
ojo[wg2,2]<-c("17%-30%")
ojo[wg3,2]<-c("0-17%")

colnames(ojo)<-c("id","number")
data <- join(grid50_data, ojo, type = "full")  

data$number<-as.factor(data$number)
p4<-ggplot(data, aes(x = long, y = lat, group = group,fill = number)) +
  geom_polygon(colour="transparent") +
  scale_fill_manual(values =c("#F46D43","#FEE090","#74ADD1"),na.value = "white",breaks = c("30%-100%","17%-30%","0-17%","0"),labels=c("30%-100%","17%-30%","0-17%","Others")) +  
  labs(fill="Percentage of protection")+
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
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm"),
  )+
  theme(text=element_text(size=10, family="sans"))+
  guides(fill = guide_legend(keywidth = 1, keyheight = 1))

library(ggpubr)

p<-ggarrange(p1, p2, p3,p4, ncol = 2, nrow = 2,
             labels = c("a","b","c","d"), 
             font.label = list(size = 12, face = "bold"), legend="left",common.legend = T)

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
  draw_grob(img_grob1, x = 0.1, y = 0.6, width = 0.12, height = 0.12)+
  draw_grob(img_grob2, x = 0.55, y = 0.6, width = 0.1, height = 0.1)+
  draw_grob(img_grob3, x = 0.1, y = 0.1,width = 0.1, height = 0.1)+
  draw_grob(img_grob4, x = 0.55, y = 0.1, width = 0.1, height = 0.1)

###保护比例 比例前10%
data<-read.csv("C:/Users/13663/Desktop/PA_WDPA&CN.csv")
data<-data[,2:3]
data[,2]<-data[,2]/14400
ojo<-data
colnames(ojo)<-c("X","x")
o<-read.csv("C:/Users/13663/Desktop/landgrid")
o<-o[,2:3]
n<-as.numeric()
for (i in 1:length(o[,2])) {
  pos3<-which(ojo[,1]==o[i,1])
  
  n<-c(n,pos3)
}
ojo<-ojo[n,]
for (i in 1:length(o[,2])) {
  pos3<-which(ojo[,1]==o[i,1])
  if(length(pos3)==0){
    o[i,2]<-0
  }else{
    o[i,2]<-ojo[pos3,2]
  }
}
pa<-o


ojo1<-read.csv("F:/Function/TM_all_FRic.csv")
ojo1<-ojo1[,-1]
ojo2<-read.csv("F:/Function/TM_change_FRic.csv")
ojo2<-ojo2[,-1]
for (i in 1:length(ojo1[,1])) {
  pos<-which(ojo2[,1]==ojo1[i,1])
  ojo1[i,2]<-ojo2[pos,2]/ojo1[i,2]
}
ojo<-ojo1
ojo<-na.omit(ojo)
max475<-sort(ojo[,2],decreasing=T)[1:length(ojo[,2])]
max475<-unique(max475)
wg<-as.numeric()
for (i in 1:length(ojo[,2])) {
  pos<-which(ojo[,2]==max475[i])
  wg<-c(wg,pos)
}
wg1<-wg[1:1824]###2.5%
hot<-ojo[wg1,1]

wg<-as.numeric()
for (i in 1:length(hot)) {
  pos<-which(pa[,1]==hot[i])
  wg<-c(wg,pos)
}

ojo<-pa[wg,]


max475<-sort(ojo[,2],decreasing=T)[1:length(ojo[,2])]
max475<-unique(max475)
wg<-as.numeric()
for (i in 1:length(ojo[,2])) {
  pos<-which(ojo[,2]==max475[i])
  wg<-c(wg,pos)
}
wg1<-wg[1:392]###2.5%
wg2<-wg[393:592]###5%
wg3<-wg[593:length(ojo[,2])]###10%
ojo[wg1,2]<-c("30%-100%")
ojo[wg2,2]<-c("17%-30%")
ojo[wg3,2]<-c("0-17%")

colnames(ojo)<-c("id","number")
data <- join(grid50_data, ojo, type = "full")  

data$number<-as.factor(data$number)
p1<-ggplot(data, aes(x = long, y = lat, group = group,fill = number)) +
  geom_polygon(colour="transparent") +
  scale_fill_manual(values =c("#F46D43","#FEE090","#74ADD1"),na.value = "white",breaks = c("30%-100%","17%-30%","0-17%","0"),labels=c("30%-100%","17%-30%","0-17%","Others")) +  
  labs(fill="Percentage of protection")+
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
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm"),
  )+
  theme(text=element_text(size=10, family="sans"))+
  guides(fill = guide_legend(keywidth = 1, keyheight = 1))

ojo1<-read.csv("F:/Function/Bird_all_FRic.csv")
ojo1<-ojo1[,-1]
ojo2<-read.csv("F:/Function/Bird_change_FRic.csv")
ojo2<-ojo2[,-1]
for (i in 1:length(ojo1[,1])) {
  pos<-which(ojo2[,1]==ojo1[i,1])
  ojo1[i,2]<-ojo2[pos,2]/ojo1[i,2]
}
ojo<-ojo1
ojo<-na.omit(ojo)
max475<-sort(ojo[,2],decreasing=T)[1:length(ojo[,2])]
max475<-unique(max475)
wg<-as.numeric()
for (i in 1:length(ojo[,2])) {
  pos<-which(ojo[,2]==max475[i])
  wg<-c(wg,pos)
}
wg1<-wg[1:1824]###2.5%
hot<-ojo[wg1,1]

wg<-as.numeric()
for (i in 1:length(hot)) {
  pos<-which(pa[,1]==hot[i])
  wg<-c(wg,pos)
}

ojo<-pa[wg,]


max475<-sort(ojo[,2],decreasing=T)[1:length(ojo[,2])]
max475<-unique(max475)
wg<-as.numeric()
for (i in 1:length(ojo[,2])) {
  pos<-which(ojo[,2]==max475[i])
  wg<-c(wg,pos)
}
wg1<-wg[1:317]###2.5%
wg2<-wg[318:460]###5%
wg3<-wg[461:length(ojo[,2])]###10%
ojo[wg1,2]<-c("30%-100%")
ojo[wg2,2]<-c("17%-30%")
ojo[wg3,2]<-c("0-17%")

colnames(ojo)<-c("id","number")
data <- join(grid50_data, ojo, type = "full")  

data$number<-as.factor(data$number)
p2<-ggplot(data, aes(x = long, y = lat, group = group,fill = number)) +
  geom_polygon(colour="transparent") +
  scale_fill_manual(values =c("#F46D43","#FEE090","#74ADD1"),na.value = "white",breaks = c("30%-100%","17%-30%","0-17%","0"),labels=c("30%-100%","17%-30%","0-17%","Others")) +  
  labs(fill="Percentage of protection")+
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
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm"),
  )+
  theme(text=element_text(size=10, family="sans"))+
  guides(fill = guide_legend(keywidth = 1, keyheight = 1))

ojo1<-read.csv("F:/Function/AM_all_FRic.csv")
ojo1<-ojo1[,-1]
ojo2<-read.csv("F:/Function/AM_change_FRic.csv")
ojo2<-ojo2[,-1]
for (i in 1:length(ojo1[,1])) {
  pos<-which(ojo2[,1]==ojo1[i,1])
  ojo1[i,2]<-ojo2[pos,2]/ojo1[i,2]
}
ojo<-ojo1
ojo<-na.omit(ojo)
pos<-which(ojo[,2]==0)
ojo<-ojo[-pos,]
hot<-ojo[,1]

wg<-as.numeric()
for (i in 1:length(hot)) {
  pos<-which(pa[,1]==hot[i])
  wg<-c(wg,pos)
}

ojo<-pa[wg,]


max475<-sort(ojo[,2],decreasing=T)[1:length(ojo[,2])]
max475<-unique(max475)
wg<-as.numeric()
for (i in 1:length(ojo[,2])) {
  pos<-which(ojo[,2]==max475[i])
  wg<-c(wg,pos)
}
wg1<-wg[1:258]###2.5%
wg2<-wg[259:465]###5%
wg3<-wg[466:length(ojo[,2])]###10%
ojo[wg1,2]<-c("30%-100%")
ojo[wg2,2]<-c("17%-30%")
ojo[wg3,2]<-c("0-17%")

colnames(ojo)<-c("id","number")
data <- join(grid50_data, ojo, type = "full")  

data$number<-as.factor(data$number)
p3<-ggplot(data, aes(x = long, y = lat, group = group,fill = number)) +
  geom_polygon(colour="transparent") +
  scale_fill_manual(values =c("#F46D43","#FEE090","#74ADD1"),na.value = "white",breaks = c("30%-100%","17%-30%","0-17%","0"),labels=c("30%-100%","17%-30%","0-17%","Others")) +  
  labs(fill="Percentage of protection")+
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
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm"),
  )+
  theme(text=element_text(size=10, family="sans"))+
  guides(fill = guide_legend(keywidth = 1, keyheight = 1))

ojo1<-read.csv("F:/Function/RE_all_FRic.csv")
ojo1<-ojo1[,-1]
ojo2<-read.csv("F:/Function/RE_change_FRic.csv")
ojo2<-ojo2[,-1]
for (i in 1:length(ojo1[,1])) {
  pos<-which(ojo2[,1]==ojo1[i,1])
  ojo1[i,2]<-ojo2[pos,2]/ojo1[i,2]
}
ojo<-ojo1
ojo<-na.omit(ojo)
max475<-sort(ojo[,2],decreasing=T)[1:length(ojo[,2])]
max475<-unique(max475)
wg<-as.numeric()
for (i in 1:length(ojo[,2])) {
  pos<-which(ojo[,2]==max475[i])
  wg<-c(wg,pos)
}
wg1<-wg[1:1824]###2.5%
hot<-ojo[wg1,1]

wg<-as.numeric()
for (i in 1:length(hot)) {
  pos<-which(pa[,1]==hot[i])
  wg<-c(wg,pos)
}

ojo<-pa[wg,]


max475<-sort(ojo[,2],decreasing=T)[1:length(ojo[,2])]
max475<-unique(max475)
wg<-as.numeric()
for (i in 1:length(ojo[,2])) {
  pos<-which(ojo[,2]==max475[i])
  wg<-c(wg,pos)
}
wg1<-wg[1:227]###2.5%
wg2<-wg[228:430]###5%
wg3<-wg[431:length(ojo[,2])]###10%
ojo[wg1,2]<-c("30%-100%")
ojo[wg2,2]<-c("17%-30%")
ojo[wg3,2]<-c("0-17%")

colnames(ojo)<-c("id","number")
data <- join(grid50_data, ojo, type = "full")  

data$number<-as.factor(data$number)
p4<-ggplot(data, aes(x = long, y = lat, group = group,fill = number)) +
  geom_polygon(colour="transparent") +
  scale_fill_manual(values =c("#F46D43","#FEE090","#74ADD1"),na.value = "white",breaks = c("30%-100%","17%-30%","0-17%","0"),labels=c("30%-100%","17%-30%","0-17%","Others")) +  
  labs(fill="Percentage of protection")+
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
    legend.margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "cm"),
  )+
  theme(text=element_text(size=10, family="sans"))+
  guides(fill = guide_legend(keywidth = 1, keyheight = 1))

library(ggpubr)

p<-ggarrange(p1, p2, p3,p4, ncol = 2, nrow = 2,
             labels = c("a","b","c","d"), 
             font.label = list(size = 12, face = "bold"), legend="left",common.legend = T)

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
  draw_grob(img_grob1, x = 0.1, y = 0.6, width = 0.12, height = 0.12)+
  draw_grob(img_grob2, x = 0.55, y = 0.6, width = 0.1, height = 0.1)+
  draw_grob(img_grob3, x = 0.1, y = 0.1,width = 0.1, height = 0.1)+
  draw_grob(img_grob4, x = 0.55, y = 0.1, width = 0.1, height = 0.1)

