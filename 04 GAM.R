####GAM
library(mgcv)
library(spdep)
library(marginaleffects)
data<-read.csv("C:/Users/13663/Desktop/alldata/data_20250703.csv")
data[,1]<-paste("g",data[,1],sep="")
data1<-read.csv("C:/Users/13663/Desktop/sa/hfp2022.csv")
data1<-data1[,c(2,8)]
data1[,1]<-paste("g",data1[,1],sep="")
data2<-read.csv("C:/Users/13663/Desktop/sa/hfp2000.csv")
data2<-data2[,c(2,8)]
data2[,1]<-paste("g",data2[,1],sep="")
data3<-data1
data3[,2]<-data1[,2]-data2[,2]
colnames(data1)[2]<-c("HFP")
colnames(data3)[2]<-c("HFP_change")
data<-data[,-c(2,37)]
data<-join(data,data1)
data<-join(data,data3)
write.csv(data,"C:/Users/13663/Desktop/alldata/data_20250728.csv")

data<-read.csv("C:/Users/13663/Desktop/alldata/data_20250728.csv")


ojo<-read.csv("F:/Function/kernel_alpha/mammal_allspecies.csv")
ojo<-ojo[,-1]
ojo<-ojo[,-2]
ojo<-na.omit(ojo)

ojo[,1]<-paste("g",ojo[,1],sep="")
ojo1<-ojo
ojo<-read.csv("F:/Function/kernel_alpha/mammal_change_disp_ddsample.csv")
ojo<-ojo[,-1]
ojo<-ojo[,-2]
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
o<-read.csv("C:/Users/13663/Desktop/langrid")
o<-o[,2:3]
a<-intersect(ojo[,1],o[,1])
n<-as.numeric()
for (i in 1:length(a)) {
  pos3<-which(ojo[,1]==a[i])
  n<-c(n,pos3)
}
ojo<-ojo[n,]
ojo<-na.omit(ojo)
colnames(ojo)<-c("X","mammal")
oo<-read.csv("F:/Function/grid/mammal_threat&ddgrid.csv")
oo[,3]<-0
oo<-oo[,-1]
colnames(oo)<-c("X","mammal")

a<-intersect(ojo[,1],oo[,1])
n<-as.numeric()
for (i in 1:length(a)) {
  pos3<-which(ojo[,1]==a[i])
  n<-c(n,pos3)
}
ojo<-ojo[n,]
colnames(ojo)[1]<-c("OBJECTID")
pos<-which(ojo[,2]<0)
ojo<-ojo[-pos,]

data1<-join(data,ojo)
data1<-data1[,c("MAT","MAP","TSN","PSN","HFP","HFP_change","lon","lat","mammal")]
data1<-na.omit(data1)

model_mammal1 <- gam(mammal ~ s(lon, lat) + s(MAT,k=5), data = data1,family = quasibinomial(link = "logit"))


model_mammal2 <- gam(mammal ~ s(lon, lat) + s(MAP,k=5), data = data1,family = quasibinomial(link = "logit"))
plot_predictions(model_mammal2, condition = 'MAP', 
                 type = 'response',
                 rug = F) 
model_mammal3 <- gam(mammal ~ s(lon, lat) + s(TSN,k=5), data = data1,family = quasibinomial(link = "logit"))
plot_predictions(model_mammal3, condition = 'TSN', 
                 type = 'response',
                 rug = F) 
model_mammal4 <- gam(mammal ~ s(lon, lat) + s(PSN,k=5), data = data1,family = quasibinomial(link = "logit"))
plot_predictions(model_mammal4, condition = 'PSN', 
                 type = 'response',
                 rug = F) 
model_mammal5 <- gam(mammal ~ s(lon, lat) + s(HFP,k=5), data = data1,family = quasibinomial(link = "logit"))
plot_predictions(model_mammal5, condition = 'HFP', 
                 type = 'response',
                 rug = F) 
model_mammal6 <- gam(mammal ~ s(lon, lat) + s(HFP_change,k=5), data = data1,family = quasibinomial(link = "logit"))
plot_predictions(model_mammal6, condition = 'HFP_change', 
                 type = 'response',
                 rug = F) 


ojo<-read.csv("F:/Function/kernel_alpha/bird_allspecies.csv")
ojo<-ojo[,-1]
ojo<-ojo[,-2]
ojo<-na.omit(ojo)

ojo[,1]<-paste("g",ojo[,1],sep="")
ojo1<-ojo
ojo<-read.csv("F:/Function/kernel_alpha/bird_change_disp_ddsample.csv")
ojo<-ojo[,-1]
ojo<-ojo[,-2]
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
o<-read.csv("C:/Users/13663/Desktop/langrid")
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
oo<-read.csv("F:/Function/grid/bird_threat&ddgrid.csv")
oo[,3]<-0
oo<-oo[,-1]
colnames(oo)<-c("X","Bird")

a<-intersect(ojo[,1],oo[,1])
n<-as.numeric()
for (i in 1:length(a)) {
  pos3<-which(ojo[,1]==a[i])
  n<-c(n,pos3)
}
ojo<-ojo[n,]
colnames(ojo)[1]<-c("OBJECTID")
pos<-which(ojo[,2]<0)
ojo<-ojo[-pos,]

data2<-join(data,ojo)
data2<-data2[,c("MAT","MAP","TSN","PSN","HFP","HFP_change","lon","lat","Bird")]
data2<-na.omit(data2)
model_bird1 <- gam(Bird ~ s(lon, lat) + s(MAT,k=5), data = data2,family = quasibinomial(link = "logit"))
plot_predictions(model_bird1, condition = 'MAT', 
                 type = 'response',
                 rug = F) 
model_bird2 <- gam(Bird ~ s(lon, lat) + s(MAP,k=5), data = data2,family = quasibinomial(link = "logit"))
plot_predictions(model_bird2, condition = 'MAP', 
                 type = 'response',
                 rug = F) 
model_bird3 <- gam(Bird ~ s(lon, lat) + s(TSN,k=5), data = data2,family = quasibinomial(link = "logit"))
plot_predictions(model_bird3, condition = 'TSN', 
                 type = 'response',
                 rug = F) 
model_bird4 <- gam(Bird ~ s(lon, lat) + s(PSN,k=5), data = data2,family = quasibinomial(link = "logit"))
plot_predictions(model_bird4, condition = 'PSN', 
                 type = 'response',
                 rug = F) 
model_bird5 <- gam(Bird ~ s(lon, lat) + s(HFP,k=5), data = data2,family = quasibinomial(link = "logit"))
plot_predictions(model_bird5, condition = 'HFP', 
                 type = 'response',
                 rug = F) 
model_bird6 <- gam(Bird ~ s(lon, lat) + s(HFP_change,k=5), data = data2,family = quasibinomial(link = "logit"))
plot_predictions(model_bird6, condition = 'HFP_change', 
                 type = 'response',
                 rug = F) 


ojo<-read.csv("F:/Function/kernel_alpha/amphibian_allspecies.csv")
ojo<-ojo[,-1]
ojo<-ojo[,-2]
ojo<-na.omit(ojo)

ojo[,1]<-paste("g",ojo[,1],sep="")
ojo1<-ojo
ojo<-read.csv("F:/Function/kernel_alpha/amphibian_change_disp_ddsample.csv")
ojo<-ojo[,-1]
ojo<-ojo[,-2]
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
o<-read.csv("C:/Users/13663/Desktop/langrid")
o<-o[,2:3]
a<-intersect(ojo[,1],o[,1])
n<-as.numeric()
for (i in 1:length(a)) {
  pos3<-which(ojo[,1]==a[i])
  n<-c(n,pos3)
}
ojo<-ojo[n,]
ojo<-na.omit(ojo)
colnames(ojo)<-c("X","amphibian")
oo<-read.csv("F:/Function/grid/amphibian_threat&ddgrid.csv")
oo[,3]<-0
oo<-oo[,-1]
colnames(oo)<-c("X","amphibian")

a<-intersect(ojo[,1],oo[,1])
n<-as.numeric()
for (i in 1:length(a)) {
  pos3<-which(ojo[,1]==a[i])
  n<-c(n,pos3)
}
ojo<-ojo[n,]
colnames(ojo)[1]<-c("OBJECTID")
pos<-which(ojo[,2]<0)
ojo<-ojo[-pos,]

data3<-join(data,ojo)
data3<-data3[,c("MAT","MAP","TSN","PSN","HFP","HFP_change","lon","lat","amphibian")]
data3<-na.omit(data3)
model_amphibian1 <- gam(amphibian ~ s(lon, lat) + s(MAT,k=5), data = data3,family = quasibinomial(link = "logit"))
plot_predictions(model_amphibian1, condition = 'MAT', 
                 type = 'response',
                 rug = F) 
model_amphibian2 <- gam(amphibian ~ s(lon, lat) + s(MAP,k=5), data = data3,family = quasibinomial(link = "logit"))
plot_predictions(model_amphibian2, condition = 'MAP', 
                 type = 'response',
                 rug = F) 
model_amphibian3 <- gam(amphibian ~ s(lon, lat) + s(TSN,k=5), data = data3,family = quasibinomial(link = "logit"))
plot_predictions(model_amphibian3, condition = 'TSN', 
                 type = 'response',
                 rug = F) 
model_amphibian4 <- gam(amphibian ~ s(lon, lat) + s(PSN,k=5), data = data3,family = quasibinomial(link = "logit"))
plot_predictions(model_amphibian4, condition = 'PSN', 
                 type = 'response',
                 rug = F) 
model_amphibian5 <- gam(amphibian ~ s(lon, lat) + s(HFP,k=5), data = data3,family = quasibinomial(link = "logit"))
plot_predictions(model_amphibian5, condition = 'HFP', 
                 type = 'response',
                 rug = F) 
model_amphibian6 <- gam(amphibian ~ s(lon, lat) + s(HFP_change,k=5), data = data3,family = quasibinomial(link = "logit"))
plot_predictions(model_amphibian6, condition = 'HFP_change', 
                 type = 'response',
                 rug = F) 
gam.check(model_amphibian6)

ojo<-read.csv("F:/Function/kernel_alpha/reptile_allspecies.csv")
ojo<-ojo[,-1]
ojo<-ojo[,-2]
ojo<-na.omit(ojo)

ojo[,1]<-paste("g",ojo[,1],sep="")
ojo1<-ojo
ojo<-read.csv("F:/Function/kernel_alpha/reptile_change_disp_ddsample.csv")
ojo<-ojo[,-1]
ojo<-ojo[,-2]
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
o<-read.csv("C:/Users/13663/Desktop/langrid")
o<-o[,2:3]
a<-intersect(ojo[,1],o[,1])
n<-as.numeric()
for (i in 1:length(a)) {
  pos3<-which(ojo[,1]==a[i])
  n<-c(n,pos3)
}
ojo<-ojo[n,]
ojo<-na.omit(ojo)
colnames(ojo)<-c("X","reptile")
oo<-read.csv("F:/Function/grid/reptile_threat&ddgrid.csv")
oo[,3]<-0
oo<-oo[,-1]
colnames(oo)<-c("X","reptile")

a<-intersect(ojo[,1],oo[,1])
n<-as.numeric()
for (i in 1:length(a)) {
  pos3<-which(ojo[,1]==a[i])
  n<-c(n,pos3)
}
ojo<-ojo[n,]
colnames(ojo)[1]<-c("OBJECTID")
pos<-which(ojo[,2]<0)
ojo<-ojo[-pos,]
data4<-join(data,ojo)
data4<-data4[,c("MAT","MAP","TSN","PSN","HFP","HFP_change","lon","lat","reptile")]
data4<-na.omit(data4)
model_reptile1 <- gam(reptile ~ s(lon, lat) + s(MAT,k=5), data = data4,family = quasibinomial(link = "logit"))
plot_predictions(model_reptile1, condition = 'MAT', 
                 type = 'response',
                 rug = F) 
model_reptile2 <- gam(reptile ~ s(lon, lat) + s(MAP,k=5), data = data4,family = quasibinomial(link = "logit"))
plot_predictions(model_reptile2, condition = 'MAP', 
                 type = 'response',
                 rug = F) 
model_reptile3 <- gam(reptile ~ s(lon, lat) + s(TSN,k=5), data = data4,family = quasibinomial(link = "logit"))
plot_predictions(model_reptile3, condition = 'TSN', 
                 type = 'response',
                 rug = F) 
model_reptile4 <- gam(reptile ~ s(lon, lat) + s(PSN,k=5), data = data4,family = quasibinomial(link = "logit"))
plot_predictions(model_reptile4, condition = 'PSN', 
                 type = 'response',
                 rug = F) 
model_reptile5 <- gam(reptile ~ s(lon, lat) + s(HFP,k=5), data = data4,family = quasibinomial(link = "logit"))
plot_predictions(model_reptile5, condition = 'HFP', 
                 type = 'response',
                 rug = F) 
model_reptile6 <- gam(reptile ~ s(lon, lat) + s(HFP_change,k=5), data = data4,family = quasibinomial(link = "logit"))
plot_predictions(model_reptile6, condition = 'HFP_change', 
                 type = 'response',
                 rug = F) 
gam.check(model_reptile1)

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
ggsave("F:/Function/fig_revision/gam_kernel_dispersion_ddsample.png", plot = p, width = 11, height = 6.5, dpi = 600,bg = "white")
