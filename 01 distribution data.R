####species range data 1°x1°
library(sp)
##build a global grid map with resolution of  1°x1°
latlon.polygons <- function(lon=c(-180,180,1),lat=c(-90,90,1),LLID = TRUE, fn=NULL, ...)
{
  require(sp)
  cellsize <- c(lon[3], lat[3])
  cells.dim <- c(ceiling((lon[2]-lon[1])/cellsize[1]),ceiling((lat[2]-lat[1])/cellsize[2]))
  cellcentre.offset <- c(lon[1]+cellsize[1]/2, lat[1]+cellsize[2]/2)
  grid.tp <- GridTopology(cellcentre.offset=cellcentre.offset, cellsize=cellsize, cells.dim=cells.dim)
  ll.polygons <- as.SpatialPolygons.GridTopology(grid.tp)#, proj4string = proj4string)
  ll.d <- coordinates(ll.polygons)
  if (!LLID)
  {ll.d <- as.data.frame(ll.d); colnames(ll.d) <- c("lon","lat")}
  else
  {ll.d <- as.data.frame(cbind(ll.d,(ll.d[,2]+90+cellsize[2]/2)*1000+(ll.d[,1]+180+cellsize[1]/2)))
  colnames(ll.d) <- c("lon","lat","LLID")}
  rownames(ll.d) <- paste("g",1:dim(ll.d)[1],sep="") 
  ll.polyshape <- SpatialPolygonsDataFrame(ll.polygons, data=ll.d)
  if (!is.null(fn)) writePolyShape(ll.polyshape, fn=fn)
  else return(ll.polyshape)
}
grid50 <- latlon.polygons(lon=c(-180,180,1),lat=c(-90,90,1),LLID = TRUE, fn=NULL)

###overlap species range data
library(rgdal)
library(maptools)
library(rgeos)
d<-readOGR( "C:/Users/13663/Desktop/AM","AMPHIBIANS")##for example of amphibians

proj4string(grid50) <- CRS("+proj=longlat")
grid50proj <- spTransform(grid50, CRS("+proj=cea"))
proj4string(d) <- CRS("+proj=longlat")
dproj <- spTransform(d, CRS("+proj=cea"))
sp.all <- unique(d@data[,"binomial"])
sp.dis <- matrix(NA, nrow=0, ncol=2) 
for (i in 1:length(sp.all))
  foreach (i = 1:length(sp.all), .combine = 'c')%dopar% 
  {
    sp <- sp.all[i]
    pos <- which(d@data[,"binomial"] == sp)
    d.sp <- dproj[pos,]
    over50 <-gIntersection(grid50proj,d.sp, byid=T ,drop_lower_td=T)
    grid50.id <- unlist(lapply(strsplit(names(over50),split=" "),"[",1))
    a <- gArea(over50,byid=T)
    if (length(grid50.id[a > 1000000])==0)
      grid50.id.sel <-F  else
        grid50.id.sel <- grid50.id[a > 1000000]
    s<-cbind(spname=sp, grid50=grid50.id.sel)
    sp.dis <- rbind(sp.dis,s)
    
  }
write.csv(sp.dis,"C:/Users/13663/Desktop/AM/sp_range")
