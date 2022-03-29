# clip north gulf of california
# MAC 03/24/22

#library(ggplot)
library(rgdal)
library(raster)
library(rgeos)

#countries<-map_data("world")
#countries<-subset(countries, region %in% c("Mexico"))

MX<-getData('GADM', country='MEX', level=1)

b<-bbox(MX)

# -114.895020,29.683281,-112.966919,31.835566
b[1,1]<-(-114.895020)
b[1,2]<-(-112.966919)
b[2,1]<-29.683281
b[2,2]<-31.835566

gClip <- function(shp, bb){
  if(class(bb) == "matrix") b_poly <- as(extent(as.vector(t(bb))), "SpatialPolygons")
  else b_poly <- as(extent(bb), "SpatialPolygons")
  gIntersection(shp, b_poly, byid = T)
}
zones_clipped <- gClip(MX, b)