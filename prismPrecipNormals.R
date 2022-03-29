# develop PRISM precipitation normals for state maps
# MAC 03/23/22

library(prism)
library(raster)
library(RCurl)
library(jsonlite)

prism_set_dl_dir("/home/crimmins/RProjects/StateMonsoonMaps/prism")

get_prism_normals(type="ppt", resolution = "4km", mon = 6:9, keepZip = FALSE)

files<-prism_archive_ls()
JJASppt<-pd_stack(files)

# get ACIS grid

## ---- download PRISM data ----
# Manually set universal date range
dateRangeStart="2018-06-15"
dateRangeEnd="2018-09-30"
allDates<-seq(as.Date(dateRangeStart), as.Date(dateRangeEnd),1)

# Automated Universal date range
# days<-35
# dateRangeEnd<-as.Date(format(Sys.time(), "%Y-%m-%d"))-1
# dateRangeStart<-as.Date(format(Sys.time(), "%Y-%m-%d"))-days
# allDates<-seq(as.Date(dateRangeStart), as.Date(dateRangeEnd),1)

# get shapefiles
all_states <- map_data("state")

# AZ/NM bbox -115.004883,31.184609,-102.524414,37.387617
ACISbbox<-"-115,31,-102,38"

# ACIS query
jsonQuery=paste0('{"bbox":"',ACISbbox,'","sdate":"',dateRangeStart,'","edate":"',dateRangeEnd,'","grid":"21","elems":"pcpn","meta":"ll,elev","output":"json"}') # or uid
#jsonQuery=paste0('{"bbox":"',ACISbbox,'","sdate":"',dateRangeStart,'","edate":"',dateRangeEnd,'","grid":"2","elems":"pcpn","meta":"ll","output":"json"}') # or uid

out<-postForm("http://data.rcc-acis.org/GridData", 
              .opts = list(postfields = jsonQuery, 
                           httpheader = c('Content-Type' = 'application/json', Accept = 'application/json')))
out<-fromJSON(out)

# convert to list of matrices, flipud with PRISM
matrixList <- vector("list",length(out$data))
for(i in 1:length(out$data)){
  matrixList[[i]]<-apply(t(out$data[[i]][[2]]),1,rev) 
}

# read into raster stack
rasterList<-lapply(matrixList, raster)
gridStack<-stack(rasterList)
gridExtent<-extent(min(out$meta$lon), max(out$meta$lon), min(out$meta$lat), max(out$meta$lat))
gridStack<-setExtent(gridStack, gridExtent, keepres=FALSE, snap=FALSE)
names(gridStack)<-allDates
# set 0 and neg to NA
gridStack[gridStack <= 0] <- NA
## ----

JJASppt <- crop(JJASppt, extent(gridStack[[1]]))
JJASppt <- resample(JJASppt,gridStack[[1]],method='bilinear')

writeRaster(JJASppt, filename="/home/crimmins/RProjects/StateMonsoonMaps/JJASppt.grd", overwrite=TRUE)