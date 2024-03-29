# State-level MRMS monsoon mapping
# adapted from plotMonsoonPRISM.R
# Get Gridded data from ACIS
# MAC 07/22/2021

library(RCurl)
library(jsonlite)

library(ggplot2)
#library(ggmap)
library(scales)
library(reshape2)
library(raster)
library(rasterVis)
library(PBSmapping)
library(Hmisc)
library(rgdal)
library(readr)
library(magick)

# library(leaflet)
# library(mapview)
# library(leafem)
# library(htmlwidgets)
# library(rmarkdown)
# library(knitr)
# library(rmdformats)


## ---- download PRISM data ----
# Manually set universal date range - ACIS PRISM current day-1, correct for LINUX UTC time
#dateRangeStart="2019-06-15"
#dateRangeEnd="2019-09-30"

# auto date range...start with 6-15 and run on 6-17 to get two days of data, end on 10/1
dateRangeStart="2021-06-15"
dateRangeEnd=as.Date(format(as.POSIXct(Sys.Date()),usetz=TRUE, tz="Etc/GMT+7")) # date on local time zone
if(dateRangeEnd<"2021-06-16" | dateRangeEnd>="2021-10-01"){
  stop()
}

# generate dates -- keep with PRISM date
allDates<-seq(as.Date(dateRangeStart), as.Date(dateRangeEnd),1)
#allDates<-as.Date(format(allDates, format="%m-%d-%Y"),format="%m-%d-%Y")

# Automated Universal date range
# days<-35
# dateRangeEnd<-as.Date(format(Sys.time(), "%Y-%m-%d"))-1
# dateRangeStart<-as.Date(format(Sys.time(), "%Y-%m-%d"))-days
# allDates<-seq(as.Date(dateRangeStart), as.Date(dateRangeEnd),1)

# get shapefiles
all_states <- map_data("state")
all_counties<-map_data("county")
tribes <- readOGR("/home/crimmins/RProjects/ClimPlot/tribes", "indlanp020")
# load cities
SWCities <- read_csv("/home/crimmins/RProjects/ClimPlot/SWCities.csv")

# state switch
#ACISbbox<-"-115,31,-102,38"
stateName<-c("Arizona","New Mexico")
stateAbb<-c("AZ","NM")
ACISbbox<-c("-115,31,-108.75,37.2","-109.25,31,-102.8,37.2") # AZ
  lons<-list(c(-115,-108.75),c(-109.25,-102.8))
  lats<-c(31,37.2)
  
k=1

# ACIS query
jsonQuery=paste0('{"bbox":"',ACISbbox[k],'","sdate":"',dateRangeStart,'","edate":"',dateRangeEnd,'","grid":"2","elems":"pcpn","meta":"ll","output":"json"}') # or uid
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
# grab grid for ts extent
gridStackTS<-gridStack
# set 0 and neg to NA
gridStack[gridStack <= 0] <- NA
## ----

# create summary grids ----
# total precip
totalPrecipAll<-calc(gridStack, sum, na.rm=TRUE)
totalPrecipAll_w0<-totalPrecipAll
totalPrecipAll[totalPrecipAll <= 0] <- NA
totalPrecipAll_w0[totalPrecipAll_w0 < 0]<- NA

# percent of average
JJASppt<-stack("/home/crimmins/RProjects/ClimPlot/PRISM/JJASppt.grd")
JJASppt<-JJASppt/25.4
moStack<-as.numeric(format(allDates[length(allDates)], format='%m'))-5
# resample PRISM to MRMS
JJASppt<-crop(JJASppt, extent(totalPrecipAll))

if (moStack==1) {
  normal<-JJASppt[[moStack]]*(as.numeric(format(allDates[length(allDates)], format='%d'))/monthDays(allDates[length(allDates)]))
} else if ( moStack==2) {
  norm1<-JJASppt[[moStack]]*(as.numeric(format(allDates[length(allDates)], format='%d'))/monthDays(allDates[length(allDates)]))
  normal<-norm1+JJASppt[[(moStack-1)]]
} else {
  norm1<-JJASppt[[moStack]]*(as.numeric(format(allDates[length(allDates)], format='%d'))/monthDays(allDates[length(allDates)]))
  norm2<-calc(JJASppt[[1:(moStack-1)]], sum, na.rm=TRUE)
  normal<-norm1+norm2
}
# resample PRISM to AHPS
normal<-resample(normal, totalPrecipAll, method="bilinear")
percPrecip<-totalPrecipAll/normal

# perc days with rain >0.01"
rainDays <- calc(gridStack, fun=function(x){sum(x > 0.01, na.rm = TRUE)})
percRainDays<-(rainDays/length(allDates))*100
percRainDays[percRainDays <= 0] <- NA

# daily intensity index
sdii<-totalPrecipAll/rainDays

# max 1-day precip
maxRain <- calc(gridStack, fun=function(x){max(x, na.rm = TRUE)})
maxRain[maxRain <= 0] <- NA

# days since 0.05" rainfall
daysSince <-length(allDates)-(calc(gridStack, fun=function(x){max(which(x >= 0.05), na.rm = TRUE)}))
daysSince[daysSince < 0] <- NA
daysSince[daysSince==Inf] <- NA

# process daily anomalies ----
dailyAnoms= list()
i<-1
for(i in 2:nlayers(gridStack)){
  totalPrecip<-calc(gridStack[[1:i]], sum, na.rm=TRUE)
  # percent of average
  JJASppt<-stack("/home/crimmins/RProjects/ClimPlot/PRISM/JJASppt.grd")
  JJASppt<-JJASppt/25.4
  moStack<-as.numeric(format(allDates[i], format='%m'))-5
  # resample PRISM to MRMS
  JJASppt<-crop(JJASppt, extent(totalPrecipAll))
  
  if (moStack==1) {
    normal<-JJASppt[[moStack]]*(as.numeric(format(allDates[i], format='%d'))/monthDays(allDates[i]))
  } else if ( moStack==2) {
    norm1<-JJASppt[[moStack]]*(as.numeric(format(allDates[i], format='%d'))/monthDays(allDates[i]))
    normal<-norm1+JJASppt[[(moStack-1)]]
  } else {
    norm1<-JJASppt[[moStack]]*(as.numeric(format(allDates[i], format='%d'))/monthDays(allDates[i]))
    norm2<-calc(JJASppt[[1:(moStack-1)]], sum, na.rm=TRUE)
    normal<-norm1+norm2
  }
  normal<-resample(normal, totalPrecipAll, method="bilinear")
  percPrecip<-(totalPrecip/normal)*100
  
  freq=hist(getValues(percPrecip),c(0,75,125,200,Inf), plot=FALSE)
  dailyAnoms[[i-1]] <- t(freq$counts/sum(freq$counts))
}
# create data table
dailyAnoms = as.data.frame(do.call(rbind, dailyAnoms))
dailyAnoms$Date<-allDates[2:i]
colnames(dailyAnoms)<-c("Below Average","Near Average","Above Average","Much Above Average","Date")

dailyAnomsMelt<-melt(dailyAnoms, id.vars = "Date")
colnames(dailyAnomsMelt)<-c("Date","Anomaly","Coverage")
dailyAnomsMelt$Coverage<-dailyAnomsMelt$Coverage*100

# plot anomaly time series 
p<-ggplot(dailyAnomsMelt, aes(x=Date,y=Coverage,fill=Anomaly))+
  geom_area(color="black")+
  scale_fill_manual(values=c("tan4", "white","green4","blue"),
                    labels=c("Below Avg (<75%)","Near Avg (75-125%)","Above Avg (125-200%)","Much Above Avg (>200%)"))+
  scale_x_date(expand = c(0, 0), date_breaks = "1 weeks", date_labels = "%b-%d") +
  scale_y_continuous(expand = c(0, 0))+
  ylab(paste0("Percent of ",stateName[k]," in Anomaly Category"))+
  guides(fill=guide_legend(title="Anom Category"))+
  ggtitle(paste0("Precipitation Anomaly (% of Ave) Coverage: ",allDates[1]," to ",allDates[length(allDates)]))+
  labs(caption=paste0("Plot created: ",format(Sys.time(), "%Y-%m-%d"),
                      "\nThe University of Arizona\nhttps://cals.arizona.edu/climate/\nData Source: NOAA Multi Sensor Precip Estimate\nRCC-ACIS"))+
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14),
        plot.title=element_text(size=14, face = "bold"),
        legend.position = c(0.1, 0.88),
        legend.background = element_rect(fill="white",
                                         size=0.5, linetype="solid", 
                                         colour ="black"),
        axis.text.x = element_text(angle = -45, hjust = 0))

# write out file
png(paste0("/home/crimmins/RProjects/StateMonsoonMaps/maps/",stateAbb[k],"/",stateAbb[k],"_Monsoon_Anomaly_TS.png"), width = 11, height = 8, units = "in", res = 300L)
#grid.newpage()
print(p, newpage = FALSE)
dev.off()

# add logos
# Call back the plot
plot <- image_read(paste0("/home/crimmins/RProjects/StateMonsoonMaps/maps/",stateAbb[k],"/",stateAbb[k],"_Monsoon_Anomaly_TS.png"))
# And bring in a logo
logo_raw <- image_read("/home/crimmins/RProjects/ClimPlot/logos/UA_CSAP_CLIMAS_logos_horiz.png") 
logo <- image_resize(logo_raw, geometry_size_percent(width=120,height = 120))
# Stack them on top of each other
#final_plot <- image_append((c(plot, logo)), stack = TRUE)
#final_plot <- image_mosaic((c(plot, logo)))
final_plot <- image_composite(plot, logo, offset = "+180+2130")
# And overwrite the plot without a logo
image_write(final_plot, paste0("/home/crimmins/RProjects/StateMonsoonMaps/maps/",stateAbb[k],"/",stateAbb[k],"_Monsoon_Anomaly_TS.png"))
# ----


# PLOT AND SAVE MAPS ----
# load map boundary data 
# ACISbbox<-"-115,31,-108.75,37.2" # AZ
xlim = lons[[k]]
ylim = lats
# state boundaries
colnames(all_states)<-c("X","Y","PID","POS","region","subregion")
states= clipPolys(all_states, xlim=xlim,ylim=ylim, keepExtra=TRUE)
# county boundaries
colnames(all_counties)<-c("X","Y","PID","POS","region","subregion")
all_states = clipPolys(all_counties, xlim=xlim,ylim=ylim, keepExtra=TRUE)
# tribal boundaries
tribes_df <- fortify(tribes)
#colnames(tribes_df)<-c("X","Y","POS","hole","piece","id","PID")
#tribes_df = clipPolys(tribes_df, xlim=xlim,ylim=ylim, keepExtra=TRUE)


# total precip Map -----
# colorramp for total precip
precipCols<-colorRampPalette(c("lightblue", "dodgerblue3", "palegreen","green4","salmon","orangered3",
                               "lightgoldenrod1","orange2","plum2","purple"))(50)
precBreaks<-seq(0,20,2)
precLabs<-as.character(seq(0,20,2))
precLabs[11]<-">20"
precLabs[1]<-"0.01"
#precBreaksmin<-seq(1,19,2)

#theme_set(theme_bw())

p<-gplot(totalPrecipAll) + geom_tile(aes(fill = value)) +
  #scale_fill_gradient2(low = 'white', high = 'blue') +
  #scale_fill_distiller(palette = "Spectral", direction = -1, na.value="darkgoldenrod", 
  #                     name="inches", limits=c(0,20),oob=squish)+
  
  scale_fill_gradientn(colours = precipCols, na.value="darkgoldenrod", 
                       name="inches", limits=c(0,20),oob=squish, breaks=precBreaks, labels=precLabs, expand=NULL)+
  guides(fill= guide_colorbar(barheight=15,nbin = 500, raster = FALSE))+
  
  coord_equal(xlim = xlim, ylim = ylim, expand = FALSE)+
  xlab("Longitude") + ylab("Latitude") 

p<-p +  geom_polygon( data=all_states, aes(x=X, y=Y, group = PID),colour="black", fill=NA, size=0.25 )+
  #scale_x_continuous(breaks = c(-120,-140))+
  #ggtitle("Daily Total Precip (inches) - PRISM")+
  ggtitle(paste0("Total Precipitation (in.): ",allDates[1]," to ",allDates[length(allDates)]))+
  labs(caption=paste0("Plot created: ",format(Sys.time(), "%Y-%m-%d"),
                      "\nThe University of Arizona\nhttps://cals.arizona.edu/climate/\nData Source: PRISM Climate Group\nRCC-ACIS"))+
  theme(plot.title=element_text(size=14, face = "bold"))

p<-p+geom_path(data = tribes_df, 
               aes(x = long, y = lat, group = group),
               color = 'azure4', size = .2)

p<-p+geom_point(data = SWCities, aes(x = lon, y = lat), size = 1, 
                shape = 20)

p<-p+geom_text(data = SWCities, aes(x = lon, y = lat, label = City), 
               size = 3, col = "black", fontface = "bold", nudge_y = 0.1)

# write out file
png(paste0("/home/crimmins/RProjects/StateMonsoonMaps/maps/",stateAbb[k],"/",stateAbb[k],"_Monsoon_TotalPrecip.png"),
    width = 16, height = 8, units = "in", res = 300L)
#grid.newpage()
print(p, newpage = FALSE)
dev.off()

# add logos
# Call back the plot
plot <- image_read(paste0("/home/crimmins/RProjects/StateMonsoonMaps/maps/",stateAbb[k],"/",stateAbb[k],"_Monsoon_TotalPrecip.png"))
# And bring in a logo
#logo_raw <- image_read("./logos/UA_CLIMAS_logos.png")
logo_raw <- image_read("/home/crimmins/RProjects/ClimPlot/logos/UA_CSAP_CLIMAS_logos_horiz.png") 
logo <- image_resize(logo_raw, geometry_size_percent(width=120,height = 120))
# Stack them on top of each other
#final_plot <- image_append((c(plot, logo)), stack = TRUE)
#final_plot <- image_mosaic((c(plot, logo)))
final_plot <- image_composite(plot, logo, offset = "+510+2150")
# And overwrite the plot without a logo
image_write(final_plot, paste0("/home/crimmins/RProjects/StateMonsoonMaps/maps/",stateAbb[k],"/",stateAbb[k],"_Monsoon_TotalPrecip.png"))



