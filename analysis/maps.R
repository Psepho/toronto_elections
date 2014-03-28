source("analysis//setup.R")
#-----------
# Packages
#-----------
library(ggplot2)
library(ggmap)
library(maptools)
#-----------
# Download shapefiles
#-----------
years <- c(2006,2010)
for (year in years) {
  var <- paste("shapefile_url_",year,sep="")
  assign(var,paste("http://opendata.toronto.ca/gcc/voting_subdivision_",year,"_wgs84.zip",sep=""))
  if(file.exists(paste("tmp/subdivisions_",year,".zip",sep=""))) {
    # Nothing to do
  }  else {
    download.file(var,
                  destfile = paste("tmp/subdivisions_",year,".zip",sep=""))
    unzip(paste("tmp/subdivisions_",year,".zip",sep=""), exdir="tmp")
  }
  shape <- paste("shapefile_",year,sep="")
  file <- paste("tmp/VOTING_SUBDIVISION_",year,"_WGS84.shp",sep="")
  assign(shape,readShapeSpatial(file, proj4string=CRS("+proj=longlat +datum=WGS84")))
  rm(file,var,shape)
}
rm(year,years)
#-----------
# Standard base map
#-----------
toronto_map <- qmap("queens park,toronto",zoom=11, maptype = 'terrain')
#---------
# Point maps
#---------
point_map <- function(data,variable){
  toronto_map + geom_point(aes_string(x="long",y="lat",colour=variable),data=data) + facet_wrap(~year)
}
point_map(turnout_geo,"turnout")
point_map(turnout_geo,"total_votes")
point_map(positions_geo,"weighted_votes")
#---------
# Shapefiles
#---------
# Extract the data object for each year and then combine into one data frame
data_2010 <- fortify(shapefile_2010,region="AREA_NAME")
data_2010$year <- as.integer(2010)
data_2006 <- fortify(shapefile_2006,region="AREA_NAME")
data_2006$year <- as.integer(2006)
data <- rbind(data_2010,data_2006)
rm(data_2006,data_2010)
data$ward_area <- paste(as.integer(str_sub(data$id,1,2)),as.integer(str_sub(data$id,-3,-1)),sep="_")
data <- as.data.frame(inner_join(data,positions_geo[,c(1,6:8)], by=c("ward_area","year")))
# Map the results
toronto_map +
  geom_polygon(aes(x=long, y=lat, group=group, fill=weighted_votes), alpha = 5/6, data=data) + 
  scale_fill_gradient2("Left-Right Score", midpoint = median(data$weighted_votes), mid = "white",limit=c(0.25,0.85)) +
  facet_wrap(~year)
#scale_fill_brewer(palette ="PuOr",type="div","Left-Right Score")
