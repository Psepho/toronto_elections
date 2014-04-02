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
  if(file.exists(paste("tmp/subdivisions_",year,".zip",sep=""))) {
    # Nothing to do
  }  else {
    download.file(paste("http://opendata.toronto.ca/gcc/voting_subdivision_",year,"_wgs84.zip",sep=""),
                  destfile = paste("tmp/subdivisions_",year,".zip",sep=""))
    unzip(paste("tmp/subdivisions_",year,".zip",sep=""), exdir="tmp")
  }
  shape <- paste("shapefile_",year,sep="")
  file <- paste("tmp/VOTING_SUBDIVISION_",year,"_WGS84.shp",sep="")
  assign(shape,readShapeSpatial(file, proj4string=CRS("+proj=longlat +datum=WGS84")))
  rm(file,shape)
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
data_2003 <- fortify(shapefile_2006,region="AREA_NAME") # Assuming 2006 locations for 2003 data
data_2003$year <- as.integer(2003)
data <- rbind(data_2010,data_2006,data_2003)
rm(data_2003,data_2006,data_2010)
data$ward_area <- paste(as.integer(str_sub(data$id,1,2)),as.integer(str_sub(data$id,-3,-1)),sep="_")
data <- as.data.frame(inner_join(data,positions_geo[,-c(2:5)], by=c("ward_area","year")))
#---------
# Map the results
#---------
# Positions
toronto_map +
  geom_polygon(aes(x=long, y=lat, group=group, fill=cut_interval(weighted_votes, length=0.15)), alpha = 5/6, data=data) + 
  scale_fill_brewer("Position", type="div") +   
  #scale_fill_gradient2("Left-Right Score", midpoint = median(data$weighted_votes), mid = "white",limit=c(0.25,0.85)) +
  facet_wrap(~year)
# Turnout
toronto_map +
  geom_polygon(aes(x=long, y=lat, group=group, fill=cut_interval(turnout,length = 0.25)), alpha = 5/6, data=data) + 
  scale_fill_brewer("Turnout") + 
  facet_wrap(~year)
# Change in position
toronto_map +
  geom_polygon(aes(x=long, y=lat, group=group, fill=cut_interval(turnout,length = 0.25)), alpha = 5/6, data=subset(data,year!=2003)) + # Exclude 2003
  scale_fill_brewer("Change in position") + 
  facet_wrap(~year)

positions_geo_high_turnout <- positions_geo %.%
  filter(turnout>0.5)
toronto_map +
  geom_polygon(aes(x=long, y=lat, group=group, fill=weighted_votes_change), alpha = 5/6, data=subset(data, year==2010 & turnout>0.5)) + 
  scale_fill_gradient("Change in position", low="white", high="red", space="Lab")


positions_in_active_areas <- tbl_df(merge(active_areas,data))
toronto_map +
  geom_polygon(aes(x=long, y=lat, group=group, fill=weighted_votes_change), alpha = 5/6, data=positions_in_active_areas) + 
  scale_fill_gradient("Change in position", low="white", high="red", space="Lab")


data_2014 <- fortify(shapefile_2010,region="AREA_NAME")
data_2014$year <- as.integer(2014)
data_2014$ward_area <- paste(as.integer(str_sub(data_2014$id,1,2)),as.integer(str_sub(data_2014$id,-3,-1)),sep="_")
scenario_geo <- tbl_df(scenario) %.%
  mutate(ward_area=paste(ward,area,sep="_"))
data <- as.data.frame(inner_join(data_2014,scenario_geo, by=c("ward_area")))
toronto_map +
  geom_polygon(aes(x=long, y=lat, group=group, fill=value, alpha = 5/6, data=data)) +
  scale_fill_gradient("Votes") + 
  facet_wrap(~variable)


#scale_fill_brewer(palette ="PuOr",type="div","Left-Right Score")
