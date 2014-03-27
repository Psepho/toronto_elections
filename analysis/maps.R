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
# TODO: Wrap these in functions based on a list of years
shapefile_url_2010 <- "http://opendata.toronto.ca/gcc/voting_subdivision_2010_wgs84.zip"
shapefile_url_2006 <- "http://opendata.toronto.ca/gcc/voting_subdivision_2006_wgs84.zip"
if(file.exists("tmp/subdivisions_2010.zip")) {
  # Nothing to do
}  else {
  download.file(shapefile_url_2010,
                destfile = "tmp/subdivisions_2010.zip")
  unzip("tmp/subdivisions_2010.zip", exdir="tmp")
}
shapefile_2010 <- readShapeSpatial("tmp/VOTING_SUBDIVISION_2010_WGS84.shp", proj4string=CRS("+proj=longlat +datum=WGS84"))
if(file.exists("tmp/subdivisions_2006.zip")) {
  # Nothing to do
}  else {
  download.file(shapefile_url_2006,
                destfile = "tmp/subdivisions_2006.zip")
  unzip("tmp/subdivisions_2006.zip", exdir="tmp")
}
shapefile_2006 <- readShapeSpatial("tmp/VOTING_SUBDIVISION_2006_WGS84.shp", proj4string=CRS("+proj=longlat +datum=WGS84"))
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
  scale_fill_gradient2("Left-Right Score", midpoint = 0.55, mid = "white",limit=c(0.25,0.85)) +
  facet_wrap(~year)
#scale_fill_brewer(palette ="PuOr",type="div","Left-Right Score")
