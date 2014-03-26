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
shapefile_2010$ward_area <- paste(as.integer(str_sub(shapefile_2010$AREA_LONG,1,2)),as.integer(str_sub(shapefile_2010$AREA_LONG,-3,-1)),sep="_")
if(file.exists("tmp/subdivisions_2006.zip")) {
  # Nothing to do
}  else {
  download.file(shapefile_url_2006,
                destfile = "tmp/subdivisions_2006.zip")
  unzip("tmp/subdivisions_2006.zip", exdir="tmp")
}
shapefile_2006 <- readShapeSpatial("tmp/VOTING_SUBDIVISION_2006_WGS84.shp", proj4string=CRS("+proj=longlat +datum=WGS84"))
shapefile_2006$ward_area <- paste(as.integer(str_sub(shapefile_2006$AREA_LONG,1,2)),as.integer(str_sub(shapefile_2006$AREA_LONG,-3,-1)),sep="_")
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
# Join positions and locations
shapefile@data = data.frame(shapefile@data, positions_2010[match(shapefile$ward_area, positions_2010$ward_area),])
data <- fortify(shapefile)
# Get the data back into the dataframe (this seems like it should be unneccessary)
data <-merge(data,shapefile@data,by.x="id",by.y="row.names")
# Map the results
p <- toronto_map +
  geom_polygon(aes(x=long, y=lat, group=group, fill=weighted_votes), alpha = 5/6, data=data) + 
  scale_fill_gradient2("Left-Right Score", midpoint = 0.55, mid = "white",limit=c(0.25,0.85))
#scale_fill_brewer(palette ="PuOr",type="div","Left-Right Score")
p

# 2006 results
# Join positions and locations
shapefile@data = data.frame(shapefile@data, positions_2006[match(shapefile$ward_area, positions_2006$ward_area),])
data <- fortify(shapefile)
# Get the data back into the dataframe (this seems like it should be unneccessary)
data <-merge(data,shapefile@data,by.x="id",by.y="row.names")
# Map the results
p <- toronto_map +
  geom_polygon(aes(x=long, y=lat, group=group, fill=weighted_votes), alpha = 5/6, data=data) + 
  scale_fill_gradient2("Left-Right Score", midpoint = 0.55, mid = "white",limit=c(0.25,0.85))
  #scale_fill_brewer(palette ="PuOr",type="div","Left-Right Score")
p
