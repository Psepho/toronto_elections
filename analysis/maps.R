source("analysis//setup.R")
#-----------
# Mapping
#-----------
library(ggplot2)
library(ggmap)
library(maptools)

# Start with a turnout map
turnout_geo <- as.data.frame(inner_join(turnout,locations, by=c("ward", "area","year")))
turnout_geo <- tbl_df(turnout_geo) %.%
  select(year,ward,area,long,lat,total_eligible,total_votes)
turnout_2010 <- turnout_geo %.% # Filter to 2010
  filter(year=="2010") %.%
  mutate(turnout=total_votes/total_eligible)
toronto_map <- qmap("queens park,toronto",zoom=11, maptype = 'terrain')
toronto_map + geom_point(aes(x=long,y=lat,size=turnout),colour="blue",data=turnout_2010)

# Also plot total votes
toronto_map + geom_point(aes(x=long,y=lat,size=total_votes),data=turnout_2010)

# Plot positions
positions_geo <- as.data.frame(inner_join(positions_by_area,locations, by=c("ward", "area","year")))
positions_geo <- as.data.frame(inner_join(positions_geo,turnout, by=c("ward", "area","year")))
positions_geo <- tbl_df(positions_geo) %.%
  mutate(turnout=total_votes/total_eligible) %.%
  select(year,ward,area,long,lat,weighted_votes,turnout)
positions_2010 <- positions_geo %.% # Filter to 2010
  filter(year=="2010")
toronto_map <- qmap("queens park,toronto",zoom=11, maptype = 'terrain')
toronto_map + geom_point(aes(x=long,y=lat,colour=weighted_votes),data=positions_geo)+facet_wrap(~year)

# 2010 results
download.file("http://opendata.toronto.ca/gcc/voting_subdivision_2010_wgs84.zip",
              destfile = "tmp/subdivisions_2010.zip")
unzip("tmp/subdivisions_2010.zip", exdir="tmp")
shapefile <- readShapeSpatial("tmp/VOTING_SUBDIVISION_2010_WGS84.shp", proj4string=CRS("+proj=longlat +datum=WGS84"))
shapefile$ward_area <- paste(as.integer(str_sub(shapefile$AREA_LONG,1,2)),as.integer(str_sub(shapefile$AREA_LONG,-3,-1)),sep="_")
positions_2010 <- positions_2010 %.%
  select(ward,area,weighted_votes) %.%
  mutate(ward_area=paste(ward,area,sep="_"))
# Join positions and locations
shapefile@data = data.frame(shapefile@data, positions_2010[match(shapefile$ward_area, positions_2010$ward_area),])
data <- fortify(shapefile)
# Get the data back into the dataframe (this seems like it should be unneccessary)
data <-merge(data,shapefile@data,by.x="id",by.y="row.names")
# Map the results
p <- toronto_map +
  geom_polygon(aes(x=long, y=lat, group=group, fill=cut_interval(weighted_votes,6)), alpha = 5/6, data=data) + 
  scale_fill_brewer(palette ="PuOr",type="div","Left-Right Score")
p

# 2006 results
download.file("http://opendata.toronto.ca/gcc/voting_subdivision_2006_wgs84.zip",
              destfile = "tmp/subdivisions_2006.zip")
unzip("tmp/subdivisions_2006.zip", exdir="tmp")
shapefile <- readShapeSpatial("tmp/VOTING_SUBDIVISION_2006_WGS84.shp", proj4string=CRS("+proj=longlat +datum=WGS84"))
shapefile$ward_area <- paste(as.integer(str_sub(shapefile$AREA_LONG,1,2)),as.integer(str_sub(shapefile$AREA_LONG,-3,-1)),sep="_")
positions_2006 <- positions_geo %.%
  filter(year=="2006") %.%
  select(ward,area,weighted_votes) %.%
  mutate(ward_area=paste(ward,area,sep="_"))
# Join positions and locations
shapefile@data = data.frame(shapefile@data, positions_2006[match(shapefile$ward_area, positions_2010$ward_area),])
data <- fortify(shapefile)
# Get the data back into the dataframe (this seems like it should be unneccessary)
data <-merge(data,shapefile@data,by.x="id",by.y="row.names")
# Map the results
p <- toronto_map +
  geom_polygon(aes(x=long, y=lat, group=group, fill=cut_interval(weighted_votes,6)), alpha = 5/6, data=data) + 
  scale_fill_brewer(palette ="PuOr",type="div","Left-Right Score")
p
