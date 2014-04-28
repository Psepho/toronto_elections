library(ggmap)
library(maptools)
library(stringr)

# Download shapefiles -----------------------------------------------------

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

# Standard base map -------------------------------------------------------

toronto_map <- qmap("queens park,toronto",zoom=11, maptype = 'terrain')

# Shapefiles --------------------------------------------------------------

# Extract the data object for each year and then combine into one data frame
geo_2010 <- fortify(shapefile_2010,region="AREA_NAME")
geo_2010$year <- as.integer(2010)
geo_2006 <- fortify(shapefile_2006,region="AREA_NAME")
geo_2006$year <- as.integer(2006)
geo_2003 <- fortify(shapefile_2006,region="AREA_NAME") # Assuming 2006 locations for 2003 geo
geo_2003$year <- as.integer(2003)
geo <- rbind(geo_2010,geo_2006,geo_2003)
geo$ward_area <- paste(as.integer(str_sub(geo$id,1,2)),as.integer(str_sub(geo$id,-3,-1)),sep="_")
geo$ward <- as.integer(str_sub(geo$id,1,2))
# For 2014, assume same geography as 2010
geo_2014 <- geo %.%
  filter(year==2010) %.%
  mutate(year=2014)
geo <- rbind(geo, geo_2014)
geo$year <- as.factor(geo$year)
rm(geo_2014, geo_2003, geo_2006, geo_2010)
ward_regions <- read.csv("data/ward_regions.csv")
geo <- inner_join(geo,ward_regions, by=c("ward"))
rm(ward_regions)

# Save objects ------------------------------------------------------------

save(geo, toronto_map, file = "data/map_data.RData")
