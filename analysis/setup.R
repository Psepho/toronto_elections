#-----------
# Packages
#-----------
library(dplyr)
library(RSQLite)
library(RSQLite.extfuns)
library(ggplot2)
library(reshape2)
library(stringr)
library(ggplot2)
library(ggmap)
library(maptools)
#-----------
# Data setup
#-----------
# tmp directory for downloads and unzips
if(file.exists("tmp")) {
  # Nothing to do
}  else {
  dir.create("tmp")
}
# Load the sqlite db and extract the tables
elections_db <- src_sqlite("data/elections_db.sqlite3")
votes <- as.data.frame(collect(tbl(elections_db,"votes")))
turnout <- as.data.frame(collect(tbl(elections_db,"turnout")))
turnout$year <- as.integer(turnout$year)
locations <- as.data.frame(tbl(elections_db,"locations"))
# Assuming (for now) that 2003 locations match 2006
locations <- rbind(locations,locations %.%
                     filter(year==2006) %.%
                     mutate(year=as.integer(2003)))
ward_regions <- read.csv("data/ward_regions.csv")
locations <- inner_join(locations,ward_regions, by=c("ward"))
income <- as.data.frame(tbl(elections_db,"family_income"))
# Positions
candidate_positions <- as.data.frame(tbl(elections_db,"positions"))
# Positions for just candidates in the upcoming election
positions_2014 <- candidate_positions %.%
  filter(year==2014) %.%
  select(candidate, score) %.%
  mutate(score=score/100)
positions_2014 <- split(positions_2014$score, positions_2014$candidate)
positions_2014 <- positions_2014[order(names(positions_2014))]
# Positions by area
positions <- as.data.frame(inner_join(votes,candidate_positions, by=c("candidate","year")))
positions <- positions %.%
  group_by(year,ward,area) %.%
  mutate(weighted_votes = votes*score/100)
positions_geo <- positions %.%
  select(year,ward,area,votes,weighted_votes) %.%
  group_by(year,ward,area) %.%
  summarize(weighted_votes = sum(weighted_votes),votes=sum(votes))
positions_geo$weighted_votes <- positions_geo$weighted_votes/positions_geo$votes
positions_geo$area <- as.integer(positions_geo$area)
positions_geo <- positions_geo %.%
  filter(votes>0)
positions_geo <- as.data.frame(inner_join(positions_geo,locations, by=c("ward", "area","year")))
positions_geo <- as.data.frame(inner_join(positions_geo,turnout, by=c("ward", "area","year")))
positions_geo <- tbl_df(positions_geo) %.%
  mutate(turnout=total_votes/total_eligible,ward_area=paste(ward,area,sep="_")) %.%
  select(year,ward,area,long,lat,weighted_votes,turnout,total_votes,ward_area)
positions_geo <- positions_geo %.%
  arrange(year) %.%
  group_by(ward_area)%.%
  mutate(weighted_votes_change = c(0,diff(weighted_votes)))
# Average positions for each ward_area
positions_average_by_ward_area <- positions_geo %.%
  group_by(ward,area,ward_area) %.%
  summarise(position=mean(weighted_votes),votes=mean(total_votes)) %.%
  arrange(ward,area)
# Areas with votes in 2010
active_areas_2010 <- positions_geo %.%
  filter(year==2010,total_votes>0) %.%
  select(ward,area,ward_area)
active_areas_2010 <- active_areas_2010[,1:2]
areas_for_2014 <- as.data.frame(inner_join(active_areas_2010,positions_average_by_ward_area, by=c("ward", "area")))
# Scenarios
scenario <- as.data.frame(tbl(elections_db,"scenario"))
names(scenario)[3:4] <- c("candidate","total_votes") 
scenario$candidate <- as.factor(scenario$candidate)
# Turnout
turnout_geo <- as.data.frame(inner_join(turnout,locations, by=c("ward", "area","year")))
turnout_geo <- tbl_df(turnout_geo) %.%
  select(year,ward,area,long,lat,total_eligible,total_votes) %.%
  mutate(turnout=total_votes/total_eligible,ward_area=paste(ward,area,sep="_"))
# Active areas
active_areas <- turnout_geo %.%
  select(year,ward,area,turnout)
active_areas <- melt(active_areas,id=1:3)
active_areas <- dcast(active_areas, ward + area ~ year)
turnout_threshold <- 0.6
active_areas <- active_areas %.%
  filter(`2006`>turnout_threshold,`2010`>turnout_threshold) %.%
  select(ward,area)
turnout_in_active_areas <- tbl_df(merge(active_areas,turnout_geo))
positions_in_active_areas <- tbl_df(merge(active_areas,positions_geo))
active_areas <- active_areas %.%
  mutate(ward_area=paste(ward,area,sep="_"))
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
# Shapefiles
#---------
# Extract the data object for each year and then combine into one data frame
geo_2010 <- fortify(shapefile_2010,region="AREA_NAME")
geo_2010$year <- as.integer(2010)
geo_2006 <- fortify(shapefile_2006,region="AREA_NAME")
geo_2006$year <- as.integer(2006)
geo_2003 <- fortify(shapefile_2006,region="AREA_NAME") # Assuming 2006 locations for 2003 geo
geo_2003$year <- as.integer(2003)
geo <- rbind(geo_2010,geo_2006,geo_2003)
rm(geo_2003,geo_2006,geo_2010)
geo$ward_area <- paste(as.integer(str_sub(geo$id,1,2)),as.integer(str_sub(geo$id,-3,-1)),sep="_")
geo$ward <- as.integer(str_sub(geo$id,1,2))
geo <- as.geo.frame(inner_join(geo,positions_geo[,-c(2:5)], by=c("ward_area","year")))
geo <- inner_join(geo,ward_regions, by=c("ward"))

# For 2014, assume same geography as 2010
geo_2014 <- geo %.%
  filter(year==2010) %.%
  mutate(year=2014)
rm(ward_regions)
