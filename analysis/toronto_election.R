#-----------
# Data setup
#-----------

#Packages
library(dplyr)
library(RSQLite)
library(RSQLite.extfuns)
library(ggplot2)

# Load the sqlite db and extract the tables
elections_db <- src_sqlite("data/elections_db.sqlite3")
votes <- as.data.frame(tbl(elections_db,"votes"))
turnout <- as.data.frame(tbl(elections_db,"turnout"))
turnout$year <- as.integer(turnout$year)
locations <- as.data.frame(tbl(elections_db,"locations"))
income <- as.data.frame(tbl(elections_db,"family_income"))

#-----------
# Summaries
#-----------

# Income
median_income <- income %.%
  filter(income_type=="Median 2005 family income $",family_structure=="Total - All economic families") %.%
  select(SGC,value)

# Votes and candidates
elections <- group_by(votes,year,candidate,ward)
votes_by_year_ward <- votes %.%
  group_by(year,ward) %.%
  summarise(
    vote=sum(votes)
  )
votes_by_year <- votes %.%
  group_by(year) %.%
  summarise(
    vote=sum(votes)
  )
areas_by_year <- votes %.%
  mutate(location=paste(ward,area,sep="-")) %.%
  select(year,location) %.%
  group_by(year) %.%
  summarise(
    count=n_distinct(location)
  )
candidates_by_year <- votes %.%
  select(year,candidate) %.%
  group_by(year) %.%
  summarise(
    count=n_distinct(candidate)
  )
candidates <- votes %.%
  group_by(year,candidate) %.%
  summarise(
    vote=sum(votes)
  ) %.%
  filter(vote>5000) %.%
  arrange(year,desc(vote))
qplot(ward,vote,data=votes_by_year_ward, facets=~year)
qplot(vote,data=votes_by_year_ward, facets=~year)

# Turnout
turnout_by_year <- turnout %.%
  group_by(year) %.%
  summarise(eligible=sum(total_eligible,na.rm=TRUE),
            votes=sum(total_votes)) %.%
  mutate(turnout=votes/eligible)
votes_eligible_by_ward_area_year <- tbl_df(merge(votes_by_year_ward,turnout))
detail_turnout <- mutate(votes_eligible_by_ward_area_year, 
                         turnout = vote/total_eligible,
                         spoiled = total_votes - vote) %.%
  filter(total_eligible > 0)
turnout_year <- tbl_df(merge(votes_by_year_ward,turnout)) %.%
  group_by(year) %.%
  summarise(eligible=sum(total_eligible),
            votes=sum(vote)) %.%
  mutate(turnout=votes/eligible)
turnout_by_ward_year <- merge(votes_by_year_ward,turnout) %.%
  group_by(year,ward) %.%
  summarise(eligible=sum(total_eligible),
            votes=sum(vote)) %.%
  mutate(turnout=votes/eligible)
qplot(turnout,data=turnout_by_ward_year, facets=~year)

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
toronto_map <- qmap("queens park,toronto",zoom=11)
toronto_map + geom_point(aes(x=long,y=lat,size=turnout),data=turnout_2010)

# Also plot total votes
toronto_map + geom_point(aes(x=long,y=lat,size=total_votes),data=turnout_2010)

# Try the shapefile
unzip("data//voting_subdivision_2010_wgs84.zip")
shapefile <- readShapeSpatial("VOTING_SUBDIVISION_2010_WGS84.shp", proj4string=CRS("+proj=longlat +datum=WGS84"))
data <- fortify(shapefile)
qmap("queens park,toronto", zoom=11, maptype="hybrid") +
  geom_polygon(aes(x=long, y=lat, group=group), data=data, colour="white", fill="black", alpha=.4, size=.3) + 
  geom_point(aes(x=long,y=lat,size=turnout),data=turnout_2010)
