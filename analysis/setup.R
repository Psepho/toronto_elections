#-----------
# Data setup
#-----------

#Packages
library(dplyr)
library(RSQLite)
library(RSQLite.extfuns)
library(ggplot2)
library(reshape2)
library(stringr)

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
income <- as.data.frame(tbl(elections_db,"family_income"))
# Positions
positions <- as.data.frame(tbl(elections_db,"positions"))
positions <- as.data.frame(inner_join(votes,positions, by=c("candidate","year")))
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



# positions_votes <- as.data.frame(inner_join(positions_geo,positions[,c(2:3,5:6,11)], by=c("ward","area","year")))
# positions_votes$score <- positions_votes$score/100
# positions_votes$intensity <- (1-(positions_votes$weighted_votes-positions_votes$score)^2)
# 
# head(positions_votes)
# qplot(weighted_votes, intensity, data = positions_votes, geom="point") + facet_grid(candidate~year) + xlab("Area position") + ylab("Voting intensity")
# qplot(weighted_votes, total_votes, data = positions_votes, geom="point") + facet_grid(~year) + xlab("Area position") + ylab("Total votes")
# qplot(intensity, total_votes, data = positions_votes, geom="point") + facet_grid(~year) + xlab("Intensity") + ylab("Total votes")
#positions_geo$intensity <- positions_geo$total_votes*(1-(positions_geo$weighted_votes-0.6)^2)


