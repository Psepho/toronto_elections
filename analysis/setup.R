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
income <- as.data.frame(tbl(elections_db,"family_income"))
# Positions
positions <- as.data.frame(tbl(elections_db,"positions"))
positions_by_geo <- as.data.frame(inner_join(votes,positions, by=c("candidate","year")))
positions_by_geo <- positions_by_geo %.%
  group_by(year,ward,area) %.%
  mutate(weighted_votes = votes*score/100)
positions_by_area <- positions_by_geo %.%
  select(year,ward,area,votes,weighted_votes) %.%
  group_by(year,ward,area) %.%
  summarize(weighted_votes = sum(weighted_votes),votes=sum(votes))
positions_by_area$weighted_votes <- positions_by_area$weighted_votes/positions_by_area$votes
positions_by_area$area <- as.integer(positions_by_area$area)
positions_by_area <- positions_by_area %.%
  filter(votes>0)
