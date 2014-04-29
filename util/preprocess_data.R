# ----------
# Pulls in various data sources
# Exports vote_history.RData
# ----------

# Packages ----------------------------------------------------------------

library(dplyr)
library(RSQLite)
library(RSQLite.extfuns)
library(reshape2)
library(stringr)

# Data setup --------------------------------------------------------------

# tmp directory for downloads and unzips
if(file.exists("tmp")) {
  # Nothing to do
}  else {
  dir.create("tmp")
}
# Load the sqlite db and extract the tables
elections_db <- src_sqlite("data/elections_db.sqlite3")
votes <- as.data.frame(collect(tbl(elections_db,"votes")))
votes$year <- as.factor(votes$year)
turnout <- as.data.frame(collect(tbl(elections_db,"turnout")))
turnout$year <- as.integer(turnout$year)
locations <- as.data.frame(tbl(elections_db,"locations"))
# Assuming (for now) that 2003 locations match 2006
locations <- rbind(locations,locations %.%
                     filter(year==2006) %.%
                     mutate(year=as.integer(2003)))
#ward_regions <- read.csv("data/ward_regions.csv")
#locations <- inner_join(locations,ward_regions, by=c("ward"))
turnout$year <- as.factor(turnout$year)
locations$year <- as.factor(locations$year)
locations <- select(locations, CTUID:area)

# Turnout -----------------------------------------------------------------

turnout_geo <- as.data.frame(inner_join(turnout,locations, by=c("ward", "area","year")))
turnout_geo <- tbl_df(turnout_geo) %.%
  select(year,ward,area,total_eligible,total_votes) %.%
  mutate(turnout=total_votes/total_eligible)

# Positions, votes, and turnout by geo -------------------------------------

load("data/candidate_positions.RData")
votes_by_issues <- as.data.frame(inner_join(votes,candidate_positions, by=c("candidate", "year")))
votes_by_issues <- as.data.frame(inner_join(votes_by_issues,turnout, by=c("ward", "area", "year"))) %.%
  mutate(turnout=total_votes/total_eligible, candidate = as.factor(candidate))
area_left_right_history <- votes_by_issues %.%
  group_by(year, ward, area) %.%
  mutate(weighted_votes = votes*left_right_score/100) %.%
  summarize(weighted_votes = sum(weighted_votes)/sum(votes))
votes_by_issues <- as.data.frame(inner_join(votes_by_issues,area_left_right_history, by=c("ward", "area", "year")))
vote_history <- as.data.frame(inner_join(votes_by_issues,locations, by=c("ward", "area", "year")))
save(vote_history, file = "data/vote_history.RData")
rm(candidate_positions, area_left_right_history, votes_by_issues, turnout_geo, turnout, votes, locations, ward_regions, elections_db)
