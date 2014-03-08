library(dplyr)
library(ggplot2)
election_results <- read.csv(file="data//election_results.csv")
elections_df <- tbl_df(election_results)
elections <- group_by(elections_df,year,candidate,ward)
votes_by_year_ward <- elections_df %.%
  group_by(year,ward,area) %.%
  summarise(
    vote=sum(votes)
  )

candidates <- elections_df %.%
  group_by(year,candidate) %.%
  summarise(
    vote=sum(votes)
  ) %.%
  filter(vote>5000) %.%
  arrange(year,desc(vote))

qplot(ward,vote,data=votes, facets=~year)
qplot(vote,data=votes, facets=~year)

turnout <- read.csv(file="data//turnout.csv")
turnout_df <- tbl_df(turnout[,-1])

votes_eligible_by_ward_area_year <- merge(votes_by_year_ward,turnout_df)
detail_turnout <- mutate(votes_eligible_by_ward_area_year, 
                         turnout = vote/total_eligible,
                         spoiled = total_votes - vote) %.%
  filter(total_eligible > 0)