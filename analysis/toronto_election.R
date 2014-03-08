library(dplyr)
library(ggplot2)
election_results <- read.csv(file="data//election_results.csv")
elections_df <- tbl_df(election_results)
elections <- group_by(elections_df,year,candidate,ward)
votes <- summarise(elections,vote=sum(votes))

qplot(ward,vote,data=votes, facets=~year)
qplot(vote,data=votes, facets=~year)

candidates <- election_results %.%
  group_by(year,candidate) %.%
  summarise(
    vote=sum(votes)
  ) %.%
  filter(vote>5000) %.%
  arrange(year,desc(vote))