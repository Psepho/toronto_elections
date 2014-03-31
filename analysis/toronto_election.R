source("analysis//setup.R")

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

# Positions
positions_by_ward <- as.data.frame(inner_join(votes,positions, by=c("candidate","year"))) %.%
  select(year,ward,votes) %.%
  group_by(year,ward) %.%
  summarize(votes=sum(votes))



library(lme4)
turnout_motivated_by_position <- lmer(turnout ~ weighted_votes | year/ward, data = positions_geo)
turnout_intercept <- lmer(turnout ~ 1 | year/ward, data = positions_geo)
summary(turnout_motivated_by_position) # check for errors
anova(turnout_motivated_by_position,turnout_intercept)

plot(turnout_motivated_by_position)
plot(turnout_motivated_by_position, form = resid(., type = "response") ~ fitted(.) | year, abline = 0)

library(nlme)
turnout_intercept <- lme(turnout ~ year, random = ~ 1|ward, data=positions_geo, method="ML")
turnout_motivated_by_position <- update(turnout_intercept, ~ weighted_votes + .)
anova(turnout_intercept,turnout_motivated_by_position)
summary(turnout_motivated_by_position)


changed_positions <- positions_geo %.%
  filter(year==2010,weighted_votes_change!=0,turnout>0.5)


# Calculate difference in score from 60 (absolute) scaled by eligible votes
