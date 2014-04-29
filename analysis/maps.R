library(ggmap)
library(maptools)
library(mapproj)
library(dplyr)
load("data/map_data.RData")
load("data/vote_history.RData")
vote_history <- vote_history %.%
  mutate(ward_area = paste(ward, area, sep = "_")) %.%
  group_by(year, ward_area) %.%
  summarize(total_votes = mean(votes), turnout = mean(turnout), weighted_votes = mean(weighted_votes))
geo <- left_join(geo, vote_history, by = c("ward_area", "year")) %.%
  filter(year != 2014)

# Positions ---------------------------------------------------------------

toronto_map +
  geom_polygon(aes(x=long, y=lat, group=group, fill=cut_interval(weighted_votes, length=0.15)), alpha = 5/6, data=geo) + 
  scale_fill_brewer("Position", type="div") +   
  #scale_fill_gradient2("Left-Right Score", midpoint = median(data$weighted_votes), mid = "white",limit=c(0.25,0.85)) +
  facet_wrap(~year)

# Turnout -----------------------------------------------------------------

toronto_map +
  geom_polygon(aes(x=long, y=lat, group=group, fill=cut_interval(turnout,length = 0.25)), alpha = 5/6, data=geo) + 
  scale_fill_brewer("Turnout") + 
  facet_wrap(~year)
