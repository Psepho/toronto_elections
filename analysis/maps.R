source("analysis//setup.R")
source("analysis//setup_maps.R")
#---------
# Point maps
#---------
point_map <- function(data,variable){
  toronto_map + geom_point(aes_string(x="long",y="lat",colour=variable),data=geo) + facet_wrap(~year)
}
point_map(turnout_geo,"turnout")
point_map(turnout_geo,"total_votes")
point_map(positions_geo,"weighted_votes")
#---------
# Map the results
#---------
# Positions
toronto_map +
  geom_polygon(aes(x=long, y=lat, group=group, fill=cut_interval(weighted_votes, length=0.15)), alpha = 5/6, data=geo) + 
  scale_fill_brewer("Position", type="div") +   
  #scale_fill_gradient2("Left-Right Score", midpoint = median(data$weighted_votes), mid = "white",limit=c(0.25,0.85)) +
  facet_wrap(~year)
# Turnout
toronto_map +
  geom_polygon(aes(x=long, y=lat, group=group, fill=cut_interval(turnout,length = 0.25)), alpha = 5/6, data=geo) + 
  scale_fill_brewer("Turnout") + 
  facet_wrap(~year)
# Change in position
toronto_map +
  geom_polygon(aes(x=long, y=lat, group=group, fill=cut_interval(turnout,length = 0.25)), alpha = 5/6, data=subset(data,year!=2003)) + # Exclude 2003
  scale_fill_brewer("Change in position") + 
  facet_wrap(~year)


positions_in_active_areas <- tbl_df(merge(active_areas,data))
toronto_map +
  geom_polygon(aes(x=long, y=lat, group=group, fill=weighted_votes_change), alpha = 5/6, data=positions_in_active_areas) + 
  scale_fill_gradient("Change in position", low="white", high="red", space="Lab")+ 
  facet_wrap(~year)

toronto_map +
  geom_polygon(aes(x=long, y=lat, group=group, fill=cut_interval(total_votes,length=200)), alpha = 5/6, data=geo) +
  scale_fill_brewer("Votes") + 
  facet_wrap(~candidate)


#scale_fill_brewer(palette ="PuOr",type="div","Left-Right Score")
