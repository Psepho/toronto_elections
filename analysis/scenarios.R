
# Setup -------------------------------------------------------------------

load("data/map_data.RData")
load("data/vote_history.RData")
load("data/candidate_positions.RData")
library(reshape2)
library(ggmap)
library(maptools)
library(mapproj)
library(dplyr)
areas_for_2014 <- geo %.% # Only run scenarios for areas active in 2014
  filter(year == "2014") %.%
  group_by(ward, area) %.%
  summarize(count = n())
areas_for_2014 <- areas_for_2014[,1:2]
candidates_for_2014 <- candidate_positions %.%
  filter(year == 2014) %.%
  select(candidate, left_right_score)
candidates_for_2014 <- split(candidates_for_2014$left_right_score/100, candidates_for_2014$candidate) # Convert to list
geo_2014 <- filter(geo, year == 2014)
position_history <-  vote_history %.% # History of left-right scores by ward, area
  group_by(ward, area) %.%
  summarize(area_position = mean(weighted_votes), area_votes = sum(votes), year = 2014)

areas_for_2014 <- left_join(areas_for_2014, position_history, by = c("ward", "area"))
rm(geo, vote_history, candidate_positions)

# Scenario function -------------------------------------------------------

election_scenario <- function(preference_sensitivity,voteability) { # Takes a preference parameter (preference_sensitivity) and list of "voteability" values for each candidate
  # Returns the votes for each ward_area, by candidate
  voteability <- voteability[order(names(voteability))]
  scenario_output <- matrix(ncol=length(names(voteability)),nrow=dim(areas_for_2014)[1]) # Create a matrix to hold scenario results
  for (candidate in 1:length(names(voteability))) { # Each candidate receives support based on their deviation from the normal distribution of position score for the ward_area
    scenario_output[,candidate] <- voteability[[candidate]]*(dnorm(candidates_for_2014[names(voteability)[candidate]][[1]],areas_for_2014$area_position,preference_sensitivity)/dnorm(areas_for_2014$area_position,areas_for_2014$area_position,preference_sensitivity))
  }
  scenario_output <- data.frame(scenario_output)
  names(scenario_output)<-names(voteability)
  scenario_output$ward <- areas_for_2014$ward
  scenario_output$area <- areas_for_2014$area
  scenario_output$votes <- areas_for_2014$area_votes
  scenario_output_adj <- scenario_output # Now each candidate receives votes in proportion to their relative share of support in each ward_area
  for (i in 1:dim(scenario_output)[1]) {
    for (j in 1:length(names(voteability))) {
      scenario_output_adj[i,j] <- scenario_output[i,length(names(voteability))+3]*scenario_output[i,j]/sum(scenario_output[i,1:length(names(voteability))])
    }
  }
  melt(scenario_output_adj[,-length(names(scenario_output_adj))],id = c(length(names(voteability))+1, length(names(voteability))+2), value.name = "votes", variable.name = "candidate")
}
scenario_summary <- function(output) { # Summarize the total votes and percent of votes for each candidate
  scenario_summary <- output %.%
    group_by(candidate) %.%
    summarize(votes=sum(votes))
  scenario_summary$percent <- scenario_summary$votes/sum(scenario_summary$votes)
  format(scenario_summary, digits=2)
}
scenario_map <- function(output) { # Plot the results on a map by ward area
  output <- droplevels(output)
  data <- as.data.frame(inner_join(geo_2014,output, by=c("ward", "area")))
  candidate_labels <- sapply(strsplit(levels(data$candidate)," "), "[", 1)
  candidate_labels <- paste(toupper(substring(candidate_labels, 1, 1)), substring(candidate_labels, 2), sep = "", collapse = " ")
  levels(data$candidate) <- unlist(strsplit(candidate_labels, split=" "))
  region_summary <- data %.%
    group_by(candidate,region) %.%
    summarize(votes=sum(votes))
  region_summary <- melt(region_summary)
  print(prop.table(tapply(region_summary$value, region_summary[1:2], sum),2))
  toronto_map +
    geom_polygon(aes(x=long, y=lat, group=group, fill=cut_interval(votes, n = 9)), alpha = 5/6, data = data) +
    scale_fill_brewer("Votes") + 
    facet_wrap(~candidate)
}

# Scenario 1 --------------------------------------------------------------

preference_sensitivity <- 0.175
#voteability=list("tory john"=0.239, "chow olivia"=0.443, "ford rob"=0.935, "stintz karen"=0.079,"soknacki david"=0.044,"left rof"=0.005,"center rof"=0.043,"right rof"=0.019, "no one"=0.001) # calibrated to match polls
voteability=list("tory john"=0.239, "chow olivia"=0.443, "ford rob"=0.935, "stintz karen"=0.079,"soknacki david"=0.044)

output <- election_scenario(preference_sensitivity,voteability)
scenario_summary(output)
output_major <- output %.%
  filter(candidate %in% names(voteability)[1:3])
scenario_map(output_major)

# Scenario 4 --------------------------------------------------------------

# Scenario 4: Move left slightly
# candidates_for_2014$`tory john` <- candidates_for_2014$tory-0.05
# candidates_for_2014$`chow olivia` <- candidates_for_2014$chow-0.05
# voteability$`tory john` <- 0.284
