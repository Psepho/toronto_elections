source("analysis//setup.R")
positions_2014 <- candidate_positions %.%
  filter(year==2014) %.%
  select(candidate, score) %.%
  mutate(score=score/100)
positions_2014 <- split(positions_2014$score, positions_2014$candidate)
positions_2014 <- positions_2014[order(names(positions_2014))]
# Average positions for each ward_area
positions_average_by_ward_area <- positions_geo %.%
  group_by(ward,area,ward_area) %.%
  summarise(position=mean(weighted_votes),votes=mean(total_votes)) %.%
  arrange(ward,area)

election_scenario <- function(preference_sensitivity,voteability) { # Takes a preference parameter (preference_sensitivity) and list of "voteability" values for each candidate
  # Returns the total votes and percent of votes, by candidate
  scenario_output <- matrix(ncol=length(names(positions_2014)),nrow=length(positions_average_by_ward_area$ward_area)) # Create a matrix to hold scenario results
  for (candidate in 1:length(names(positions_2014))) { # Each candidate receives support based on their deviation from the normal distribution of position score for the ward_area
    scenario_output[,candidate] <- voteability[[candidate]]*(dnorm(positions_2014[[candidate]],positions_average_by_ward_area$position,preference_sensitivity)/dnorm(positions_average_by_ward_area$position,positions_average_by_ward_area$position,preference_sensitivity))
  }
  scenario_output <- data.frame(scenario_output)
  names(scenario_output)<-names(positions_2014)
  scenario_output$ward_area <- positions_average_by_ward_area$ward_area
  scenario_output$votes <- positions_average_by_ward_area$votes
  scenario_output_adj <- scenario_output # Now each candidate receives votes in proportion to their relative share of support in each ward_area
  for (i in 1:dim(scenario_output)[1]) {
    for (j in 1:8) {
      scenario_output_adj[i,j] <- scenario_output[i,length(positions_2014)+2]*scenario_output[i,j]/sum(scenario_output[i,1:length(positions_2014)])
    }
  }
  # Now summarize the total votes and percent of votes for each candidate
  scenario_output <- melt(scenario_output_adj[,-length(names(scenario_output_adj))],id = length(positions_2014)+1, value.name = "votes", variable.name = "candidate") %.%
    group_by(candidate) %.%
    summarize(votes=sum(votes))
  scenario_output$percent <- scenario_output$votes/sum(scenario_output$votes)
  format(scenario_output, digits=2)
}

preference_sensitivity <- 0.175
voteability=list("tory john"=0.243, "chow olivia"=0.4938, "ford rob"=1, "stintz karen"=0.0609,"soknacki david"=0.0219,"left rof"=0.15,"center rof"=0.0297,"right rof"=0.1) # calibrated to match polls
voteability <- voteability[order(names(voteability))]
election_scenario(preference_sensitivity,voteability)
