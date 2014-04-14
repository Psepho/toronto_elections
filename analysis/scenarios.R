source("analysis//setup.R")
# Data required:
#areas_for_2014 <- load("data/areas_for_2014.RData")
#positions_2014 <- load("data/positions_2014.RData")
#locations_2014 <- load("data/data_2014.RData")
locations_2014 <- data_2014 
election_scenario <- function(preference_sensitivity,voteability) { # Takes a preference parameter (preference_sensitivity) and list of "voteability" values for each candidate
  # Returns the votes for each ward_area, by candidate
  voteability <- voteability[order(names(voteability))]
  scenario_output <- matrix(ncol=length(names(voteability)),nrow=length(areas_for_2014$ward_area)) # Create a matrix to hold scenario results
  for (candidate in 1:length(names(voteability))) { # Each candidate receives support based on their deviation from the normal distribution of position score for the ward_area
    scenario_output[,candidate] <- voteability[[candidate]]*(dnorm(positions_2014[names(voteability)[candidate]][[1]],areas_for_2014$position,preference_sensitivity)/dnorm(areas_for_2014$position,areas_for_2014$position,preference_sensitivity))
  }
  scenario_output <- data.frame(scenario_output)
  names(scenario_output)<-names(voteability)
  scenario_output$ward_area <- areas_for_2014$ward_area
  scenario_output$votes <- areas_for_2014$votes
  scenario_output_adj <- scenario_output # Now each candidate receives votes in proportion to their relative share of support in each ward_area
  for (i in 1:dim(scenario_output)[1]) {
    for (j in 1:length(names(voteability))) {
      scenario_output_adj[i,j] <- scenario_output[i,length(names(voteability))+2]*scenario_output[i,j]/sum(scenario_output[i,1:length(names(voteability))])
    }
  }
  melt(scenario_output_adj[,-length(names(scenario_output_adj))],id = length(names(voteability))+1, value.name = "votes", variable.name = "candidate")
}
scenario_summary <- function(output) { # Summarize the total votes and percent of votes for each candidate
  scenario_summary <- output %.%
    group_by(candidate) %.%
    summarize(votes=sum(votes))
  scenario_summary$percent <- scenario_summary$votes/sum(scenario_summary$votes)
  format(scenario_summary, digits=2)
}
scenario_map <- function(output) {
  data <- as.data.frame(inner_join(locations_2014,output, by=c("ward_area")))
  candidate_labels <- sapply(strsplit(levels(data$candidate)," "), "[", 1)
  candidate_labels <- paste(toupper(substring(candidate_labels, 1, 1)), substring(candidate_labels, 2), sep = "", collapse = " ")
  levels(data$candidate) <- unlist(strsplit(candidate_labels, split=" "))
  toronto_map +
    geom_polygon(aes(x=long, y=lat, group=group, fill=cut_interval(votes,length=200)), alpha = 5/6, data=data) +
    scale_fill_brewer("Votes") + 
    facet_wrap(~candidate)
}

preference_sensitivity <- 0.175
voteability=list("tory john"=0.243, "chow olivia"=0.4938, "ford rob"=1, "stintz karen"=0.0609,"soknacki david"=0.0219,"left rof"=0.15,"center rof"=0.0297,"right rof"=0.1) # calibrated to match polls

output <- election_scenario(preference_sensitivity,voteability)
scenario_summary(output)
scenario_map(output)
