source("analysis//setup.R")
ward_regions <- read.csv("data/ward_regions.csv")
# Data required:
#areas_for_2014 <- load("data/areas_for_2014.RData")
#positions_2014 <- load("data/positions_2014.RData")
#geo_2014 <- load("data/geo_2014.RData")

positions_2014 <- c(positions_2014, 0.5)
names(positions_2014)[9] <- "no one"
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
  output <- droplevels(output)
  data <- as.data.frame(inner_join(geo_2014,output, by=c("ward_area")))
  candidate_labels <- sapply(strsplit(levels(data$candidate)," "), "[", 1)
  candidate_labels <- paste(toupper(substring(candidate_labels, 1, 1)), substring(candidate_labels, 2), sep = "", collapse = " ")
  levels(data$candidate) <- unlist(strsplit(candidate_labels, split=" "))
  region_summary <- data %.%
    group_by(candidate,region) %.%
    summarize(votes=sum(votes))
  region_summary$percent <- region_summary$votes/sum(region_summary$votes)
  print(region_summary)
  toronto_map +
    geom_polygon(aes(x=long, y=lat, group=group, fill=cut_interval(votes,length=150)), alpha = 5/6, data=data) +
    scale_fill_brewer("Votes") + 
    facet_wrap(~candidate)
}
scenario_region <- function(output) {
  output$ward <- as.integer(lapply(strsplit(as.character(output$ward_area), "_"), "[", 1))
  output <- inner_join(output,ward_regions, by=c("ward"))
  region_summary <- output %.%
    group_by(candidate,region) %.%
    summarize(votes=sum(votes))
  region_summary <- melt(region_summary)
  #format(dcast(region_summary, candidate ~ region, margins = TRUE, fun.aggregate = sum),digits=3)
  prop.table(tapply(region_summary$value, region_summary[1:2], sum),2)
}
preference_sensitivity <- 0.175
voteability=list("tory john"=0.248, "chow olivia"=0.435, "ford rob"=0.9, "stintz karen"=0.068,"soknacki david"=0.041,"left rof"=0,"center rof"=0.048,"right rof"=0.019, "no one"=0.001) # calibrated to match polls

# Scenario 4: Move left slightly
# positions_2014$`tory john` <- positions_2014$tory-0.05
# positions_2014$`chow olivia` <- positions_2014$chow-0.05
# voteability$`tory john` <- 0.284

output <- election_scenario(preference_sensitivity,voteability)
scenario_summary(output)
scenario_region(output)

output_major <- output %.%
  filter(candidate %in% names(voteability)[1:3])

scenario_map(output_major)
