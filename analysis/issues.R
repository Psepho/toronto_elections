# Basic setup -------------------------------------------------------------
library(ggmap)
library(maptools)
library(mapproj)
library(dplyr)
library(reshape2)
load("data/map_data.RData")
load("data/vote_history.RData")
load("data/candidate_positions.RData")

# Create data frame -------------------------------------------------------

issues_df <- vote_history  %.%
  group_by(ward, area, candidate, add = FALSE) %.%
  summarize(votes=sum(votes), airport_expansion=mean(`Airport expansion`), finance_budget=mean(`Finance & Budget`), transit=mean(Transit), transportation=mean(Transportation), waste_management=mean(`Waste management`))
issues_df$ward <- as.factor(issues_df$ward)
issues_df <- group_by(issues_df[,c(1,4:9)], ward)

areas_per_ward <- geo %.%
  filter(year == "2014") %.%
  group_by(ward, area) %.%
  summarize(count = n()) %.%
  group_by(ward, add = FALSE) %.% # Seems roundabout, but need to summarize with area first
  summarize(count = n())

geo_2014 <- filter(geo, year == 2014)

# Issue models for each ward ----------------------------------------------
issues_formula <- votes ~ transportation + transit + finance_budget + waste_management + airport_expansion
library(plyr)
library(dplyr)
issues_model <- function(df){
  lm(issues_formula, data = df, na.action = na.omit)
}
issues_by_ward <- dlply(issues_df, .(ward), issues_model)
coefs_by_ward <- ldply(issues_by_ward, function(x) coef(x))
coefs_melt <- melt(coefs_by_ward[,-2])
max_coefficients_by_ward <- ddply(coefs_melt, "ward", function(x) x[which.max(abs(x$value)),])
max_coefficients_by_ward$ward <- as.integer(max_coefficients_by_ward$ward)

# Predict votes from position scores --------------------------------------
names(candidate_positions)[3:7] <- names(issues_df)[3:7]
predicted_votes <- function(pred_values) {
  output <- ldply(issues_by_ward, function(x) predict(x, pred_values, interval = "confidence"))
  names(output)[2] <- "votes"
  output
}

# Collect votes by candidate ----------------------------------------------
library(plyr)
library(dplyr)
results <- data.frame(candidate = NA, ward = 0, votes = NA, lwr = NA, upr = NA)
candidates <- c(1,3,13,14,16)
for(candidate in candidates) {
  pred_values <- candidate_positions[candidate,] # Position scores
  results <- rbind(results, data.frame(candidate=candidate_positions[candidate,1],predicted_votes(pred_values)))
}
detach("package:plyr", unload=TRUE)
results <- results[-1,]
results$ward <- as.integer(results$ward)
results <- left_join(results, areas_per_ward, by = "ward")
results$adj_votes <- results$votes * results$count
prop.table(tapply(results$adj_votes, results[1:2], sum, na.rm=TRUE),2)
prop.table(tapply(results$adj_votes, results[1], sum, na.rm=TRUE))
max_coefficients_by_ward

# Top issue by ward -------------------------------------------------------

library(dplyr)
geo_2014 <- left_join(geo_2014, max_coefficients_by_ward[,1:2], by = "ward")
names(geo_2014)[13] <- "top_issue"
toronto_map +
  geom_polygon(aes(x=long, y=lat, group=group, fill=top_issue), alpha = 5/6, data=geo_2014) + 
  scale_fill_brewer("Top issue", type="qual")
ggsave(file = "fig/top_issue_map.png")

# Candidate support by ward -----------------------------------------------

issue_map <- function(output) { # Plot the results on a map by ward
  output <- droplevels(output)
  data <- as.data.frame(inner_join(geo_2014,output, by=c("ward")))
  candidate_labels <- sapply(strsplit(levels(data$candidate)," "), "[", 1)
  candidate_labels <- paste(toupper(substring(candidate_labels, 1, 1)), substring(candidate_labels, 2), sep = "", collapse = " ")
  levels(data$candidate) <- unlist(strsplit(candidate_labels, split=" "))
  toronto_map +
    geom_polygon(aes(x=long, y=lat, group=group, fill=cut_interval(adj_votes, n = 8)), alpha = 5/6, data=data) +
    scale_fill_brewer("Votes", labels=c("Low", rep("",6), "High")) + 
    facet_wrap(~candidate)
}
output_major <- results %.%
  filter(candidate %in% c("tory john", "chow olivia", "ford rob")) %.%
  mutate(candidate=as.factor(candidate))
issue_map(output_major)
ggsave(file = "fig/candidate_support_by_issue_map.png")

# Issue importance by ward ------------------------------------------------

names(coefs_melt)[2:3] <- c("coefficient", "beta")
data <- as.data.frame(inner_join(geo_2014,coefs_melt, by=c("ward")))
toronto_map +
  geom_polygon(aes(x=long, y=lat, group=group, fill=cut_interval(beta,7)), alpha = 5/6, data=data) +
  scale_fill_brewer("Position", type="div", labels=c("Left", rep("",5), "Right")) + 
  facet_wrap(~coefficient)
ggsave(file = "fig/issue_importance_map.png")

