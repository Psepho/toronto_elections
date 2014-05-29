# Basic setup -------------------------------------------------------------

library(dplyr)
library(reshape2)
library(ggplot2)
load("data/vote_history.RData")
load("data/candidate_positions.RData")

names(candidate_positions)[3:7] <- c("airport_expansion", "finance_budget", "transit", "transportation", "waste_management")
candidate_positions <- 
  candidate_positions  %>% filter(year == 2014) %>% 
  group_by(candidate, year) %>%
  summarise_each(funs(mean))

# Create data frame -------------------------------------------------------

issues_df <- vote_history  %.%
  group_by(ward, area, candidate, add = FALSE) %.%
  summarize(votes=sum(votes), airport_expansion=mean(`Airport expansion`), finance_budget=mean(`Finance & Budget`), transit=mean(Transit), transportation=mean(Transportation), waste_management=mean(`Waste management`))
issues_df$ward <- as.factor(issues_df$ward)
issues_df <- group_by(issues_df[,c(1,4:9)], ward)

# Issue models for each ward ----------------------------------------------

models <- issues_df %>% do(mod = lm(votes ~ transportation + transit + finance_budget + waste_management + airport_expansion, data = ., na.action = na.omit))
models <- models %>% group_by(ward)

# Predict votes for positions ---------------------------------------------

issue_resolution <- 10.0
issue_vector <- as.integer(seq(20, 80, issue_resolution))
issue_universe <- expand.grid(airport_expansion = issue_vector, finance_budget = issue_vector, transit = 50, transportation = issue_vector, waste_management = 50, ward = levels(issues_df$ward))
issue_universe <- issue_universe %>% group_by(airport_expansion, finance_budget, transit, transportation, waste_management, ward)
predictions <- issue_universe %>% do(votes = predict(models$mod[models$ward==.$ward][[1]], ., interval = "confidence")[1])
predictions$votes <- unlist(predictions$votes)
rm(issue_resolution, issue_vector, issue_universe)
#saveRDS(predictions, "data/issue_predictions.rds")

# Plot contour maps -------------------------------------------------------

predictions <- readRDS("data/issue_predictions.rds")
plot_axes <- c("transportation", "airport_expansion")
# Dynamically summarize votes, based on plot_axes
data <- eval(parse( text = paste( "predictions %>% group_by(ward, ", plot_axes[1], ",", plot_axes[2], ", add = FALSE) %>% summarize(votes = mean(votes))", sep ='')))
tory_positions <- eval(parse( text = paste( "candidate_positions %>% select(", plot_axes[1], ",", plot_axes[2], ") %>% filter(candidate == \"tory john\")", sep ='')))

# Plot the results
ggplot(data, aes_string(x = plot_axes[1], y = plot_axes[2], z = plot_response)) + 
  stat_contour(aes(colour = ..level..)) +
  scale_colour_gradient2("Votes", low = "white") +
  annotate("text", x = tory_positions[1,2], y = tory_positions[1,3], label = "T") + 
  facet_wrap(~ward)
