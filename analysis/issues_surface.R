# Basic setup -------------------------------------------------------------

library(dplyr)
library(reshape2)
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

issue_resolution <- 10
issue_vector <- as.integer(seq(20, 80, issue_resolution))
issue_universe <- expand.grid(airport_expansion = issue_vector, finance_budget = issue_vector, transit = issue_vector, transportation = issue_vector, waste_management = issue_vector, ward = levels(issues_df$ward), votes = 0, lwr = 0, upr = 0)

for (i in 1:dim(issue_universe)[1]) {
  issue_universe[i,7:9] <- predict(models$mod[models$ward==issue_universe$ward[i]][[1]], issue_universe[i,1:5], interval = "confidence")
}
