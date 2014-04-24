# Basic setup -------------------------------------------------------------

source("analysis//setup.R")

# Create data frame -------------------------------------------------------

issues_df <- positions  %.%
  group_by(ward, area, candidate, add = FALSE) %.%
  summarize(votes=sum(votes), airport_expansion=mean(`Airport expansion`), finance_budget=mean(`Finance & Budget`), transit=mean(Transit), transportation=mean(Transportation), waste_management=mean(`Waste management`))
issues_df$ward <- as.factor(issues_df$ward)
issues_df <- group_by(issues_df[,c(1,4:9)], ward)
areas_per_ward <- areas_for_2014 %.%
  group_by(ward) %.%
  summarize(count=n())

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
max_coefficients_by_ward <- ddply(coefs_melt, "ward", function(x) x[which.max(x$value),])

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
#results <- left_join(results, areas_per_ward, by = "ward")
#results$adj_votes <- results$votes * results$count
prop.table(tapply(results$votes, results[1:2], sum, na.rm=TRUE),2)
prop.table(tapply(results$votes, results[1], sum, na.rm=TRUE))
max_coefficients_by_ward

# dplyr attempt -----------------------------------------------------------

issues_df.dplyr <- group_by(issues_df[,c(1,4:9)], ward)
issues_by_ward.dplyr <- do(issues_df.dplyr, failwith(NULL, lm), formula = issues_formula)

# Archived stuff ----------------------------------------------------------
# library(biglm)
# 
# issues_df <- positions  %.%
#   group_by(ward, area, candidate, add = FALSE) %.%
#   summarize(votes=sum(votes), transportation=mean(Transportation), life_environment=mean(Life_environment), finance_budget=mean(Finance_Budget), waste_management=mean(Waste_management))
# issues_df$ward <- as.factor(issues_df$ward)
# 
# i_model <- votes ~ transportation + life_environment + finance_budget + waste_management
# issues_model <- lm(i_model, data=issues_df, na.action = na.omit)
# summary(issues_model)
# issues_model_ward <- update(issues_model, . ~ (.):ward)
# summary(issues_model_ward)
# anova(issues_model,issues_model_ward)
