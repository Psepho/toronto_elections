# Basic setup -------------------------------------------------------------

source("analysis//setup.R")

# Create data frame -------------------------------------------------------

issues_df <- positions  %.%
  group_by(ward, area, candidate, add = FALSE) %.%
  summarize(votes=sum(votes), transportation=mean(Transportation), life_environment=mean(Life_environment), finance_budget=mean(Finance_Budget), waste_management=mean(Waste_management))
issues_df$ward <- as.factor(issues_df$ward)

# Issue models for each ward ----------------------------------------------

library(plyr)
library(dplyr)
issues_model <- function(df){
  lm(votes ~ transportation + life_environment + finance_budget + waste_management, data = df)
}
issues_by_ward <- dlply(issues_df, .(ward), issues_model)
coefs_by_ward <- ldply(issues_by_ward, function(x) coef(x))

# Predict votes from position scores --------------------------------------

#Not sure if this is necessary, does it match by names?
# names(candidate_positions)[4:7] <- names(issues_df)[c(7,5,8,6)]
pred_values <- issues_df[1,5:8] # Position scores
predicted_votes <- ldply(issues_by_ward, function(x) predict(x, pred_values, interval = "confidence"))

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
# 
# issues_ward_df <- positions %.%
#   group_by(ward,candidate, add = FALSE) %.%
#   summarize(votes=sum(votes), transportation=mean(Transportation), life_environment=mean(Life_environment), finance_budget=mean(Finance_Budget), waste_management=mean(Waste_management))
# issues_ward_df$ward <- as.factor(issues_ward_df$ward)
# 
# mod1 <- do(issues_ward_df, biglm, formula = votes ~ transportation + life_environment + finance_budget + waste_management)
# mod2 <- do(issues_df, failwith(NULL, lm), formula = votes ~ transportation + life_environment + finance_budget + waste_management)
