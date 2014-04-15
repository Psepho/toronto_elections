load("data//census.RData")
source("analysis//setup.R")
locations <- read.csv("data//voting_locations.csv")
locations <- locations %.%
  filter(year==2010) %.%
  mutate(ward_area=paste(ward,area,sep="_"),GEO=as.character(CTUID))
locations <- locations[,c(12:13)]
locations$GEO <- as.factor(locations$GEO)
library(MASS)
require(akima)
require(mgcv)

model <- (~ commuting_duration + income + median_age)
pca <- princomp(model, data = census_summary, na.action = na.omit, cor = TRUE)
summary(pca)
loadings(pca)
biplot(pca, c(1,2), scale = TRUE, main = model)
biplot(pca, c(1,3), scale = TRUE, main = model)

model_predictions <- predict(pca)
merged_data <- merge(census_summary, model_predictions, by="row.names", all.x = TRUE, sort = FALSE)
merged_data <- merged_data[,-1]
merged_data <- inner_join(merged_data,locations, by=c("GEO"))
merged_data <- inner_join(merged_data,positions_average_by_ward_area, by=c("ward_area"))

position_model <- lm(position ~ Comp.1 + Comp.2, data = merged_data)
summary(position_model)
