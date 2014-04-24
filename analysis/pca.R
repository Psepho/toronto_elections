load("data//census.RData")
source("analysis//setup.R")
locations <- read.csv("data//voting_locations.csv")
locations <- locations %.%
  filter(year==2010) %.%
  mutate(ward_area=paste(ward,area,sep="_"),GEO=as.character(CTUID))
locations <- locations[,c(12:13)]
locations$GEO <- as.factor(locations$GEO)
locations <- inner_join(locations,positions_average_by_ward_area, by=c("ward_area")) %.%
  group_by(GEO) %.%
  summarize(position=mean(position))
library(MASS)
require(akima)
require(mgcv)

#model <- (~ commuting_duration + income + median_age + median_home_value + public_transit + children)
model <- (~ median_age + median_home_value + public_transit + children)
pca <- princomp(model, data = census_summary, na.action = na.omit, cor = TRUE)
summary(pca)
loadings(pca)
biplot(pca, c(1,2), scale = TRUE, main = model)
biplot(pca, c(1,3), scale = TRUE, main = model)

model_predictions <- predict(pca)
merged_data <- merge(census_summary, model_predictions, by="row.names", all.x = TRUE, sort = FALSE)
merged_data <- merged_data[,-1]
merged_data <- inner_join(merged_data,locations, by=c("GEO"))

position_model <- lm(position ~ Comp.1 + Comp.2, data = merged_data)
summary(position_model)

plot(pca$scores[,1:2], main = "Position score", xlab = "Comp1", ylab = "Comp2")
contour(interp(pca$scores[,1], pca$scores[,2], fitted(position_model), duplicate = "mean"), add = TRUE, col = "red", labcex = 0.8)
AbilityContour <- merge(MuniPCA$scores, as.data.frame(fitted(AbilityModel)), by = "row.names")
AbilityContour <- AbilityContour[,-1]
names(AbilityContour)[5] <- "fitted"
contour(interp(AbilityContour[,1], AbilityContour[,2], AbilityContour$fitted, duplicate = "mean"), add = TRUE, col = "blue", labcex = 0.8)


predict.pca <- predict(pca)
eqscplot(model_predictions)
