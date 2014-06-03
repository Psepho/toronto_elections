# Basic setup -------------------------------------------------------------

library(ggmap)
library(maptools)
library(mapproj)
library(dplyr)
library(reshape2)
library(MASS)
library(akima)
library(mgcv)
load("data/census.RData")
load("data/map_data.RData")
load("data/vote_history.RData")
vote_history$CTUID <- as.factor(vote_history$CTUID)
vote_history <- vote_history %.%
  group_by(year, ward, area, CTUID) %.%
  summarize(area_position = mean(weighted_votes))
census_toronto <- inner_join(census_summary,vote_history, by=c("CTUID"))
census_toronto$prop_children <- census_toronto$children/census_toronto$pop

# Create PCA model --------------------------------------------------------

#model <- (~ commuting_duration + income + median_age + median_home_value + public_transit + children)
model <- (~ median_age + median_home_value + public_transit + prop_children)
pca <- princomp(model, data = census_toronto, na.action = na.omit, cor = TRUE)
summary(pca)
loadings(pca)
biplot(pca, c(1,2), scale = TRUE, main = model)
biplot(pca, c(1,3), scale = TRUE, main = model)

# Test components against voting patterns ---------------------------------

model_predictions <- predict(pca)
merged_data <- merge(census_toronto, model_predictions, by="row.names", all.x = TRUE, sort = FALSE)
merged_data <- merged_data[,-1]
position_model <- lm(area_position ~ Comp.1 + Comp.2 + Comp.3 + Comp.4, data = merged_data)
summary(position_model)

# Map components ----------------------------------------------------------

component_data <- merged_data[,c(9,10,11,14:17)]
component_data <- melt(component_data, id.vars = c("year", "ward", "area"), variable.name = "component", value.name = "loading")
geo <- left_join(component_data, geo, by = c("ward", "area", "year"))
levels(geo$component) <- c("Young transit users", "Poorer families", "Richer families", "Comp.4")
geo <- filter(geo, component!="Comp.4")
toronto_map +
  geom_polygon(aes(x=long, y=lat, group=group, fill=cut_interval(loading, n = 8)), alpha = 5/6, data=geo) + 
  scale_fill_brewer("Component loading", type = "div", palette = "RdBu", labels=c("Low", rep("", 6), "High")) +   
  facet_wrap(~component)
ggsave(file = "fig/pca_component_map.png")
# plot(pca$scores[,1:2], main = "Position score", xlab = "Comp1", ylab = "Comp2")
# contour(interp(pca$scores[,1], pca$scores[,2], fitted(position_model), duplicate = "mean"), add = TRUE, col = "red", labcex = 0.8)
# AbilityContour <- merge(MuniPCA$scores, as.data.frame(fitted(AbilityModel)), by = "row.names")
# AbilityContour <- AbilityContour[,-1]
# names(AbilityContour)[5] <- "fitted"
# contour(interp(AbilityContour[,1], AbilityContour[,2], AbilityContour$fitted, duplicate = "mean"), add = TRUE, col = "blue", labcex = 0.8)
# predict.pca <- predict(pca)
# eqscplot(model_predictions)
