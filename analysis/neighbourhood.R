library(ggmap)
library(maptools)
library(mapproj)
library(dplyr)
library(XLConnect)
library(scales)
load("data/map_data.RData")
neighbourhoods <- readWorksheet(object=loadWorkbook("data//core_gotv.xlsx"), "Summary", startRow=1)
names(neighbourhoods) <- c("n_type", "ward", "area")
neighbourhoods <- transform(neighbourhoods, n_type = as.factor(n_type), ward = as.integer(ward), area = as.integer(area))
neighbourhoods$n_type <- ordered(neighbourhoods$n_type, levels = c("Core", "Other", "GOTV"))
levels(neighbourhoods$n_type)[2] <- "Neutral"
geo <- left_join(geo, neighbourhoods, by = c("ward", "area")) %.%
  filter(year == 2014)
toronto_map +
  geom_polygon(aes(x=long, y=lat, group=group, fill=n_type), alpha = 5/6, data=geo) +
  scale_fill_brewer("Type",type="div", palette = "RdBu")
ggsave(file = "fig/priority_neighbourhood_map.png")

# toronto_map +
#   geom_polygon(aes(x=long, y=lat, group=group, fill=n_type), alpha = 5/6, data=filter(geo, n_type != "Other")) +
#   scale_fill_manual("Type", values = alpha(c("blue", "red"), 0.3))
