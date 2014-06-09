#----------
# Imports the candidate position excel file
# Outputs candidate_positions.RData
#----------
library(XLConnect)
library(dplyr)
library(reshape2)
drop_topic <- "Life & environment" # Drop this topic
positions_excel <- readWorksheet(object=loadWorkbook("data//Position Scores.xlsx"), "Detailed Postions", startRow=3)
names(positions_excel) <- tolower(names(positions_excel))
positions_excel$candidate <- tolower(positions_excel$candidate)
positions_excel$year <- as.factor(positions_excel$year)
candidate_positions <- positions_excel %.%
  mutate(topic=topic.type) %.%
  group_by(candidate, year, topic) %.%
  summarize(score=mean(score, na.rm = TRUE)) %.%
  filter(topic!=drop_topic)
candidate_scores <- positions_excel %.% # Generate summary left-right score
  filter(topic!=drop_topic) %.% # Drop this topic
  group_by(candidate, year) %.%
  summarize(left_right_score=mean(score, na.rm = TRUE))
candidate_positions <- melt(candidate_positions)
candidate_positions <- dcast(candidate_positions, candidate + year ~ topic)
candidate_positions <- inner_join(candidate_positions,candidate_scores, by=c("candidate","year"))
rm(positions_excel, candidate_scores)
save(candidate_positions, file = "data/candidate_positions.RData")
