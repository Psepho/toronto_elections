data <- read.csv(file="../data/98-316-XWE2011001-401.CSV")
# Downloaded from http://www12.statcan.gc.ca/census-recensement/2011/dp-pd/prof/details/page.cfm?Lang=E&Geo1=CSD&Code1=3520005&Geo2=CD&Code2=3520&Data=Count&SearchText=toronto&SearchType=Begins&SearchPR=01&B1=All&Custom=&TABID=1
library(dplyr)
census_df <- tbl_df(data)
toronto_census <- census_df %.%
  filter(CMACA_Name=="Toronto") %.%
  filter(Topic=="Population and dwelling counts" |  Topic=="Age characteristics") %.%
  select(Geo_Code,Characteristic,Total,Male,Female)
head(toronto_census)
