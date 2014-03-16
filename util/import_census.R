# 2010 Census data
data <- read.csv(file="../data/98-316-XWE2011001-401.CSV")
# Downloaded from http://www12.statcan.gc.ca/census-recensement/2011/dp-pd/prof/details/page.cfm?Lang=E&Geo1=CSD&Code1=3520005&Geo2=CD&Code2=3520&Data=Count&SearchText=toronto&SearchType=Begins&SearchPR=01&B1=All&Custom=&TABID=1
library(dplyr)
census_df <- tbl_df(data)
toronto_census <- census_df %.%
  filter(CMACA_Name=="Toronto") %.%
  filter(Topic=="Population and dwelling counts" |  Topic=="Age characteristics") %.%
  select(Geo_Code,Characteristic,Total,Male,Female)
head(toronto_census)

# 2006 Census data
library(dplyr)
census_text <- readLines("census.txt")
census_2006 <- matrix(data=census_text,ncol=4,byrow=TRUE, dimnames=list(NULL,c("GEO","age","sex","count")))
rm(census_text)
census_2006 <- as.data.frame(census_2006)
census_2006_df <- tbl_df(census_2006)
toronto_census_2006 <- census_2006_df %.%
  filter(grep("5",GEO))
head(toronto_census_2006)
rm(census_2006, census_2006_df)
subset(toronto_census_2006, grepl("001000100", GEO))
