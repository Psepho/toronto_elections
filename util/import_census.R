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

# 2006 age-sex Census data
library(dplyr)
census_text <- readLines("data/census.txt")
census_2006 <- matrix(data=census_text,ncol=4,byrow=TRUE, dimnames=list(NULL,c("GEO","age","sex","count")))
rm(census_text)
census_2006 <- as.data.frame(census_2006)
census_2006_df <- tbl_df(census_2006)
toronto_census_2006 <- subset(census_2006_df, grepl("^535", GEO))
write.csv(toronto_census_2006,file="data/toronto_census_2006.csv",row.names=FALSE)
head(toronto_census_2006)
rm(census_2006, census_2006_df)

# 2006 income Census data
library(dplyr)
census_text <- readLines("data/census_2006.txt")
census_2006 <- matrix(data=census_text,ncol=4,byrow=TRUE, dimnames=list(NULL,c("GEO","dim0","dim1","value")))
rm(census_text)
census_2006 <- as.data.frame(census_2006)
census_2006_df <- tbl_df(census_2006)
toronto_census_2006 <- subset(census_2006_df, grepl("^535", GEO))
write.csv(toronto_census_2006,file="data/census_income_2006.csv",row.names=FALSE)
head(toronto_census_2006)
rm(census_2006, census_2006_df)


#----------
# Create census summary
#----------
library(dplyr)
library(RSQLite)
library(RSQLite.extfuns)
elections_db <- src_sqlite("data/elections_db.sqlite3")
# Age sex
age_sex <- as.data.frame(collect(tbl(elections_db,"age_sex"))) %.%
  filter(count>0)
age_sex$SGC <- as.character(age_sex$SGC)
names(age_sex)[4] <- "GEO"
age_sex_expanded <- data.frame(age=rep(age_sex[,1],times=age_sex[,3]),sex=rep(age_sex[,2],times=age_sex[,3]),GEO=rep(age_sex[,4],times=age_sex[,3]),census=rep(age_sex[,5],times=age_sex[,3]))
age_sex_summary <- age_sex_expanded %.%
  group_by(GEO) %.%
  summarize(median_age = median(age))
rm(age_sex_expanded)
# Family income
family_income <- as.data.frame(collect(tbl(elections_db,"family_income")))
family_income <- transform(family_income,SGC=as.character(family_income$SGC), income_type=as.factor(family_income$income_type),family_structure=as.factor(family_income$family_structure))
names(family_income)[c(1,4)] <- c("GEO","income")
family_median_income <- family_income %.%
  filter(income_type=="Median 2005 family income $",family_structure=="Total - All economic families") %.%
  group_by(GEO) %.%
  select(GEO,income)
census <- as.data.frame(collect(tbl(elections_db,"census"))) %.%
  mutate(GEO=as.character(GEO)) %.%
  select(-c(3:4)) %.%
  group_by(GEO)
census_commuting <- census %.%
  filter(Topic=="Median commuting duration", Characteristic=="Median commuting duration") %.%
  summarize(commuting_duration=median(Total))
census_summary <- as.data.frame(inner_join(census_commuting,family_median_income, by=c("GEO")))
census_summary$GEO <- as.factor(census_summary$GEO)
census_summary <- as.data.frame(inner_join(census_summary,age_sex_summary, by=c("GEO")))
save(census_summary,file="data/census.RData")
