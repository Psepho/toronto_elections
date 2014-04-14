source("analysis//setup.R")
conn <- dbConnect("SQLite", dbname = "data/elections_db.sqlite3")
dbListTables(conn)
# Age sex
age_sex <- as.data.frame(collect(tbl(elections_db,"age_sex"))) %.%
  filter(count>0)
age_sex_expanded <- data.frame(age=rep(age_sex[,1],times=age_sex[,3]),sex=rep(age_sex[,2],times=age_sex[,3]),SGC=rep(age_sex[,4],times=age_sex[,3]),census=rep(age_sex[,5],times=age_sex[,3]))
age_sex_summary <- age_sex_expanded %.%
  group_by(census,sex,SGC) %.%
  summarize(median_age = median(age))
rm(age_sex_expanded)
# Family income
family_income <- as.data.frame(collect(tbl(elections_db,"family_income")))
family_income <- transform(family_income,income_type=as.factor(family_income$income_type),family_structure=as.factor(family_income$family_structure))
family_median_income <- family_income %.%
  filter(income_type=="Median 2005 family income $",family_structure=="Total - All economic families") %.%
  group_by(SGC) %.%
  select(SGC,value)

census <- as.data.frame(collect(tbl(elections_db,"census"))) %.%
  mutate(SCG=as.character(GEO))
head(filter(census,Characteristic=="Median commuting duration"))

head(paste(as.character(census$GEO),str_sub(as.character(census$CT_Name),start=-2),sep=""))
head(as.character(census$GEO)
head(census$CT_Name)
head(str_sub(as.character(census$CT_Name),start=-2))
length(census$CT_Name)
census_commuting <- census %.%
  group_by(GEO) %.%
  filter(Topic=="Median commuting duration", Characteristic=="Median commuting duration") %.%
  summarize(median_commuting_duration=mean(Total))
