library(XLConnect)
library(reshape)

importWorksheets <- function(filename) { # Takes a filename and returns a dataframe
  workbook <- loadWorkbook(filename)
  sheet_names <- getSheets(workbook)
  names(sheet_names) <- sheet_names
  sheet_list <- lapply(sheet_names, function(.sheet){ # Take each sheet, melt the data into a long table, and append the year, candidate type, and ward
      data.frame(fyear,
                 melt(readWorksheet(object=workbook, .sheet, startRow=ifelse(fyear=="2003",3,2))), # Different number of header rows in 2003
                 ifelse(fyear=="2003",(strsplit(.sheet, ' Ward ')[[1]][[1]]),"Mayor"), # Different worksheet-name structure for 2003
                 ifelse(fyear=="2003",strsplit(.sheet, ' Ward ')[[1]][[2]],strsplit(.sheet, ' Ward ')[[1]][[1]]))
    })
  do.call("rbind", sheet_list) # Collect the dataframes for each worksheet from the file into a single dataframe
}

processFiles <- function(pattern) { # Takes a filename pattern and returns a dataframe
  lfiles <- list.files(pattern = fpattern) # Find all files that match the pattern
  results <- do.call("rbind", lapply(lfiles, importWorksheets)) # Collapse all down to a single dataframe
  names(results) <-c("year", "candidate", "area", "votes", "type", "ward") # Clean up the names
  row.names(results) <- NULL # Clear the rownames
  results <- subset(results, area!="Total") # Drop the total column
  results <- subset(results, !grepl("Totals", candidate)) # Drop the total rows
}


fyear <- "2010"
fpattern <- paste(fyear,".*_Mayor.xls?",sep="")  # pattern for filenames
election_results <- processFiles(fpattern)
election_results$candidate <- gsub(",","",tolower(election_results$candidate))
election_results$ward <- gsub("Ward","",election_results$ward)
election_results$area <- gsub("x","",election_results$area)
write.csv(election_results, file=paste(fyear,"_election_results.csv",sep=""))
          
rm(fpattern,fyear)
election_results <- rbind(read.csv("2003_election_results.csv"), read.csv("2006_election_results.csv"), read.csv("2010_election_results.csv"))

write.csv(election_results[,-1], file="Data//election_results.csv")

###
# Turnout
###

turnout_2003 <- read.csv(file="Data//2003 turnout.csv")
turnout_2006 <- read.csv(file="Data//2006 turnout.csv")
turnout_2010 <- read.csv(file="Data//2010 turnout.csv")
turnout <- rbind(turnout_2003,turnout_2006,turnout_2010)
turnout <- subset(turnout, !grepl("Total", ward)) # Drop the total rows
write.csv(turnout,file="Data//turnout.csv")
