
library(rvest)
library(tidyverse)
library(stringr)

columns <-cols(
  COUNTY_OF_RESIDENCE = col_character(),
  VOTER_ID = col_character(),
  VOTED = col_character()
)
sht1 <- read_csv("/Users/David/Dropbox/EVID-source-data/Source/ORA2016-1.csv", col_types = columns)
sht2 <- read_csv("/Users/David/Dropbox/EVID-source-data/Source/ORA2016-2.csv", col_types = columns)
sht3 <- read_csv("/Users/David/Dropbox/EVID-source-data/Source/ORA2016-3.csv", col_types = columns)
sht4 <- read_csv("/Users/David/Dropbox/EVID-source-data/Source/ORA2016-4.csv", col_types = columns)

df16 <- bind_rows(sht1, sht2, sht3, sht4)
names(df16) <- c("county", "voterid", "datetime")
df16 <- separate(df16, datetime, into = c("date", "time"), sep = " ")

write.table(df16,"../Data/Parsed/OrangeEarlyVotingEVID2016General-parsed.txt", sep = ";", row.names = F)

sht1 <- read_csv("/Users/David/Dropbox/EVID-source-data/Source/ORA2012-1.csv", col_types = columns)
sht2 <- read_csv("/Users/David/Dropbox/EVID-source-data/Source/ORA2012-2.csv", col_types = columns)

df12 <- bind_rows(sht1, sht2)
names(df12) <- c("county", "voterid", "datetime")
df12 <- separate(df12, datetime, into = c("date", "time"), sep = " ")
write.table(df12,"../Data/Parsed/OrangeEarlyVotingEVID2012General-parsed.txt", sep = ";", row.names = F)
