library(tidyverse)
library(lubridate)
library(stringr)
library(RSQLite)

## Set working directory
if (length(grep("herron", Sys.info()["user"]))) {
  ## Michael
  setwd("/Users/herron/research/EVID/R")
} else {
  ## David
  setwd("~/Dropbox/EVID/R")
}


# Create file.sqlite3 -----------------------------------------------

my_db <- src_sqlite("../Data/evid-db.sqlite3", create = TRUE)              # create src


# Create voter history table -----------------------------------------------

nms_h <- c("CountyCode", "VoterID", "ElectionDate", "ElectionType", "HistoryCode")
history <- read_delim("../Data/Parsed/voterfile-history-jan-2016.txt", delim = "\t", col_names = nms_h, col_types = cols("c", "c", "c", "c", "c"), quote = "") # read 2016 history
history <- history %>% separate(ElectionDate, sep = "-", into = c("y", "m", "d")) %>% unite_("ElectionDate", c("m", "d", "y"), sep = "/")
names(history) <- tolower(nms_h)
copy_to( my_db, history, "history", overwrite = TRUE, temporary = FALSE, indexes = list("voterid"))                 # create table


#files  <- list.files("../Data/Parsed/VoterFile-Dec-2014/VoterHistory",full.names =  T) #Palm Beach is missing voters in 2014
files  <- list.files("../Data/Parsed/VoterFile-Apr-2015/VoterHistory",full.names =  T)

for (i in 1:length(files)){
  print(files[i])
  file <-  read_delim(files[i], delim = "\t", col_names = nms_h, col_types = cols("c", "c", "c", "c", "c"), quote = "")
  db_insert_into( con = my_db$con, table = "history", values = file)  #Insert all files into db
}

#Create hierarchy to remove duplicates
dedup <- function (h) {
  codes <- c("E","Y", "A", "Z", "F", "B", "P", "N")
  h$historycode <- factor(h$historycode, codes)
  h <- h %>%
    arrange(voterid,historycode) %>%
    distinct(voterid, .keep_all = T)
  h$historycode <- as.character(h$historycode)
  return(h)
}

#Create separate tables for 2008, 2012, 2014, and 2016 general elections
history <- my_db %>% tbl("history")
history <- history %>% filter(electiontype == "GEN", historycode != "B", historycode != "P")

history08 <- history %>% filter(electiondate == "11/04/2008") %>% select(voterid, historycode) %>% collect(Inf)
history08 <- history08 %>% dedup()
history08 <- history08 %>% rename(gen08 = historycode)
copy_to(my_db, history08, "history08", overwrite = TRUE, temporary = FALSE, indexes = list("voterid"))  
rm(history08)

history12 <- history %>% filter(electiondate == "11/06/2012") %>% select(voterid, historycode) %>% collect(Inf)
history12 <- history12 %>% dedup()
history12 <- history12 %>% rename(gen12 = historycode)
copy_to( my_db, history12, "history12",overwrite = TRUE, temporary = FALSE, indexes = list("voterid"))
rm(history12)

history14 <- history %>% filter(electiondate == "11/04/2014") %>% select(voterid, historycode) %>% collect(Inf)
history14 <- history14 %>% dedup()
history14 <- history14 %>% rename(gen14 = historycode)
copy_to( my_db, history14, "history14", overwrite = TRUE, temporary = FALSE, indexes = list("voterid"))
rm(history14)

history16 <- history %>% filter(electiondate == "11/08/2016") %>% select(voterid, historycode) %>% collect(Inf)
history16 <- history16 %>% dedup()
history16 <- history16 %>% rename(gen16 = historycode)
copy_to( my_db, history16, "history16",overwrite = TRUE,  temporary = FALSE, indexes = list("voterid"))
rm(history16)




# Create extract table -----------------------------------------------

nms<-c("CountyCode","VoterID","NameLast","NameSuffix","NameFirst","NameMiddle","Requestedpublicrecordsexemption","ResidenceAddressLine1","ResidenceAddressLine2","ResidenceCity(USPS)","ResidenceState","ResidenceZipcode","MailingAddressLine1","MailingAddressLine2","MailingAddressLine3","MailingCity","MailingState","MailingZipcode","MailingCountry","Gender","Race","BirthDate","RegistrationDate","PartyAffiliation","Precinct","PrecinctGroup","PrecinctSplit","PrecinctSuffix","VoterStatus","CongressionalDistrict","HouseDistrict","SenateDistrict","CountyCommissionDistrict","SchoolBoardDistrict","DaytimeAreaCode","DaytimePhoneNumber","DaytimePhoneExtension","Emailaddress")


tps <- cols("c","c","c","c","c","c","c","c","c","c","c","i","c","c","c","c","c","c","c","c","i","c","c","c","c","i","c","c","c","i","i","i","i","i","i","i","c","c")
extract <- read_delim("../Data/Parsed/voterfile-extract-jan-2017.txt", delim = "\t", col_names = nms, na = c("NA", "", " ", "*"), col_types = tps , quote = "")
extract <- extract %>% 
  select(VoterID, NameFirst, NameMiddle, NameLast,CountyCode, Gender, Race, BirthDate, RegistrationDate, PartyAffiliation, VoterStatus, ResidenceZipcode, Precinct, PrecinctSplit) %>%
  mutate(ResidenceZipcode = str_sub(ResidenceZipcode,1,5))
names(extract) <- tolower(names(extract))
copy_to( my_db, extract, "extract", overwrite = TRUE, temporary = FALSE, indexes = list("voterid"))   


