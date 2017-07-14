library(tidyverse)
library(hms)
library(stringr)
library(DBI)


coltypes <- list(
  voterid = col_character(),
  date = col_character(), 
  time = col_character()
)


# Load EVL data -----------------------------------------------------------

simpleCap <- function(x) {
  gsub("(?<=\\b)([a-z])", "\\U\\1", tolower(x), perl=TRUE)
}

path <- "../Data/Parsed/2012GEEarly.txt"
loc12 <- read_delim(path, delim = "\t", col_types = cols(.default = "c")) # expect warning from number of columns error
loc12 <- loc12 %>% select(voterid = FvrsVoterIdNumber, location = Location)
loc12 <- loc12 %>% filter(voterid != "FvrsVoterIdNumber", !duplicated(voterid)) # get rid of the repeated header and the  32 duplicated ids
loc12 <- loc12 %>% mutate(location = simpleCap(location))

path <- "../Data/Parsed/2016GEEarly.txt"
loc16 <- read_delim(path, delim = "\t", col_names = F, col_types = cols(.default = "c")) 
loc16 <- loc16 %>% select(voterid = X6, location = X17) 
loc16 <- loc16 %>% filter(!duplicated(voterid)) # get rid of the repeated header and the  32 duplicated ids
loc16 <- loc16 %>% mutate(location = simpleCap(location))


# Alachua -----------------------------------------------------------------

path <- "../Data/Parsed/AlachuaEarlyVotingEVID2012General-parsed.txt"
ala12 <- read_delim(path, delim = ";", col_types = coltypes)
ala12 <- ala12 %>% transmute(voterid, county = "ALA", location, date, time)
ala12 <- ala12 %>% mutate(time = str_pad(time, pad = "0", width = 5, side = "left"))


path <- "../Data/Parsed/AlachuaEarlyVotingEVID2016General-parsed.txt"
ala16 <- read_delim(path, delim = "\t", col_types = coltypes)
ala16 <- ala16 %>% transmute(voterid, county  = "ALA", location, date, time)
ala16 <- ala16 %>% mutate(time = str_pad(time, pad = "0", width = 5, side = "left"))


# Broward -----------------------------------------------------------------

path <- "../Data/Parsed/BrowardEarlyVotingEVID2012General-parsed.txt"
bro12 <- read_delim(path, delim = ";", col_types = coltypes)
bro12 <- bro12 %>% transmute(voterid, county = "BRO", location, date, time)
bro12 <- bro12 %>% mutate(time = str_pad(time, pad = "0", width = 5, side = "left"))

path <- "../Data/Parsed/BrowardEarlyVotingEVID2016General-parsed.txt"
bro16 <- read_delim(path, delim = ";", col_types = coltypes)
bro16 <- bro16 %>% transmute(voterid, county = "BRO", date, time)
bro16 <- bro16 %>% mutate(time = str_sub(time, 1, 5) ) 
bro16 <- bro16 %>% separate(date, into = c("year", "month", "day"), sep = "-")
bro16 <- bro16 %>% mutate(year = str_replace(year, "20", ""))
bro16 <- bro16 %>% unite(date, month, day, year, remove = T, sep = "/")
bro16 <- bro16 %>% filter(date != "11/08/16")
bro16 <- bro16 %>% left_join(loc16, by = "voterid") 
bro16 <- bro16 %>% filter(!is.na(location)) #remove 195,887 that dont match to in-person early voting locations
bro16 <- bro16 %>% select(voterid, county, location, date, time)
bro16 <- bro16 %>% mutate(time = str_pad(time, pad = "0", width = 5, side = "left"))


# Hillsborough -----------------------------------------------------------------

path <- "../Data/Parsed/HillsboroughEarlyVotingEVID2012General-parsed.txt"
hil12 <- read_delim(path, delim = ";", col_types = coltypes)
hil12 <- hil12 %>% transmute(voterid,county = "HIL", location, date, time)
hil12 <- hil12  %>% mutate(time = str_pad(time, pad = "0", width = 5, side = "left"))


path <- "../Data/Parsed/HillsboroughEarlyVotingEVID2016General-parsed.txt"
hil16 <- read_delim(path, delim = ";", col_types = coltypes)
hil16 <- hil16 %>% transmute(voterid,county = "HIL", location, date, time)
hil16 <- hil16 %>% mutate(time = str_pad(time, pad = "0", width = 5, side = "left"))


# Miami Dade -----------------------------------------------------------------

path <- "../Data/Parsed/MiamiDadeEarlyVotingEVID2012General-parsed.txt"
dad12 <- read_delim(path, delim = ";", col_types = coltypes)
dad12 <- dad12 %>% transmute(voterid, county = "DAD", location, date, time= str_pad(time, pad = "0", width = 5, side = "left"))
dad12 <- dad12 %>% mutate(time = str_pad(time, pad = "0", width = 5, side = "left"))

path <- "../Data/Parsed/MiamiDadeEarlyVotingEVID2016General-parsed.txt"
dad16 <- read_delim(path, delim = ";", col_types = coltypes)
dad16 <- dad16 %>% transmute(voterid, county = "DAD", location, date, time)
dad16 <- dad16 %>% separate(time, into = c("hour", "minute", "second", "AMPM"), sep = ":| ")
dad16 <- dad16 %>% mutate(hour = str_pad(if_else(AMPM == "PM" & hour != "12" , as.character(as.integer(hour) + 12), hour), pad = "0", width = 2))
dad16 <- dad16 %>% unite(time, hour, minute,remove = T, sep = ":")
dad16 <- dad16 %>% select(voterid, county, location, date, time)
dad16 <- dad16 %>% mutate(time = str_pad(time, pad = "0", width = 5, side = "left"))

# Orange -----------------------------------------------------------------

path <- "../Data/Parsed/OrangeEarlyVotingEVID2012General-parsed.txt"
ora12 <- read_delim(path, delim = ";", col_types = coltypes)
ora12 <- ora12 %>% transmute(voterid, county = "ORA", date, time)
ora12 <- ora12 %>% filter(!is.na(time))
ora12 <- ora12 %>% mutate(time = str_sub(time, 1, 5), date = str_replace(date, "2012", "12") )
ora12 <- left_join(ora12, loc12, by = "voterid")
ora12 <- ora12 %>% select(voterid, county, location, date, time)
ora12 <- ora12 %>% mutate(time = str_pad(time, pad = "0", width = 5, side = "left"))
ora12 <- ora12 %>% filter(!is.na(location)) #remove 292 that dont match to in-person early voting locations

path <- "../Data/Parsed/OrangeEarlyVotingEVID2016General-parsed.txt"
ora16 <- read_delim(path, delim = ";", col_types = coltypes)
ora16 <- ora16 %>% transmute(voterid, county = "ORA", date, time)
ora16 <- ora16 %>% mutate(time = str_sub(time, 1, 5) , date = str_replace(date, "2016", "16") ) 
ora16 <- left_join(ora16, loc16, by = "voterid")
ora16 <- ora16 %>% select(voterid, county, location, date, time)
ora16 <- ora16 %>% filter(!is.na(location)) #remove 8 that dont match to in-person early voting locations

# Palm Beach --------------------------------------------------------------

path <- "../Data/Parsed/PalmBeachEarlyVotingEVID2012General-parsed.txt"
pal12 <- read_delim(path, delim = "\t", col_types = cols(.default = "c"))
names(pal12) <- c("voterid", "last", "first", "date", "time")
pal12 <- pal12 %>% transmute(voterid, county = "PAL", date, time)
pal12 <- pal12 %>% separate(date, into = c("year", "month", "day"), sep = "-")
pal12 <- pal12 %>% mutate(year = str_replace(year, "20", ""))
pal12 <- pal12 %>% unite(date, month, day, year, remove = T, sep = "/")
pal12 <- left_join(pal12, loc12, by = "voterid")
pal12 <- pal12 %>% select(voterid, county, location, date, time)
pal12 <- pal12 %>% filter(!is.na(location)) #remove 44 that dont match to in-person early voting locations

path <- "../Data/Parsed/PalmBeachEarlyVotingEVID2016General-parsed.txt"
pal16 <- read_delim(path, delim = "\t", col_types = cols(.default = "c"))
names(pal16) <- c("voterid", "last", "first", "date", "time")
pal16 <- pal16 %>% transmute(voterid, county = "PAL", date, time)
pal16 <- pal16 %>% separate(date, into = c("year", "month", "day"), sep = "-")
pal16 <- pal16 %>% mutate(year = str_replace(year, "20", ""))
pal16 <- pal16 %>% unite(date, month, day, year, remove = T, sep = "/")
pal16 <- left_join(pal16, loc16, by = "voterid")
pal16 <- pal16 %>% select(voterid, county, location, date, time)
pal16 <- pal16 %>% filter(!is.na(location)) #remove 1,409 that dont match to in-person early voting locations


# Bind Counties ------------------------------------------------------------

evid12 <- bind_rows(ala12, bro12, hil12, dad12, ora12, pal12)
evid16 <- bind_rows(ala16, bro16, hil16, dad16, ora16, pal16)

# clean date
evid12 <- evid12 %>% separate(date, into = c("month", "day", "year"), sep = "/")
evid12 <- evid12 %>% mutate(day = str_pad(day, pad = "0", width = 2, side = "left"))
evid12 <- evid12 %>% unite(date, month, day, year, remove = T, sep = "/")
evid12 <- evid12 %>% filter(date != "11/30/12")

evid16 <- evid16 %>% separate(date, into = c("month", "day", "year"), sep = "/")
evid16 <- evid16 %>% mutate(day = str_pad(day, pad = "0", width = 2, side = "left"))
evid16 <- evid16 %>% unite(date, month, day, year, remove = T, sep = "/")
evid16 <- evid16 %>% filter(!date %in% c("11/10/16", "11/14/16", "12/20/16") )

# Store in SQL ------------------------------------------------------------

my_db <- src_sqlite("../Data/evid-db.sqlite3")
dbRemoveTable(my_db$con, "evid12")
dbRemoveTable(my_db$con, "evid16")

copy_to(my_db, evid12, "evid12", temporary = FALSE, indexes = list("voterid"))
copy_to(my_db, evid16, "evid16", temporary = FALSE, indexes = list("voterid"))


