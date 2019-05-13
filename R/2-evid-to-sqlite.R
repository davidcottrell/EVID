library(tidyverse)
library(hms)
library(stringr)
library(DBI)


coltypes <- list(
  voterid = col_character(),
  date = col_character(), 
  time = col_character()
)

if (Sys.info()["user"] == "herron") {
  setwd ("/Users/herron/research/EVID/R")
}


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

# Osceola --------------------------------------------------------------

path <- "../Data/Parsed/OsceolaEarlyVotingEVID2012General-parsed.tex"
osc12 <- read_delim(path, delim = ";", col_types = coltypes)
osc12 <- osc12 %>% transmute(voterid, county = "OSC", location, date, time)
osc12 <- osc12 %>% mutate(time = str_pad(time, pad = "0", width = 5, side = "left"))

path <- "../Data/Parsed/OsceolaEarlyVotingEVID2016General-parsed.tex"
osc16 <- read_delim(path, delim = ";", col_types = coltypes)
osc16 <- osc16 %>% transmute(voterid, county  = "OSC", location, date, time)
osc16 <- osc16 %>% mutate(time = str_pad(time, pad = "0", width = 5, side = "left"))

# Hernando --------------------------------------------------------------

path <- "../Data/Parsed/HernandoEarlyVotingEVID2012General-parsed.tex"
her12 <- read_delim(path, delim = ";", col_types = coltypes)
her12 <- her12 %>% transmute(voterid, county = "HER", location, date, time)
her12 <- her12 %>% mutate(time = str_pad(time, pad = "0", width = 5, side = "left"))
her12 <- her12 %>% select(-location) # the location variable for Hernando 2012 is missing
her12 <- left_join(her12, loc12, by = "voterid")
sum(is.na(her12$location))

path <- "../Data/Parsed/HernandoEarlyVotingEVID2016General-parsed.tex"
her16 <- read_delim(path, delim = ";", col_types = coltypes)
her16 <- her16 %>% transmute(voterid, county  = "HER", location, date, time)
her16 <- her16 %>% mutate(time = str_pad(time, pad = "0", width = 5, side = "left"))

# Levy --------------------------------------------------------------

## Warning!  Some voting times are reported as 00:00

path <- "../Data/Parsed/LevyEarlyVotingEVID2012General-parsed.tex"
## Warning: the read reports some warning messages (18476 rows) about QualifiedCertificate field
lev12 <- read_delim(path, delim = ",", col_types = paste(rep("c", times = 18), collapse = ""))
lev12 <- lev12 %>%
  rename(voterid = RegNum) %>%
  mutate(location = "Supervisor of Elections Office",
         county = "LEV")
lev12 <- lev12 %>%
  mutate(time = str_sub(VoteDate, start = 12, end = 16)) 
lev12 <- lev12 %>%
  mutate(date = str_sub(VoteDate, start = 6, end = 10)) %>%
  mutate(date = paste(date, "/12", sep = "")) %>%
  mutate(date = str_replace_all(date, pattern = '-', replacement = '/'))
lev12 <- lev12 %>%
  select(voterid, county, location, date, time)

path <- "../Data/Parsed/LevyEarlyVotingEVID2016General-parsed.tex"
## Warning: the read reports some warning messages (19524 rows) about QualifiedCertificate field
lev16 <- read_delim(path, delim = ",", col_types = paste(rep("c", times = 18), collapse = ""))
lev16 <- lev16 %>%
  rename(voterid = RegNum) %>%
  mutate(location = "Supervisor of Elections Office",
         county = "LEV")
lev16 <- lev16 %>%
  mutate(time = str_sub(VoteDate, start = 12, end = 16)) 
lev16 <- lev16 %>%
  mutate(date = str_sub(VoteDate, start = 6, end = 10)) %>%
  mutate(date = paste(date, "/16", sep = "")) %>%
  mutate(date = str_replace_all(date, pattern = '-', replacement = '/'))
lev16 <- lev16 %>%
  select(voterid, county, location, date, time)

## Drop 2016 Levy county votes with 00:00 time and after early voting period
lev16 <- lev16 %>%
  filter(time != "00:00") %>%
  filter(date != "11/07/16" & date != "11/08/16")

# Bind Counties ------------------------------------------------------------

evid12 <- bind_rows(ala12, bro12, hil12, dad12, ora12, pal12, osc12, her12, lev12)
evid16 <- bind_rows(ala16, bro16, hil16, dad16, ora16, pal16, osc16, her16, lev16)

# clean date
evid12 <- evid12 %>% separate(date, into = c("month", "day", "year"), sep = "/")
evid12 <- evid12 %>% mutate(day = str_pad(day, pad = "0", width = 2, side = "left"))
evid12 <- evid12 %>% unite(date, month, day, year, remove = T, sep = "/")
evid12 <- evid12 %>% filter(date != "11/30/12")

evid16 <- evid16 %>% separate(date, into = c("month", "day", "year"), sep = "/")
evid16 <- evid16 %>% mutate(day = str_pad(day, pad = "0", width = 2, side = "left"))
evid16 <- evid16 %>% unite(date, month, day, year, remove = T, sep = "/")
evid16 <- evid16 %>% filter(!date %in% c("11/10/16", "11/14/16", "12/20/16") )

# drop voters with duplicated ids in 2012 or 2016
dupids12 <- evid12 %>% filter(duplicated(voterid) | duplicated(voterid,fromLast = T)) %>% arrange(voterid) %>% pull(voterid) %>% unique # 5 duplicates from 2012
evid12 <- evid12 %>% filter(!voterid %in% dupids12) # because only 5 duplicates, simply drop them
dupids16 <- evid16 %>% filter(duplicated(voterid) | duplicated(voterid,fromLast = T)) %>% arrange(voterid) %>% pull(voterid) %>% unique # 15 duplicates from 2016
evid16 <- evid16 %>% filter(!voterid %in% dupids16) # because only 15 duplicates, simply drop them

# Store in SQL ------------------------------------------------------------

my_db <- src_sqlite("../Data/evid-db.sqlite3")

if (dbExistsTable(my_db$con, "evid12")){dbRemoveTable(my_db$con, "evid12")}
if (dbExistsTable(my_db$con, "evid16")){dbRemoveTable(my_db$con, "evid16")}

copy_to(my_db, evid12, "evid12", temporary = FALSE, indexes = list("voterid"))
copy_to(my_db, evid16, "evid16", temporary = FALSE, indexes = list("voterid"))


evid <-
  tbl(
    my_db,
    sql(
      "SELECT x.*, e.residencezipcode, e.gender, e.race, e.birthdate, e.registrationdate, e.partyaffiliation, e.gen_08, e.gen_12, e.gen_14, e.gen_16
      FROM (
        SELECT *, 2012 AS year FROM evid12
        UNION ALL
        SELECT *, 2016 AS year FROM evid16
      ) x
      LEFT JOIN (
        SELECT extract.*, IFNULL(h1.gen08,'N') AS gen_08, IFNULL(h2.gen12,'N') AS gen_12, IFNULL(h3.gen14,'N') AS gen_14, IFNULL(h4.gen16,'N') AS gen_16
        FROM (SELECT * FROM extract WHERE voterid IN (SELECT DISTINCT voterid FROM evid12 UNION ALL SELECT DISTINCT voterid FROM evid16)) extract
        LEFT JOIN history08 AS h1 ON extract.voterid = h1.voterid
        LEFT JOIN history12 AS h2 ON extract.voterid = h2.voterid
        LEFT JOIN history14 AS h3 ON extract.voterid = h3.voterid
        LEFT JOIN history16 AS h4 ON extract.voterid = h4.voterid
      ) e
      ON x.voterid = e.voterid" 
    )
  ) %>% collect(n = Inf)

if (dbExistsTable(my_db$con, "evid")){dbRemoveTable(my_db$con, "evid")}
copy_to(my_db, evid, "evid", temporary = FALSE, indexes = list("voterid"))
