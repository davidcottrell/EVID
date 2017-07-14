library(rvest)
library(tidyverse)
library(stringr)

colspec <- cols(
  RegNum = col_character(),
  Precinct = col_character(),
  VoteDate = col_character()
)


broward <- read_delim("../Data/Parsed/BrowardEarlyVotingEVID2016General-parsed.txt", delim = "\t",  col_types = colspec)

broward <- broward %>% 
  separate(VoteDate, sep = " ", into = c("date", "time")) %>%
  mutate(time = str_replace(time, "Z", ""), 
         Precinct = str_sub(Precinct, 1, 4)) %>%
  left_join(df, by = "Precinct")

broward <- broward %>% 
  rename(voterid = RegNum,
         location = Location,
         precinct = Precinct) %>%
  select(location, date, time, precinct, voterid)

write.table(broward,"../Data/Parsed/BrowardEarlyVotingEVID2016General-parsed.txt", sep = ";", row.names = F)

