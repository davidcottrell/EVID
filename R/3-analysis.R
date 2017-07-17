library(tidyverse)
library(lubridate)
library(stringr)
library(RSQLite)
library(stargazer)

## Set working directory
if (length(grep("herron", Sys.info()["user"]))) {
  ## Michael
  setwd("/Users/herron/research/EVID/R")
} else {
  ## David
  setwd("~/Dropbox/EVID/R")
}

my_db <- src_sqlite("../Data/evid-db.sqlite3")

# EARLY VOTE --------------------------------------------------------------

evid12 <- my_db %>% tbl("evid12") %>% collect(n = Inf) %>% mutate(year = 2012)
evid16 <- my_db %>% tbl("evid16") %>% collect(n = Inf) %>% mutate(year = 2016)
evid <- bind_rows(evid12, evid16)
evid <- evid %>% mutate(datetime = parse_datetime(paste(date, time), "%m/%d/%y %H:%M" ))

make_hour <- function(d) {hour(d) + minute(d)/60}

# remove duplicates
dupids12 <- evid12 %>% filter(duplicated(voterid) | duplicated(voterid,fromLast = T)) %>% arrange(voterid) %>% data.frame %>% .$voterid %>% unique # 4 duplicates from 2012
evid12 <- evid12 %>% filter(!voterid %in% dupids12) # because only 4 duplicates, simply drop them
dupids16 <- evid16 %>% filter(duplicated(voterid) | duplicated(voterid,fromLast = T)) %>% arrange(voterid) %>% data.frame %>% .$voterid %>% unique # 15 duplicates from 2016
evid16 <- evid16 %>% filter(!voterid %in% dupids16) # because only 15 duplicates, simply drop them

evid <- evid %>%
  mutate(hour = hour(datetime),
         time2 = if_else(hour > 2, make_hour(datetime), 24 + make_hour(datetime)),
         day = if_else(hour > 2, format(datetime, "%m/%d"), format(datetime - days(1), "%m/%d")))


evid$hour[evid$hour == 6] <- 7 #Change 6:00 to 7:00 -- these early votes came in seconds before 7:00
hrs <- c(paste0(c(7:11), ":00am"), paste0(c(12, 1:11), ":00pm"), paste0(c(12, 1), ":00am"))
evid$hr <- factor(evid$hour,levels = c(7:23,0,1), labels = hrs )

evidIDs <- unique(evid$voterid)

# VOTER EXTRACT --------------------------------------------------------

extract <- my_db %>% tbl("extract")  %>% 
  select(voterid, gender, race, birthdate, registrationdate, partyaffiliation) %>%
  filter(VoterID %in% evidIDs) %>% collect(n = Inf)

#Recode race
extract <- extract %>% 
  mutate(race=recode(race, `1`="Other",`2`="Asian",`3`="Black",`4`="Hispanic",`5`="White",`6`="Other",`7`="Other",`9`="Other"))


#Recode party affiliation
extract$party <- if_else(!extract$partyaffiliation %in% c("NPA", "DEM", "REP", "IDP"), "OTH", extract$partyaffiliation)


age <- function(dob, age.day, units = "years") {
  calc.age = interval(dob, age.day) / duration(num = 1, units = units)
  return(calc.age)
}


extract <- extract %>%
  mutate(age = age(as.Date(birthdate, format = "%m/%d/%Y"), as.Date("11/08/2016", format = "%m/%d/%Y")),
         agegroup = cut(age, seq(0,120, 5), right = FALSE),
         registered2008 =as.Date(registrationdate, format = "%m/%d/%Y") < as.Date("11/04/2008", format = "%m/%d/%Y")
         )


# MERGE EVID WITH EXTRACT -----------------------------------------------

vte <- evid %>% left_join(extract, by = c("voterid") )



# Plot EVID -------------------------------------------------------------

vte12 <- vte %>% filter(year == 2012)

vte12 <- vte12 %>% filter(!str_detect(location, pattern =  "OTC")) #remove OTC locations
lim <- c(levels(vte12$hr))
vte12 <- vte12 %>% filter(!day %in% c("10/26", "11/04"))
vte12$day <- factor(vte12$day, 
                   labels = c("SAT 10/27", "SUN 10/28", "MON 10/29", "TUE 10/30", "WED 10/31", "THU 11/01", "FRI 11/02", "SAT 11/03"),
                   levels = c("10/27", "10/28", "10/29", "10/30", "10/31", "11/01", "11/02", "11/03") )






xlabs <- c(paste0(c(7:11), ":00am"), paste0(c(12, 1:11), ":00pm"), paste0(c(12,1), ":00am"))
hst <- vte12 %>% filter(location %in% "Fred B. Karl County Center", day == "SAT 11/03", !is.na(race))
hst$Race3 <- if_else(hst$race == "White", "White", "Non-White")
hst$Race3 <- factor(hst$Race3, levels = c("White", "Non-White"))
hst$time2[hst$time2 <= 7] <- 7 + .000001
clse <- hst %>% group_by(day) %>% summarize( close = max(time2))
plot2save <- ggplot(hst , aes(time2, fill = Race3)) + 
  geom_histogram(binwidth = 10/60, colour = "black", boundary = 0) + 
  geom_vline(xintercept = 19, colour = "red") +
  geom_vline(data = clse, aes(xintercept = close), colour = "red", linetype = "dashed") +
  scale_fill_manual(values = c("grey", "green"), name = "Race") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=.5)) +
  scale_x_continuous(breaks = 7:25, labels = xlabs, limits = c(7,25), name = "") +
  scale_y_continuous(breaks = seq(0,45,10), limits = c(0,45), name = "Count\n") +
  theme(legend.position = c(.8,.7)) +
  theme(axis.text = element_text(size = 12)) 
ggsave (plot2save, filename = "../Plots/example00.pdf", height = 3, width = 11)


xlabs <- c(paste0(c(7:11), ":00am"), paste0(c(12, 1:11), ":00pm"), paste0(c(12,1), ":00am"))
hst <- vte12 %>% filter(location %in% "West Kendall Regional Library", day == "SAT 11/03", !is.na(race))
hst$Race3 <- if_else(hst$race == "White", "White", "Non-White")
hst$Race3 <- factor(hst$Race3, levels = c("White", "Non-White"))
hst$time2[hst$time2 <= 7] <- 7 + .000001
clse <- hst %>% group_by(day) %>% summarize( close = max(time2))
plot2save <- ggplot(hst , aes(time2, fill = Race3)) + 
  geom_histogram(binwidth = 10/60, colour = "black", boundary = 0) + 
  geom_vline(xintercept = 19, colour = "red") +
  geom_vline(data = clse, aes(xintercept = close), colour = "red", linetype = "dashed") +
  scale_fill_manual(values = c("grey", "green"), name = "Race") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=.5)) +
  scale_x_continuous(breaks = 7:25, labels = xlabs, limits = c(7,25), name = "") +
  scale_y_continuous(breaks = seq(0,45,10), limits = c(0,45), name = "Count\n") +
  theme(axis.text = element_text(size = 12)) +
  guides(fill = F)
ggsave (plot2save, filename = "../Plots/example01.pdf", height = 3, width = 11)


listofcount <- vte12 %>%  distinct(county, location) %>% data.frame()

for (i in 1:nrow(listofcount)){
  pdf(paste0("../Plots/distributions/", paste0("figure", i, "-", listofcount[i,1], ".pdf")), width = 9, height = 16)
  locdf <- vte12 %>% filter(county == listofcount[i,1], location == listofcount[i,2]) %>% arrange(time2) %>% mutate(count = row_number())
  forplot <-ggplot(locdf, aes(time2, count)) +
    geom_point(size = .1) +
    scale_x_continuous(name = "hour", breaks = c(7:25), labels = xlabs) +
    theme_bw() +
    facet_grid(day~.) +
    ggtitle(paste0(listofcount[i,2], ", ", listofcount[i,1])) +
    theme(panel.grid.major.x = element_line(colour = "grey"))
  print(forplot)
  dev.off()
}


forplot <- vte12 %>%  group_by(hr, day, location) %>% count() %>% count() %>% complete(hr, day, fill = list(nn = 0))

plot2save <- ggplot(forplot, aes(hr, nn, colour = day, group = day)) + geom_line(position = position_nudge(.5)) + theme_bw() + scale_colour_grey(start = 0.8, end = 0.2, name = "Day") +
  geom_vline(xintercept = 13, colour ="red") + geom_point( position = position_nudge(.5)) + xlab("") + xlim(lim) +
  scale_y_continuous(breaks = seq(0, 100, 5), name = "") +
  #ggtitle("Number of locations where votes are being cast") +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=.5),
        legend.text = element_text(size = 8),
        legend.key.height = unit(.8,"line"))
ggsave (plot2save, filename = "../Plots/number_of_locations.pdf", height = 4, width = 7)


plot2save <- ggplot(vte12, aes(x=hr)) +  geom_bar(stat="count", position = position_nudge(.5)) + geom_vline(xintercept = 13, colour ="red") +
  scale_y_continuous(breaks = seq(0, 100000, 10000), labels = seq(0, 100, 10), name = "Count in thousands\n") + theme_bw() +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=.5)) + xlab("")  + xlim(lim) 
#ggtitle("Distribution of Voters by Hour Voted")
ggsave(plot2save, filename = "../Plots/histogram_by_hour.pdf", height = 4, width = 7)

library(MultinomialCI)
cis <- function(x, i){data.frame(multinomialCI(x,.05))[,i]}
plt1 <- vte12 %>% filter(!is.na(race), race != "Other") %>% group_by(hr, race) %>% count() %>% mutate(pct = n/sum(n), low = cis(n,1), high = cis(n,2))

rc <- c("White", "Black", "Hispanic", "Asian")
plt1$race <- factor(plt1$race, rc)

plot2save <- ggplot(plt1, aes(hr, pct, colour = race, group = race, ymin = low, ymax = high)) + 
  geom_line(position = position_nudge(.5)) + geom_point(position = position_nudge(.5)) + 
  #geom_errorbar(width = 0)  + 
  theme_bw() + ylab("Percent") +  xlab("") +
  geom_vline(xintercept = 13, colour ="red") +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=.5)) +
  scale_y_continuous(breaks = seq(0, 1, .05), labels = seq(0, 100, 5), name = "Percent of total voters\n") +
  scale_x_discrete(limits = c(levels(plt1$hr), "1:00am") ) +
  #ggtitle("Racial Composition of Early Voters by Hour") +
  scale_colour_discrete(name = "Race")  
ggsave(plot2save, filename = "../Plots/racial_composition.pdf", height = 4, width = 7)


plt1.3 <- vte12 %>% group_by(hr,race) %>% summarize(pct = mean(party == "DEM"), n=n()) %>% mutate(party = "DEM") %>% filter(n>30, !is.na(race), race != "Other")
rc <- c("White", "Black", "Hispanic", "Asian", "All")
plt1.3$race <- factor(plt1.3$race, rc)
plt1.35 <- vte12 %>% group_by(hr) %>% summarize(pct = mean(party == "DEM", na.rm = T)) %>% mutate(race = "All", party = "DEM")
rc <- c("White", "Black", "Hispanic", "Asian", "All")
plt1.35$race <- factor(plt1.35$race, rc)
pt <-bind_rows(plt1.3, plt1.35)
plot2save <- ggplot(pt, aes(hr, pct, group = interaction(party, race), colour = race)) + 
  geom_line(position = position_nudge(.5)) + geom_point(position = position_nudge(.5)) +
  theme_bw() + ylab("Percent") +  xlab("") +
  geom_vline(xintercept = 13, colour ="red") +
  geom_hline(yintercept = .5, colour ="grey") +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=.5)) +
  scale_y_continuous(breaks = seq(0, 1, .05), labels = seq(0, 100, 5), name = "Percent Democratic\n", limits = c(.3,1)) +
  scale_x_discrete(limits = lim) +
  scale_colour_manual(values = c("#F8766D", "#7CAE00", "#00BFC4", "#C77CFF", "black"),  name = "Race")  
#ggtitle("Partisan Composition of Early Voters by Hour (Whites)")
ggsave(plot2save, file = "../Plots/partisan_composition_by_race.pdf", height = 4, width = 7)

###EVID 2016
vte16 <- vte %>% filter(year == 2016)

vte16$day <- factor(vte16$day, labels = c("MON 10/24",  "TUE 10/25", "WED 10/26", "THU 10/27", "FRI 10/28", "SAT 10/29", "SUN 10/30", "MON 10/31", "TUE 11/01", "WED 11/02", "THU 11/03", "FRI 11/04", "SAT 11/05", "SUN 11/06"))


lim <- c(levels(vte16$hr), "1:00am")

forplot <- vte16 %>% group_by(hr, day, location) %>% count() %>% count() %>% complete(hr, day, fill = list(nn = 0))
plot2save <-  ggplot(forplot, aes(hr, nn, colour = day, group = day)) + geom_line(position = position_nudge(.5)) + theme_bw() + scale_colour_grey(start = 0.8, end = 0.2, name = "Day") +
  geom_vline(xintercept = 13, colour ="red") + geom_point( position = position_nudge(.5)) + xlab("") + xlim(lim) +
  scale_y_continuous(breaks = seq(0, 125, 5), name = "") +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=.5),
        legend.text = element_text(size = 8),
        legend.key.height = unit(.8,"line"))
ggsave (plot2save, file = "../Plots/number_of_locations_2016.pdf", height = 4, width = 7)

plot2save <- ggplot(filter(vte, race != "Asian",race != "Other"), aes(x=hr)) +  geom_bar(stat="count", position = position_nudge(.5)) + facet_grid(race~year) + geom_vline(xintercept = 13, colour ="red") +
  scale_y_continuous(breaks = seq(0, 100000, 10000), labels = seq(0, 100, 10), name = "Count in thousands\n") + theme_bw() +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=.5)) + xlab("") +
  scale_x_discrete(breaks = hrs,limits = hrs)  
ggsave (plot2save, file = "../Plots/histogram_by_hour_by_race_2012_2016.pdf", height = 4, width = 8)




# VOTER HISTORY --------------------------------------------------------

history <- my_db %>% tbl("history")
history <- history %>% filter(VoterID %in% evidIDs) %>% collect(n = Inf)
history <- history %>% filter(ElectionDate %in% c("11/04/2008", "11/06/2012", "11/08/2016"))
history <- history %>% filter(ElectionType == "GEN")

# 901 duplicates 
# dups <- duplicated(history[,c(2,3)]) | duplicated(history[,c(2,3)], fromLast = T)
# history[dups,]

#Create hierarchy and remove duplicates
codes <- c("E","Y", "A", "Z", "F", "B", "P", "N")
history$HistoryCode <- factor(history$HistoryCode, codes)
history <- history %>%
  arrange(VoterID,ElectionDate,HistoryCode) %>%
  distinct(VoterID, ElectionDate, .keep_all = T)
history$HistoryCode <- as.character(history$HistoryCode)

#Reshape data
history <- history %>%
  select(VoterID,ElectionDate, HistoryCode) %>%
  mutate(ElectionDate = paste0("GEN_", str_sub(ElectionDate, 7))) %>% 
  spread(ElectionDate,HistoryCode)

#Did not vote (N)
history <- replace_na(history, list(GEN_2008 = "N", GEN_2012 = "N", GEN_2016 = "N" ))

# create variables
transform <- function(col) {if_else(col == "N", 0, 1)}
transform2 <- function(col) {if_else(col == "E", 1, 0)}
transform3 <- function(col) {if_else(col == "A", 1, 0)}
history <- history %>% 
  mutate(
  voted_08 = transform(GEN_2008),
  voted_16 = transform(GEN_2016),
  voted_early_16 = transform2(GEN_2016),
  voted_absentee_16 = transform3(GEN_2016)
  )

#any more duplicates?
any(duplicated(history$VoterID))


#############

#############

#############



vte2 <- vte12 %>% left_join(history, by = c("voterid" = "VoterID") )
groups <- c(18, 30, 40, 50, 60, 70, 120)
vte3 <- vte2 %>% filter(gender != "U") %>% group_by(location, day) %>% mutate(over = max(time2) >= 19.5, agegroup = cut(age, groups, right = FALSE)) %>% filter(time2 < 19, party %in% c("DEM", "REP", "IDP", "NPA"))
rc <- c("White", "Black", "Hispanic", "Asian")
vte3$race <- factor(vte3$race, rc)
mn <- glm(voted_16 ~ hr + over + hr:over +  gender + race + agegroup + party + voted_08, data = vte3, family = "binomial")

summary(mn)

orderedvar <- names(mn$coefficients)[c(1:12, 26:36, 13:25)]
t <- stargazer(mn,               
               title = "Logit regression predicting voter turnout in 2016",
               label = "tab:reg",
               dep.var.labels =  c("Voted in 2016"),               
               omit.stat=c("LL","ser","f"),
               type = "latex", 
               order = c(1:12, 26:36, 13:25),
               notes = "",
               single.row=TRUE,
               covariate.labels = c(
                 "8:00am",
                 "9:00am",
                 "10:00am",
                 "11:00am",
                 "12:00pm",
                 "1:00pm",
                 "2:00pm",
                 "3:00pm",
                 "4:00pm",
                 "5:00pm",
                 "6:00pm",
                 "Over",
                 "8:00am \\& Over",
                 "9:00am \\& Over",
                 "10:00am \\& Over",
                 "11:00am \\& Over",
                 "12:00pm \\& Over",
                 "1:00pm \\& Over",
                 "2:00pm \\& Over",
                 "3:00pm \\& Over",
                 "4:00pm \\& Over",
                 "5:00pm \\& Over",
                 "6:00pm \\& Over",
                 "Gender: male",
                 "Race: Black",
                 "Race: Hispanic",
                 "Race: Asian",
                 "Age group: 30-39",
                 "Age group: 40-49",
                 "Age group: 50-59",
                 "Age group: 60-69",
                 "Age group: 70+",
                 "Party: independent",
                 "Party: none",
                 "Party: Republican",
                 "Voted08: yes"),
               no.space = TRUE,
               font.size = "scriptsize")

cat(t, file = "../plots/table_out.tex", sep = "\n")



newdata <-  data.frame(
  hr = factor(levels(vte3$hr), levels(vte3$hr)),
  location = c("Bloomingdale Regional Public Library"),
  day = "SAT 10/27",
  gender = "M",
  race = "Black",
  age = mean(vte$age, na.rm = T),
  agegroup = "[50,60)",
  party = "DEM",
  voted_08 = 0,
  over = TRUE
)

p2over <- predict(mn, newdata = newdata[as.integer(newdata$hr)<13,], type = "link", se = TRUE)

newdata <-  data.frame(
  hr = factor(levels(vte3$hr), levels(vte3$hr)),
  location = c("Bloomingdale Regional Public Library"),
  day = "SAT 10/27",
  gender = "M",
  race = "Black",
  age = mean(vte$age, na.rm = T),
  agegroup = "[50,60)",
  party = "DEM",
  voted_08 = 0,
  over = FALSE
)

p2under <- predict(mn, newdata = newdata[as.integer(newdata$hr)<13,], type = "link", se = TRUE)


plt3over <- data.frame(hr =newdata$hr[as.integer(newdata$hr)<13], 
                       pct = plogis(p2over$fit), 
                       low = plogis(p2over$fit - 1.96*p2over$se.fit), 
                       high = plogis(p2over$fit + 1.96*p2over$se.fit),
                       var = "last voter checked-in after 7:30pm", stringsAsFactors = F)

plt3under <- data.frame(hr =newdata$hr[as.integer(newdata$hr)<13], 
                        pct = plogis(p2under$fit), 
                        low = plogis(p2under$fit - 1.96*p2under$se.fit), 
                        high = plogis(p2under$fit + 1.96*p2under$se.fit),
                        var = "last voter checked-in before 7:30pm", stringsAsFactors = F)

plt3overunder <- bind_rows(plt3over, plt3under)


plot2save <- ggplot(plt3overunder, aes(hr, pct, ymin = low, ymax = high, colour = var)) + 
  geom_point(position = position_nudge(.5)) + geom_errorbar(width = 0,position = position_nudge(.5))  + theme_bw() +  xlab("") +
  geom_vline(xintercept = 13, colour ="red") +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=.5)) +
  scale_y_continuous(breaks = seq(0, 1, .02), labels = seq(0, 100, 2),limits = c(.75, .9), name = "Predicted probability of voting (%)\n") +
  scale_x_discrete(limits = levels(plt3overunder$hr)[1:14], name = "") +
  scale_color_manual(values = c("grey50", "black"), name = "Among polling locations where...") +
  ggtitle("") +
  theme(legend.position = c(.3,.2))
ggsave(plot2save, filename = "../Plots/probability_of_voting_in_2016_over_under.pdf", height = 5, width = 7)
