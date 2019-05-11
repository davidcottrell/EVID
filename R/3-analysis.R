# !diagnostics off
library(tidyverse)
library(lubridate)
library(stringr)
library(RSQLite)
library(stargazer)
library(dbplyr)
library(xtable)

## Set working directory
if (length(grep("herron", Sys.info()["user"]))) {
  ## Michael
  setwd("/Users/herron/research/EVID/R")
} else {
  ## David
  setwd("~/Dropbox/EVID/R")
}

my_db <- src_sqlite("../Data/evid-db.sqlite3")

# CEARLY VOTE --------------------------------------------------------------

# Use evid table
evid <- my_db %>% tbl("evid") %>% collect(Inf)

# Make time variables
evid <- evid %>% mutate(datetime = parse_datetime(paste(date, time), "%m/%d/%y %H:%M" ))

make_hour <- function(d) {hour(d) + minute(d)/60}

evid <- evid %>%
  mutate(hour = hour(datetime),
         time2 = if_else(hour > 2, make_hour(datetime), 24 + make_hour(datetime)),
         day = if_else(hour > 2, format(datetime, "%m/%d"), format(datetime - days(1), "%m/%d")))

# remove 10 locations where only one or two votes were cast, likely due to coding error
evid %>%  group_by(county, location, year, day) %>% summarize(totalvotes = n()) %>% arrange(totalvotes) %>% print(n = 20)
evid <- evid %>%  group_by(county, location, year, day) %>% filter(n() > 2) %>% ungroup()

# remove the 41 Winter Park voters who voted on 11/4/2012 due to bomb scare on the previous day.
evid <- evid %>% filter(!(county == "ORA" & location == "Winter Park Library" & year == 2012 & day == "11/04"))

evid$hour[evid$hour == 6] <- 7 #Change 6:00 to 7:00 -- these early votes came in seconds before 7:00
hrs <- c(paste0(c(7:11), ":00am"), paste0(c(12, 1:11), ":00pm"), paste0(c(12, 1), ":00am"))
evid$hr <- factor(evid$hour,levels = c(7:23,0,1), labels = hrs )


#Recode race
evid <- evid %>% 
  mutate(race=recode(race, `1`="Other",`2`="Asian",`3`="Black",`4`="Hispanic",`5`="White",`6`="Other",`7`="Other",`9`="Other"))


#Recode party affiliation
evid$party <- if_else(!evid$partyaffiliation %in% c("NPA", "DEM", "REP", "IDP"), "OTH", evid$partyaffiliation)

#Create age and age group variables
age <- function(dob, age.day, units = "years") {
  calc.age = interval(dob, age.day) / duration(num = 1, units = units)
  return(calc.age)
}


evid <- evid %>%
  mutate(
    age = age(as.Date(birthdate, format = "%m/%d/%Y"), as.Date("11/08/2016", format = "%m/%d/%Y")),
    agegroup = cut(age, seq(0,120, 5), right = FALSE)
    )

#Create variable for whether voter was registered before 2008
evid <- evid %>% mutate(registered2008 = as.Date(registrationdate, format = "%m/%d/%Y") < as.Date("11/04/2008", format = "%m/%d/%Y"))


# create variables
voted <- function(col) {if_else(col == "N", 0, 1)}
votedearly <- function(col) {if_else(col == "E", 1, 0)}
votedabsentee <- function(col) {if_else(col == "A", 1, 0)}
evid <- evid %>% 
  mutate(
    voted_08 = voted(gen_08),
    voted_14 = voted(gen_14),
    voted_16 = voted(gen_16),
    voted_early_16 = votedearly(gen_16),
    voted_absentee_16 = votedabsentee(gen_16)
  )

# 942,194 early voters in 2012 and 1,687,304 early voters in 2016
cat ("EVID counts before dropping midnight votes\n")
evid %>% group_by(year) %>% count

# 40,455 in 2012 and 969 in 2016 could not be matched to the voter extract.  
evid %>% filter(is.na(gender) & is.na(race) & is.na(birthdate)) %>% group_by(year)  %>% count

evid <- evid %>% filter(time != "00:00") # There are spikes at midnight that dont make sense.  So remove 115 with suspicious midnight time

# Total counts after dropping suspicious midnight votes
cat ("EVID counts after dropping midnight votes\n")
evid %>% group_by(year) %>% count

## Make evid count summary table for our six counties

table2use <- 
    rbind(table(dplyr::select(evid %>% filter(year == 2012), county)),
          table(dplyr::select(evid %>% filter(year == 2016), county))
          )
colnames(table2use)[match("ALA", colnames(table2use))] <- "Alachua"
colnames(table2use)[match("BRO", colnames(table2use))] <- "Broward"
colnames(table2use)[match("DAD", colnames(table2use))] <- "Miami-Dade"
colnames(table2use)[match("HIL", colnames(table2use))] <- "Hillsborough"
colnames(table2use)[match("ORA", colnames(table2use))] <- "Orange"
colnames(table2use)[match("PAL", colnames(table2use))] <- "Palm Beach"
colnames(table2use)[match("OSC", colnames(table2use))] <- "Osceola"
table2use <- table2use[, order(colnames(table2use))]

Total <- apply(table2use, 1, sum)
table2use <- cbind(table2use, Total)

table2print <- xtable(t(table2use),
                      row.names =  TRUE,
                      caption = "EVID check-ins by county, 2012 and 2016 General Elections",
                      label = "tab:evidcounts",
                      hline.after = TRUE,
                      align = "lrr",
                      type = "latex")
names(table2print) <- c("2012", "2016")

latex2print <- print(table2print,format.args = list(big.mark = ","), caption.placement = "top")

cat(latex2print,
    file = "../Paper/county_evid_counts.tex", sep="\n")


# Plot EVID -------------------------------------------------------------


evid12 <- evid %>% filter(year == 2012)

unique(evid12$county)
length(unique(evid12$location))

lim <- c(levels(evid12$hr))
evid12$day <- factor(evid12$day, 
                   labels = c("SAT 10/27", "SUN 10/28", "MON 10/29", "TUE 10/30", "WED 10/31", "THU 11/01", "FRI 11/02", "SAT 11/03"),
                   levels = c("10/27", "10/28", "10/29", "10/30", "10/31", "11/01", "11/02", "11/03") )



xlabs <- c(paste0(c(7:11), ":00am"), paste0(c(12, 1:11), ":00pm"), paste0(c(12,1), ":00am"))
hst <- evid12 %>% filter(location %in% "Fred B. Karl County Center", day == "SAT 11/03", !is.na(race))
hst$race2 <- if_else(hst$race == "White", "White", "Non-white")
hst$race2 <- factor(hst$race2, levels = c("White", "Non-white"))
hst$time2[hst$time2 <= 7] <- 7 + .000001
clse <- hst %>% group_by(day) %>% summarize( close = max(time2))
plot2save <- ggplot(hst , aes(time2, fill = race2)) + 
  geom_histogram(binwidth = 10/60, colour = "black", boundary = 0) + 
  geom_vline(xintercept = 19, colour = "black", size = 1.25) +
  #geom_vline(data = clse, aes(xintercept = close), colour = "red", linetype = "dashed") +
    # scale_fill_manual(values = c("grey", "white"), name = "Race") +
    scale_fill_manual(values = c("white", "grey"), name = "Race") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=.5)) +
  scale_x_continuous(breaks = 7:25, labels = xlabs, limits = c(7,25), name = "") +
  scale_y_continuous(breaks = seq(0,45,10), limits = c(0,45), name = "Count\n") +
  theme(legend.position = c(.8,.7)) +
  theme(axis.text = element_text(size = 12)) 
ggsave (plot2save, filename = "../Plots/example00.pdf", height = 3, width = 11)


xlabs <- c(paste0(c(7:11), ":00am"), paste0(c(12, 1:11), ":00pm"), paste0(c(12,1), ":00am"))
hst <- evid12 %>% filter(location %in% "West Kendall Regional Library", day == "SAT 11/03", !is.na(race))
hst$race2 <- if_else(hst$race == "White", "White", "Non-White")
hst$race2 <- factor(hst$race2, levels = c("White", "Non-White"))
hst$time2[hst$time2 <= 7] <- 7 + .000001
clse <- hst %>% group_by(day) %>% summarize( close = max(time2))
plot2save <- ggplot(hst , aes(time2, fill = race2)) + 
  geom_histogram(binwidth = 10/60, colour = "black", boundary = 0) + 
  geom_vline(xintercept = 19, colour = "black", size = 1.25) +
  #geom_vline(data = clse, aes(xintercept = close), colour = "red", linetype = "dashed") +
  #scale_fill_manual(values = c("grey", "white"), name = "Race") +
  scale_fill_manual(values = c("white", "grey"), name = "Race") +
  theme_bw() +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=.5)) +
  scale_x_continuous(breaks = 7:25, labels = xlabs, limits = c(7,25), name = "") +
  scale_y_continuous(breaks = seq(0,45,10), limits = c(0,45), name = "Count\n") +
  theme(axis.text = element_text(size = 12)) +
  guides(fill = F)
ggsave (plot2save, filename = "../Plots/example01.pdf", height = 3, width = 11)
 

listofcount <- evid12 %>%  distinct(county, location, day) %>% data.frame()

rundist <- "no"
if (rundist == "yes"){
  for (i in 1:nrow(listofcount)){
    pdf(paste0("../Plots/distributions/", paste0("figure", i, "-", listofcount[i,1], ".pdf")), width = 11, height = 5)
    locdf <- evid12 %>% filter(county == listofcount[i,1], location == listofcount[i,2], day == listofcount[i,3]) %>% arrange(time2) %>% mutate(count = row_number(), lagdiff = time2 - lag(time2), smoothdiff = roll_meanr(lagdiff, n = 50, na.rm = T))
    locdf <- locdf %>% mutate(lagdiff = if_else(is.na(lagdiff), min(time2), lagdiff),  smoothdiff = if_else(is.na(smoothdiff),lagdiff, smoothdiff ), est = cumsum(smoothdiff))
    forplot <-ggplot(locdf, aes(time2, count)) +
      geom_point(size = .1) +
      geom_point(aes(x = est), size = .01, col = "red") +
      scale_x_continuous(name = "hour", breaks = c(7:25), labels = xlabs, limits = c(6,26)) +
      theme_bw() +
      #facet_grid(day~.) +
      ggtitle(paste0(listofcount[i,2], ", ", listofcount[i,1], " (", listofcount[i,3], ")")) +
      theme(panel.grid.major.x = element_line(colour = "grey"))
    print(forplot)
    dev.off()
  }
}

# 
# evid12 %>% 
#   arrange(county, location, day, time2) %>%
#   mutate(diff = if_else( time2 > 19, time2 - lag(time2, n = 3), 0) ) %>%
#   arrange(county, location, day) %>%
#   summarise(open = min(time2), close = max(time2), maxdiff = max(diff))

forplot <- evid12 %>%  distinct(hr, day, location) %>% count(hr, day) %>% complete(hr, day, fill = list(n = 0))

plot2save <- ggplot(forplot, aes(hr, n, colour = day, group = day)) + geom_line(position = position_nudge(.5)) + theme_bw() + scale_colour_grey(start = 0.8, end = 0.2, name = "Day") + 
  geom_vline(xintercept = 13, colour = "black", size = 1.25) + geom_point( position = position_nudge(.5)) + xlab("") + xlim(lim) +
  scale_y_continuous(breaks = seq(0, 100, 5), name = "Count") +
  #ggtitle("Number of locations where votes are being cast") +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=.5),
        legend.text = element_text(size = 8),
        legend.key.height = unit(.8,"line"))
ggsave (plot2save, filename = "../Plots/number_of_locations.pdf", height = 4, width = 7)


plot2save <- ggplot(evid12, aes(x=hr)) +  geom_bar(stat="count", position = position_nudge(.5), fill = "lightgrey", colour = "black") + geom_vline(xintercept = 13, colour = "black", size = 1.25) +
  scale_y_continuous(breaks = seq(0, 100000, 10000), labels = seq(0, 100, 10), name = "Count in thousands\n") + theme_bw() +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=.5)) + xlab("")  + xlim(lim) 
#ggtitle("Distribution of Voters by Hour Voted")
ggsave(plot2save, filename = "../Plots/histogram_by_hour.pdf", height = 4, width = 7)

library(MultinomialCI)
cis <- function(x, i){data.frame(multinomialCI(x,.05))[,i]}
plt1 <- evid12 %>% filter(!is.na(race), race != "Other") %>% count(hr, race) %>% group_by(hr) %>% mutate(pct = n/sum(n), low = cis(n,1), high = cis(n,2))

rc <- c("White", "Black", "Hispanic", "Asian")
plt1$race <- factor(plt1$race, rc)

plot2save <- ggplot(plt1, aes(hr, pct, shape = race, group = race, ymin = low, ymax = high)) + 
  geom_line(position = position_nudge(.5), colour = "grey60") + geom_point(position = position_nudge(.5)) + 
  #geom_errorbar(width = 0)  + 
  theme_bw() + ylab("Percent") +  xlab("") +
  geom_vline(xintercept = 13, colour ="black", size = 1.25) +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=.5)) +
  scale_y_continuous(breaks = seq(0, 1, .05), labels = seq(0, 100, 5), name = "Percent of total voters\n") +
  scale_x_discrete(limits = c(levels(plt1$hr), "1:00am") ) +
  #ggtitle("Racial Composition of Early Voters by Hour") +
  scale_shape_discrete(name = "Race/ethnicity")  
ggsave(plot2save, filename = "../Plots/racial_composition.pdf", height = 4, width = 7)


plt1.3 <- evid12 %>% group_by(hr,race) %>% summarize(pct = mean(party == "DEM"), n=n()) %>% mutate(party = "DEM") %>% filter(n>30, !is.na(race), race != "Other")
rc <- c("White", "Black", "Hispanic", "Asian", "All")
plt1.3$race <- factor(plt1.3$race, rc)
plt1.35 <- evid12 %>% group_by(hr) %>% summarize(pct = mean(party == "DEM", na.rm = T)) %>% mutate(race = "All", party = "DEM")
rc <- c("White", "Black", "Hispanic", "Asian", "All")
plt1.35$race <- factor(plt1.35$race, rc)
pt <- bind_rows(plt1.3, plt1.35)
plot2save <- ggplot(pt, aes(hr, pct, shape = race, group = race)) + 
  geom_line(position = position_nudge(.5), colour = "grey60") + geom_point(position = position_nudge(.5)) +
  theme_bw() + ylab("Percent") +  xlab("") +
  geom_vline(xintercept = 13, colour ="black", size = 1.25) +
  geom_hline(yintercept = .5, colour ="grey") +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=.5)) +
  scale_y_continuous(breaks = seq(0, 1, .05), labels = seq(0, 100, 5), name = "Percent Democratic\n", limits = c(.3,1)) +
  scale_x_discrete(limits = lim) +
  scale_shape_discrete(name = "Race/ethnicity")  
#ggtitle("Partisan Composition of Early Voters by Hour (Whites)")
ggsave(plot2save, file = "../Plots/partisan_composition_by_race.pdf", height = 4, width = 7)

###EVID 2016
evid16 <- evid %>% filter(year == 2016)

evid16$day <- factor(evid16$day, labels = c("MON 10/24",  "TUE 10/25", "WED 10/26", "THU 10/27", "FRI 10/28", "SAT 10/29", "SUN 10/30", "MON 10/31", "TUE 11/01", "WED 11/02", "THU 11/03", "FRI 11/04", "SAT 11/05", "SUN 11/06"))


lim <- c(levels(evid16$hr), "1:00am")

forplot <- evid16 %>%  distinct(hr, day, location) %>% count(hr, day) %>% complete(hr, day, fill = list(n = 0))
plot2save <-  ggplot(forplot, aes(hr, n, colour = day, group = day)) + geom_line(position = position_nudge(.5)) + theme_bw() + scale_colour_grey(start = 0.8, end = 0.2, name = "Day") +
  geom_vline(xintercept = 13, colour ="black", size = 1.25) + geom_point( position = position_nudge(.5)) + xlab("")  +
  scale_y_continuous(breaks = seq(0, 125, 5), name = "Count") +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=.5),
        legend.text = element_text(size = 8),
        legend.key.height = unit(.8,"line"))
ggsave (plot2save, file = "../Plots/number_of_locations_2016.pdf", height = 4, width = 7)

plot2save <- ggplot(filter(evid, race != "Asian",race != "Other"), aes(x=hr)) +  geom_bar(stat="count", position = position_nudge(.5), colour = "black", fill = "grey") + facet_grid(race~year) + geom_vline(xintercept = 13, colour ="black", size = 1.25) +
  scale_y_continuous(breaks = seq(0, 100000, 10000), labels = seq(0, 100, 10), name = "Count in thousands\n") + theme_bw() +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=.5)) + xlab("") +
  scale_x_discrete(breaks = hrs,limits = hrs)  
ggsave (plot2save, file = "../Plots/histogram_by_hour_by_race_2012_2016.pdf", height = 4, width = 8)



plt1 <- evid16 %>% filter(!is.na(race), race != "Other") %>% count(hr, race) %>% group_by(hr) %>% mutate(pct = n/sum(n), low = cis(n,1), high = cis(n,2))
rc <- c("White", "Black", "Hispanic", "Asian")
plt1$race <- factor(plt1$race, rc)

plot2save <- ggplot(plt1, aes(hr, pct, shape = race, group = race, ymin = low, ymax = high)) + 
  geom_line(position = position_nudge(.5), colour = "grey60") + geom_point(position = position_nudge(.5)) + 
  #geom_errorbar(width = 0)  + 
  theme_bw() + ylab("Percent") +  xlab("") +
  geom_vline(xintercept = 13, colour ="black", size = 1.25) +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=.5)) +
  scale_y_continuous(breaks = seq(0, 1, .05), labels = seq(0, 100, 5), name = "Percent of total voters\n") +
  scale_x_discrete(limits = c(levels(plt1$hr), "1:00am") ) +
  #ggtitle("Racial Composition of Early Voters by Hour") +
  scale_shape_discrete(name = "Race")  
ggsave(plot2save, filename = "../Plots/racial_composition_2016.pdf", height = 4, width = 7)


#############
# TEST
#############

library(MASS)
library(boot)

zipdata <- read_csv("../Data/aff_download/ACS_14_5YR_S1903_with_ann.csv", na = c("-", "**", NA), col_types = cols(.default = "c"))
zipdata <- zipdata %>% transmute(residencezipcode = GEO.id2, logincome = log(as.integer(HC02_EST_VC02)))
evid12  <- left_join(evid12, zipdata, by = "residencezipcode") # evid

sum(is.na(evid12$gender) & is.na(evid12$race) & is.na(evid12$birthdate))
sum(evid12$time2 >= 19)
sum(!evid12$party %in% c("DEM", "REP", "IDP", "NPA"))
sum(!evid12$gender %in% c("U", NA))

groups <- c(18, 30, 40, 50, 60, 70, 120)
vte <- evid12  %>% group_by(location, day) %>% mutate(over = max(time2) >= 19.5, agegroup = cut(age, groups, right = FALSE)) %>% filter(time2 < 19, party %in% c("DEM", "REP", "IDP", "NPA"), gender != "U")

 
vte$race <- factor(vte$race, c("White", "Black", "Hispanic", "Asian"))
vte$party <- factor(vte$party, c("DEM", "REP", "IDP", "NPA"))
vte$hr <- factor(vte$hr, c("7:00am","8:00am","9:00am","10:00am","11:00am","12:00pm","1:00pm","2:00pm","3:00pm","4:00pm","5:00pm","6:00pm"))
vte$gender <- factor(vte$gender, c("F", "M"))
vte$agegroup <- factor(vte$agegroup, c("[18,30)","[30,40)","[40,50)","[50,60)","[60,70)","[70,120)"))
mn1 <- glm(voted_16 ~ county + hr + over + hr:over +  gender + race + agegroup + party + logincome + voted_08, data = vte, family = "binomial")
mn2 <- glm(voted_early_16 ~ county + hr + over + hr:over +  gender + race + agegroup + party + logincome + voted_08, data = vte, family = "binomial")
mn3 <- glm(voted_absentee_16 ~ county + hr + over + hr:over +  gender + race + agegroup + party + logincome + voted_08, data = vte, family = "binomial")
mn4 <- glm(voted_14 ~ county + hr + over + hr:over +  gender + race + agegroup + party + logincome + voted_08, data = vte, family = "binomial")


####################
## Make ATE table
####################

marginfx <- function(fit, sims, samplesize){
  df <- fit$model
  betas <- mvrnorm(mu = coef(fit), Sigma =vcov(fit), n = sims)
  # Calculate predicted probabilities manually
  ivs <- fit$formula
  hrs <- levels(df$hr)
  ybar <- lo <- hi <- rep(NA,length(hrs))
  j <- sample(1:nrow(df), samplesize, replace = FALSE)
  for (i in 1:length(hrs)){
    t1 <- model.matrix( ivs, data = mutate(df[j,], over = TRUE,  hr = factor(hrs[i],levels = hrs)))
    t0 <- model.matrix( ivs, data = mutate(df[j,], over = FALSE, hr = factor(hrs[i],levels = hrs)))
    nms <- colnames(betas)
    yhat1 <- plogis(tcrossprod(t1[,nms], betas))
    yhat0 <- plogis(tcrossprod(t0[,nms], betas))
    ate <- colMeans(yhat1 - yhat0) #  Pr(voted16 | no.line) - Pr(voted16 | line)
    ybar[i] <- mean(ate)*100
    lo[i] <- quantile(ate, probs = .025)*100
    hi[i] <- quantile(ate, probs = .975)*100
  }
  return(data.frame(hrs,ybar, lo, hi))
}


marg1 <- marginfx(mn1, 500, 10000)
margfx <- ggplot(marg1, aes(hrs, ybar, ymin = lo, ymax = hi)) + 
  geom_point(position = position_nudge(.5)) + geom_errorbar(width = 0,position = position_nudge(.5))  + theme_bw() +  xlab("") +
  geom_vline(xintercept = 13, colour ="black", size = 1.25) +
  geom_hline(yintercept = 0) +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=.5)) +
  scale_y_continuous(breaks = seq(-7, 2, .5), limits = c(-7, 2), name = "Average treatment effect (%)") +
  scale_x_discrete(limits = 
                     c("7:00am","8:00am","9:00am","10:00am","11:00am","12:00pm",
                       "1:00pm","2:00pm","3:00pm","4:00pm","5:00pm","6:00pm", "7:00pm")) +
  #ggtitle("Voted in 2016") +
  scale_color_manual(values = c("grey50", "black"), name = "Among polling locations where...") 
ggsave(margfx, filename = "../Plots/margfx_2016_vote.pdf", height = 5, width = 7)

marg2 <- marginfx(mn2, 500, 10000)
margfx2 <- ggplot(marg2, aes(hrs, ybar, ymin = lo, ymax = hi)) + 
  geom_point(position = position_nudge(.5)) + geom_errorbar(width = 0,position = position_nudge(.5))  + theme_bw() +  xlab("") +
  geom_vline(xintercept = 13, colour ="black", size = 1.25) +
  geom_hline(yintercept = 0) +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=.5)) +
  scale_y_continuous(breaks = seq(-7, 2, .5),limits = c(-7, 2), name = "Average treatment effect (%)") +
  scale_x_discrete(limits = 
                     c("7:00am","8:00am","9:00am","10:00am","11:00am","12:00pm",
                       "1:00pm","2:00pm","3:00pm","4:00pm","5:00pm","6:00pm", "7:00pm")) +
  #ggtitle("Voted early in 2016") +
  scale_color_manual(values = c("grey50", "black"), name = "Among polling locations where...")
ggsave(margfx2, filename = "../Plots/margfx_2016_vote_early.pdf", height = 5, width = 7)

marg3 <- marginfx(mn3, 500, 10000)
margfx3 <- ggplot(marg3, aes(hrs, ybar, ymin = lo, ymax = hi)) + 
  geom_point(position = position_nudge(.5)) + geom_errorbar(width = 0,position = position_nudge(.5))  + theme_bw() +  xlab("") +
  geom_vline(xintercept = 13, colour ="black", size = 1.25) +
  geom_hline(yintercept = 0) +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=.5)) +
  scale_y_continuous(breaks = seq(-7, 2, .5),limits = c(-7, 2), name = "Average treatment effect (%)") +
  scale_x_discrete(limits = 
                     c("7:00am","8:00am","9:00am","10:00am","11:00am","12:00pm",
                       "1:00pm","2:00pm","3:00pm","4:00pm","5:00pm","6:00pm", "7:00pm")) +
  #ggtitle("Voted absentee in 2016") +
  scale_color_manual(values = c("grey50", "black"), name = "Among polling locations where...")
ggsave(margfx3, filename = "../Plots/margfx_2016_vote_abesentee.pdf", height = 5, width = 7)

marg4 <- marginfx(mn4, 500, 10000)
margfx4 <- ggplot(marg4, aes(hrs, ybar, ymin = lo, ymax = hi)) + 
  geom_point(position = position_nudge(.5)) + geom_errorbar(width = 0,position = position_nudge(.5))  + theme_bw() +  xlab("") +
  geom_vline(xintercept = 13, colour ="black", size = 1.25) +
  geom_hline(yintercept = 0) +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=.5)) +
  scale_y_continuous(breaks = seq(-7, 2, .5), limits = c(-7, 2), name = "Average treatment effect (%)") +
  scale_x_discrete(limits = 
                     c("7:00am","8:00am","9:00am","10:00am","11:00am","12:00pm",
                       "1:00pm","2:00pm","3:00pm","4:00pm","5:00pm","6:00pm", "7:00pm")) +
  #ggtitle("Voted in 2014") +
  scale_color_manual(values = c("grey50", "black"), name = "Among polling locations where...")
ggsave(margfx4, filename = "../Plots/margfx_2014_vote.pdf", height = 5, width = 7)


orderedvar <- names(mn1$coefficients)[-1][c(1:12, 27:37, 13:26)]
# t <- stargazer(mn1, mn2, mn3, mn4,               
t <- stargazer(mn1, mn2, mn4,                              
               title = "Logit regression predicting voter turnout in 2016",
               label = "tab:reg",
               ##                   dep.var.labels =  c("Voted in 2016", "Voted early in 2016", "Voted absentee in 2016", "Voted in 2014"),               
               dep.var.labels =  c("Voted in 2016", "Voted early in 2016", "Voted in 2014"),                              
               omit.stat=c("LL","ser","f"),
               type = "latex",
               order = c(1:12, 27:37, 13:26),
               notes = "",
               single.row=TRUE,
               omit = "county",
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
                 "Party: Republican",
                 "Party: Independent",
                 "Party: none",
                 "Log median income",
                 "Voted08: yes"),
               no.space = TRUE,
               font.size = "scriptsize")

row2change <- match(TRUE, grepl("\\{\\}", t))
t[row2change] <- gsub("\\{\\}", "\\{County fixed effects not displayed.\\}", t[row2change])

cat(t, file = "../plots/table_out.tex", sep = "\n")

newdata <-  data.frame(
  hr = factor(levels(vte$hr), levels(vte$hr)),
  location = c("Bloomingdale Regional Public Library"),
  day = "SAT 10/27",
  gender = "M",
  race = "Black",
  age = mean(vte$age, na.rm = T),
  agegroup = "[50,60)",
  party = "DEM",
  voted_08 = 0,
  logincome = mean(vte$logincome, na.rm = T),
  over = TRUE,
  county = "HIL"
)

p2over <- predict(mn1, newdata = newdata, type = "link", se = TRUE)
p2under <- predict(mn1, newdata = mutate(newdata, over = FALSE), type = "link", se = TRUE)

plt3over <- data.frame(hr =newdata$hr,
                       pct = plogis(p2over$fit),
                       low = plogis(p2over$fit - qnorm(.975)*p2over$se.fit),
                       high = plogis(p2over$fit + qnorm(.975)*p2over$se.fit),
                       var = "last voter checked-in after 7:30pm", stringsAsFactors = F)

plt3under <- data.frame(hr =newdata$hr,
                        pct = plogis(p2under$fit),
                        low = plogis(p2under$fit - qnorm(.975)*p2under$se.fit),
                        high = plogis(p2under$fit + qnorm(.975)*p2under$se.fit),
                        var = "last voter checked-in before 7:30pm", stringsAsFactors = F)

plt3overunder <- bind_rows(plt3over, plt3under)


plot2save <- ggplot(plt3overunder, aes(hr, pct, ymin = low, ymax = high, colour = var)) +
  geom_point(position = position_nudge(.5)) + geom_errorbar(width = 0,position = position_nudge(.5))  + theme_bw() +  xlab("") +
  geom_vline(xintercept = 13, colour ="black", size = 1.25) +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=.5)) +
  scale_y_continuous(breaks = seq(0, 1, .02), labels = seq(0, 100, 2),limits = c(.75, .9), name = "Predicted probability of voting\n") +
  scale_x_discrete(limits = c("7:00am","8:00am","9:00am","10:00am","11:00am","12:00pm",
                              "1:00pm","2:00pm","3:00pm","4:00pm","5:00pm","6:00pm", "7:00pm"), name = "") +
  scale_color_manual(values = c("grey50", "black"), name = "Among polling locations where...") +
  ggtitle("") +
  theme(legend.position = c(.3,.2))
ggsave(plot2save, filename = "../Plots/probability_of_voting_in_2016_over_under.pdf", height = 5, width = 7)




#############################
#  OLD CODE
#############################

# #Define variables
# means <- coef(mn)
# varcov <- vcov(mn)
# over <- c(FALSE, TRUE)
# df <- mn$model
# n <- nrow(df)
# h <- levels(df$hr)
# 
# df_no <- df
# df_no$over <- rep(FALSE, times = nrow(df))
# 
# df_yes <- df
# df_yes$over <- rep(TRUE, times = nrow(df))
# 
# nsim <- 50
# sim_ate <- matrix(nrow = nsim, ncol = length(h))
# set.seed(100)
# 
# mn_new <- mn 
# 
# betas <- mvrnorm(mu = means, Sigma = varcov, n = nsim)
# 
# 
# 
# for (sim in 1:nsim) {
#     cat("Simulation ", sim, " of ", nsim, ".\n", sep = "")
#     mn_new$coefficients <- betas[sim,]
#     predict_no <- predict(object = mn_new, newdata = mutate(df, over = TRUE), type = "response")
#     predict_yes <- predict(object = mn_new, newdata = mutate(df, over = TRUE), type = "response")
#     sim_ate[sim,] <- tapply(predict_yes - predict_no, df$hr, mean)
# }

# 
# xbar <- colMeans(sim_ate)
# lo <- apply(sim_ate,2,function(x) quantile(x, .025))
# hi <- apply(sim_ate,2,function(x) quantile(x, .975))
# plot(xbar*100, ylim = range(ci))
# segments(1:length(xbar), lo*100, 1:length(xbar), hi*100)
# abline(h = 0)
# 
# p <- data.frame(hr = df$hr, p)
# 
# table2use <- data.frame()
# for (hr2use in sort(unique(p$hr))) {
#     diff <- p$line_yes[p$hr == hr2use] - p$line_no[p$hr == hr2use]
#     ate <- mean(diff)
#     ci <- quantile(diff, probs = c(.025, .975))
#     results <- paste0(format(round(c(ate, ci)*100, digits = 2), nsmall = 2), "%")
#     results <- c(hr2use, results)
#     names(results) <- c("Time", "Mean", "Low", "High")
#     cat (results[1:2], "\n")
#     table2use <- bind_rows(table2use, results)
# }

## diff <- p$line_no - p$line_yes
## ate <- mean(diff)
## ci <- quantile(diff, probs = c(.025, .975))
## results <- paste0(format(round(c(ate, ci)*1, digits = 2), nsmall = 2), "%")
## results <- c("All", results)
## names(results) <- c("Time", "Mean", "Low", "High")
## cat (results[1:2], "\n")
## table2use <- bind_rows(table2use, results)
# 
# 
# table2print <- print(xtable(table2use,
#                             caption = "Effect on probability of turnout of waiting in line, by time of 2012 early vote",
#                             label = "tab:ate",
#                             hline.after = TRUE,
#                             hline.after = 2:3,
#                             type = "latex"),
#                      include.rownames=FALSE)
# 
# cat(table2print, file = "../plots/table_out_ate.tex", sep = "\n")

## End ATE table


# 
# newdata <-  data.frame(
#   hr = factor(levels(vte$hr), levels(vte$hr)),
#   location = c("Bloomingdale Regional Public Library"),
#   day = "SAT 10/27",
#   gender = "M",
#   race = "Black",
#   age = mean(vte$age, na.rm = T),
#   agegroup = "[50,60)",
#   party = "DEM",
#   voted_08 = 0,
#   over = TRUE
# )
# 
# p2over <- predict(mn, newdata = newdata[as.integer(newdata$hr)<13,], type = "link", se = TRUE)
# 
# newdata <-  data.frame(
#   hr = factor(levels(vte$hr), levels(vte$hr)),
#   location = c("Bloomingdale Regional Public Library"),
#   day = "SAT 10/27",
#   gender = "M",
#   race = "Black",
#   age = mean(vte$age, na.rm = T),
#   agegroup = "[50,60)",
#   party = "DEM",
#   voted_08 = 0,
#   over = FALSE
# )
# 
# p2under <- predict(mn, newdata = newdata[as.integer(newdata$hr)<13,], type = "link", se = TRUE)
# 
# 
# plt3over <- data.frame(hr =newdata$hr[as.integer(newdata$hr)<13], 
#                        pct = plogis(p2over$fit), 
#                        low = plogis(p2over$fit - 1.96*p2over$se.fit), 
#                        high = plogis(p2over$fit + 1.96*p2over$se.fit),
#                        var = "last voter checked-in after 7:30pm", stringsAsFactors = F)
# 
# plt3under <- data.frame(hr =newdata$hr[as.integer(newdata$hr)<13], 
#                         pct = plogis(p2under$fit), 
#                         low = plogis(p2under$fit - 1.96*p2under$se.fit), 
#                         high = plogis(p2under$fit + 1.96*p2under$se.fit),
#                         var = "last voter checked-in before 7:30pm", stringsAsFactors = F)
# 
# plt3overunder <- bind_rows(plt3over, plt3under)
# 
# 
# plot2save <- ggplot(plt3overunder, aes(hr, pct, ymin = low, ymax = high, colour = var)) + 
#   geom_point(position = position_nudge(.5)) + geom_errorbar(width = 0,position = position_nudge(.5))  + theme_bw() +  xlab("") +
#   geom_vline(xintercept = 13, colour ="black", size = 1.25) +
#   theme(axis.text.x = element_text(angle=90, hjust=1, vjust=.5)) +
#   scale_y_continuous(breaks = seq(0, 1, .02), labels = seq(0, 100, 2),limits = c(.75, .9), name = "Predicted probability of voting\n") +
#   scale_x_discrete(limits = levels(plt3overunder$hr)[1:14], name = "") +
#   scale_color_manual(values = c("grey50", "black"), name = "Among polling locations where...") +
#   ggtitle("") +
#   theme(legend.position = c(.3,.2))
# ggsave(plot2save, filename = "../Plots/probability_of_voting_in_2016_over_under.pdf", height = 5, width = 7)
# 
# 
# 
# 
# newdata <-  data.frame(
#   hr = factor(levels(vte$hr), levels(vte$hr)),
#   location = c("Bloomingdale Regional Public Library"),
#   day = "SAT 10/27",
#   gender = "M",
#   race = "White",
#   age = mean(vte$age, na.rm = T),
#   agegroup = "[50,60)",
#   party = "DEM",
#   voted_08 = 0,
#   over = TRUE
# )
# 
# p2over <- predict(mn, newdata = newdata[as.integer(newdata$hr)<13,], type = "link", se = TRUE)
# 
# newdata <-  data.frame(
#   hr = factor(levels(vte$hr), levels(vte$hr)),
#   location = c("Bloomingdale Regional Public Library"),
#   day = "SAT 10/27",
#   gender = "M",
#   race = "White",
#   age = mean(vte$age, na.rm = T),
#   agegroup = "[50,60)",
#   party = "DEM",
#   voted_08 = 0,
#   over = FALSE
# )
# 
# p2under <- predict(mn, newdata = newdata[as.integer(newdata$hr)<13,], type = "link", se = TRUE)
# 
# 
# plt3over <- data.frame(hr =newdata$hr[as.integer(newdata$hr)<13], 
#                        pct = plogis(p2over$fit), 
#                        low = plogis(p2over$fit - 1.96*p2over$se.fit), 
#                        high = plogis(p2over$fit + 1.96*p2over$se.fit),
#                        var = "last voter checked-in after 7:30pm", stringsAsFactors = F)
# 
# plt3under <- data.frame(hr =newdata$hr[as.integer(newdata$hr)<13], 
#                         pct = plogis(p2under$fit), 
#                         low = plogis(p2under$fit - 1.96*p2under$se.fit), 
#                         high = plogis(p2under$fit + 1.96*p2under$se.fit),
#                         var = "last voter checked-in before 7:30pm", stringsAsFactors = F)
# 
# plt3overunder <- bind_rows(plt3over, plt3under)
# 
# 
# plot2save <- ggplot(plt3overunder, aes(hr, pct, ymin = low, ymax = high, colour = var)) + 
#   geom_point(position = position_nudge(.5)) + geom_errorbar(width = 0,position = position_nudge(.5))  + theme_bw() +  xlab("") +
#   geom_vline(xintercept = 13, colour ="black", size = 1.25) +
#   theme(axis.text.x = element_text(angle=90, hjust=1, vjust=.5)) +
#   scale_y_continuous(breaks = seq(0, 1, .02), labels = seq(0, 100, 2),limits = c(.75, .9), name = "Predicted probability of voting (%)\n") +
#   scale_x_discrete(limits = levels(plt3overunder$hr)[1:14], name = "") +
#   scale_color_manual(values = c("grey50", "black"), name = "Among polling locations where...") +
#   ggtitle("") +
#   theme(legend.position = c(.3,.2))
# ggsave(plot2save, filename = "../Plots/probability_of_voting_in_2016_over_under_white.pdf", height = 5, width = 7)
# 
# 
# 
# # voted early -------------------------------------------------------------
# 
# mn2 <- glm(voted_early_16 ~ hr + over + hr:over +  gender + race + agegroup + party + voted_08, data = vte[vte$voted_16 == 1, ], family = "binomial")
# 
# summary(mn2)
# 
# orderedvar <- names(mn2$coefficients)[-1][c(1:12, 26:36, 13:25)]
# t <- stargazer(mn2,               
#                title = "Logit regression predicting voter turnout in 2016",
#                label = "tab:reg",
#                dep.var.labels =  c("Voted in 2016"),               
#                omit.stat=c("LL","ser","f"),
#                type = "latex", 
#                order = c(1:12, 26:36, 13:25),
#                notes = "",
#                single.row=TRUE,
#                covariate.labels = c(
#                  "8:00am",
#                  "9:00am",
#                  "10:00am",
#                  "11:00am",
#                  "12:00pm",
#                  "1:00pm",
#                  "2:00pm",
#                  "3:00pm",
#                  "4:00pm",
#                  "5:00pm",
#                  "6:00pm",
#                  "Over",
#                  "8:00am \\& Over",
#                  "9:00am \\& Over",
#                  "10:00am \\& Over",
#                  "11:00am \\& Over",
#                  "12:00pm \\& Over",
#                  "1:00pm \\& Over",
#                  "2:00pm \\& Over",
#                  "3:00pm \\& Over",
#                  "4:00pm \\& Over",
#                  "5:00pm \\& Over",
#                  "6:00pm \\& Over",
#                  "Gender: male",
#                  "Race: Black",
#                  "Race: Hispanic",
#                  "Race: Asian",
#                  "Age group: 30-39",
#                  "Age group: 40-49",
#                  "Age group: 50-59",
#                  "Age group: 60-69",
#                  "Age group: 70+",
#                  "Party: independent",
#                  "Party: none",
#                  "Party: Republican",
#                  "Voted08: yes"),
#                no.space = TRUE,
#                font.size = "scriptsize")
# 
# cat(t, file = "../plots/table_out_early.tex", sep = "\n")
# 
# 
# 
# newdata <-  data.frame(
#   hr = factor(levels(vte$hr), levels(vte$hr)),
#   location = c("Bloomingdale Regional Public Library"),
#   day = "SAT 10/27",
#   gender = "M",
#   race = "Black",
#   age = mean(vte$age, na.rm = T),
#   agegroup = "[50,60)",
#   party = "DEM",
#   voted_08 = 0,
#   over = TRUE
# )
# 
# p2over <- predict(mn2, newdata = newdata[as.integer(newdata$hr)<13,], type = "link", se = TRUE)
# 
# newdata <-  data.frame(
#   hr = factor(levels(vte$hr), levels(vte$hr)),
#   location = c("Bloomingdale Regional Public Library"),
#   day = "SAT 10/27",
#   gender = "M",
#   race = "Black",
#   age = mean(vte$age, na.rm = T),
#   agegroup = "[50,60)",
#   party = "DEM",
#   voted_08 = 0,
#   over = FALSE
# )
# 
# p2under <- predict(mn2, newdata = newdata[as.integer(newdata$hr)<13,], type = "link", se = TRUE)
# 
# 
# plt3over <- data.frame(hr =newdata$hr[as.integer(newdata$hr)<13], 
#                        pct = plogis(p2over$fit), 
#                        low = plogis(p2over$fit - 1.96*p2over$se.fit), 
#                        high = plogis(p2over$fit + 1.96*p2over$se.fit),
#                        var = "last voter checked-in after 7:30pm", stringsAsFactors = F)
# 
# plt3under <- data.frame(hr =newdata$hr[as.integer(newdata$hr)<13], 
#                         pct = plogis(p2under$fit), 
#                         low = plogis(p2under$fit - 1.96*p2under$se.fit), 
#                         high = plogis(p2under$fit + 1.96*p2under$se.fit),
#                         var = "last voter checked-in before 7:30pm", stringsAsFactors = F)
# 
# plt3overunder <- bind_rows(plt3over, plt3under)
# 
# 
# plot2save <- ggplot(plt3overunder, aes(hr, pct, ymin = low, ymax = high, colour = var)) + 
#   geom_point(position = position_nudge(.5)) + geom_errorbar(width = 0,position = position_nudge(.5))  + theme_bw() +  xlab("") +
#   geom_vline(xintercept = 13, colour ="black", size = 1.25) +
#   theme(axis.text.x = element_text(angle=90, hjust=1, vjust=.5)) +
#   scale_y_continuous(breaks = seq(0, 1, .02), labels = seq(0, 100, 2),limits = c(.7, .77), name = "Predicted probability of voting early\n") +
#   scale_x_discrete(limits = levels(plt3overunder$hr)[1:14], name = "") +
#   scale_color_manual(values = c("grey50", "black"), name = "Among polling locations where...") +
#   ggtitle("") +
#   theme(legend.position = c(.3,.2))
# ggsave(plot2save, filename = "../Plots/probability_of_voting_in_2016_over_under_early.pdf", height = 5, width = 7)
# 
# 
# ## Make combined table
# 
# t_combined <- stargazer(mn, mn2,
#                         omit.stat = c("LL","ser","f"),
#                         title = "Regression results predicting voter behavior in 2016",
#                         label = "tab:reg",
#                         type = "latex",
#                         single.row=TRUE,
#                         dep.var.labels =  c("Voted in 2016", "Voted early in 2016"),               
#                         covariate.labels = c(
#                    "8:00am",
#                    "9:00am",
#                    "10:00am",
#                    "11:00am",
#                    "12:00pm",
#                    "1:00pm",
#                    "2:00pm",
#                    "3:00pm",
#                    "4:00pm",
#                    "5:00pm",
#                    "6:00pm",
#                    "Over",
#                    "8:00am \\& Over",
#                    "9:00am \\& Over",
#                    "10:00am \\& Over",
#                    "11:00am \\& Over",
#                    "12:00pm \\& Over",
#                    "1:00pm \\& Over",
#                    "2:00pm \\& Over",
#                    "3:00pm \\& Over",
#                    "4:00pm \\& Over",
#                    "5:00pm \\& Over",
#                    "6:00pm \\& Over",
#                    "Gender: male",
#                    "Race: Black",
#                    "Race: Hispanic",
#                    "Race: Asian",
#                    "Age group: 30-39",
#                    "Age group: 40-49",
#                    "Age group: 50-59",
#                    "Age group: 60-69",
#                    "Age group: 70+",
#                    "Party: independent",
#                    "Party: none",
#                    "Party: Republican",
#                    "Voted08: yes"),
#                no.space = TRUE,
#                font.size = "scriptsize")
# 
# cat(t_combined, file = "../Plots/table_out_combined.tex", sep = "\n")
# 
# 
# history <- my_db %>% 
#   tbl("history") %>% 
#   filter(electiontype == "GEN", 
#          historycode != "B", 
#          historycode != "P", 
#          electiondate == "11/04/2014") %>%
#   #select(voterid, historycode) %>% 
#   rename(gen14 = historycode) %>%
#   count(countycode) %>%
#   collect()
# 
# df <- bind_cols(arrange(history, countycode), arrange(fla, X2))
# pdf("differences.pdf")
# i <- abs((df$n - df$X3)/df$X3) > .1
# plot(df$n/1000, df$X3/1000, xlab = "Voterfile", ylab = "Actual", main = "Total Votes (in thousands)")
# abline(a = 0, b = 1)
# text(df$n[i]/1000, df$X3[i]/1000, labels = df$countycode[i], pos = 4)
# dev.off()
# 
# df %>%
#   transmute(county = X1, countycode, X2, voterfile = n, actual = X3, difference = actual - voterfile, percentdiff = round(difference/voterfile*100, 2)) %>% arrange(desc(abs(percentdiff)))
# 
# 

## make usage plot based on over

forplot <- vte[names(mn1$linear.predictors),] %>% 
  group_by(hr, over) %>% 
  summarize(num = n() / length(unique(time))) %>%
  mutate(over2 = recode(as.character(over), "TRUE" = 1, "FALSE" = 2)) %>%
  mutate(over2 = as.factor(over2))
  
ggplot(data = forplot, aes(x = hr, y = num, colour = over2)) +
  geom_point(size = 1.5, position = position_nudge(.5)) +
  theme_bw() +
  xlab("") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)) +
  scale_color_manual(values = c("grey50", "black"), name = "Among polling locations where...", 
                     labels = c("last voter checked-in after 7:30pm", "last voter checked-in before 7:30pm")) +
  scale_y_continuous(limits = c(0, 800), breaks = seq(0, 800, by = 100), name = "Check-ins per hour") +
  scale_x_discrete(limits = 
                     c("7:00am","8:00am","9:00am","10:00am","11:00am","12:00pm",
                       "1:00pm","2:00pm","3:00pm","4:00pm","5:00pm","6:00pm", "7:00pm")) +
  geom_vline(xintercept = 13, colour ="black", size = 1.25) +
  theme(legend.position = c(0.3, 0.15))
ggsave(filename = "../Plots/over-plot.pdf", height = 5, width = 7)

  
  
  
  


