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
    rbind(table(select(evid %>% filter(year == 2012), county)),
          table(select(evid %>% filter(year == 2016), county))
          )
colnames(table2use)[match("ALA", colnames(table2use))] <- "Alachua"
colnames(table2use)[match("BRO", colnames(table2use))] <- "Broward"
colnames(table2use)[match("DAD", colnames(table2use))] <- "Miami-Dade"
colnames(table2use)[match("HIL", colnames(table2use))] <- "Hillsborough"
colnames(table2use)[match("ORA", colnames(table2use))] <- "Orange"
colnames(table2use)[match("PAL", colnames(table2use))] <- "Palm Beach"
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
hst$race2 <- if_else(hst$race == "White", "White", "Non-White")
hst$race2 <- factor(hst$race2, levels = c("White", "Non-White"))
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
    locdf <- evid12 %>% filter(county == listofcount[i,1], location == listofcount[i,2], day == listofcount[i,3]) %>% arrange(time2) %>% mutate(count = row_number())
    forplot <-ggplot(locdf, aes(time2, count)) +
      geom_point(size = .1) +
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
  scale_y_continuous(breaks = seq(0, 100, 5), name = "") +
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
  scale_shape_discrete(name = "Race")  
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
  scale_shape_discrete(name = "Race")  
#ggtitle("Partisan Composition of Early Voters by Hour (Whites)")
ggsave(plot2save, file = "../Plots/partisan_composition_by_race.pdf", height = 4, width = 7)

###EVID 2016
evid16 <- evid %>% filter(year == 2016)

evid16$day <- factor(evid16$day, labels = c("MON 10/24",  "TUE 10/25", "WED 10/26", "THU 10/27", "FRI 10/28", "SAT 10/29", "SUN 10/30", "MON 10/31", "TUE 11/01", "WED 11/02", "THU 11/03", "FRI 11/04", "SAT 11/05", "SUN 11/06"))


lim <- c(levels(evid16$hr), "1:00am")

forplot <- evid16 %>%  distinct(hr, day, location) %>% count(hr, day) %>% complete(hr, day, fill = list(n = 0))
plot2save <-  ggplot(forplot, aes(hr, n, colour = day, group = day)) + geom_line(position = position_nudge(.5)) + theme_bw() + scale_colour_grey(start = 0.8, end = 0.2, name = "Day") +
  geom_vline(xintercept = 13, colour ="black", size = 1.25) + geom_point( position = position_nudge(.5)) + xlab("")  +
  scale_y_continuous(breaks = seq(0, 125, 5), name = "") +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=.5),
        legend.text = element_text(size = 8),
        legend.key.height = unit(.8,"line"))
ggsave (plot2save, file = "../Plots/number_of_locations_2016.pdf", height = 4, width = 7)

plot2save <- ggplot(filter(evid, race != "Asian",race != "Other"), aes(x=hr)) +  geom_bar(stat="count", position = position_nudge(.5), colour = "black", fill = "grey") + facet_grid(race~year) + geom_vline(xintercept = 13, colour ="black", size = 1.25) +
  scale_y_continuous(breaks = seq(0, 100000, 10000), labels = seq(0, 100, 10), name = "Count in thousands\n") + theme_bw() +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=.5)) + xlab("") +
  scale_x_discrete(breaks = hrs,limits = hrs)  
ggsave (plot2save, file = "../Plots/histogram_by_hour_by_race_2012_2016.pdf", height = 4, width = 8)





#############
# TEST
#############

library(MASS)
library(boot)

sum(is.na(evid12$gender) & is.na(evid12$race) & is.na(evid12$birthdate))
sum(evid12$time2 >= 19)
sum(!evid12$party %in% c("DEM", "REP", "IDP", "NPA"))
sum(!evid12$gender %in% c("U", NA))

groups <- c(18, 30, 40, 50, 60, 70, 120)
vte <- evid12  %>% group_by(location, day) %>% mutate(over = max(time2) >= 19.5, agegroup = cut(age, groups, right = FALSE)) %>% filter(time2 < 19, party %in% c("DEM", "REP", "IDP", "NPA"), gender != "U")

rc <- c("White", "Black", "Hispanic", "Asian")
vte$race <- factor(vte$race, rc)

mn <- glm(voted_16 ~ hr + over + hr:over +  gender + race + agegroup + party + voted_08, data = vte, family = "binomial")
#Define variables
means <- coef(mn)
varcov <- vcov(mn)
over <- c(FALSE, TRUE)
df <- mn$model
n <- nrow(df)
p <- data.frame(noline = rep(NA, n), line = rep(NA, n))
for (i in 1:n){
  # Draw a set of beta coefficients for a multivariate normal distribution.
  betas <- mvrnorm(mu = means, Sigma = varcov)
  nms <- names(betas)
  for (j in 1:2){
    bx <- vector(length = 9)
    bx[1] <- betas["(Intercept)"]
    bx[2] <- betas[paste0("hr", df[1,"hr"])]
    bx[3] <- betas[paste0("over", over[j])]
    bx[4] <- betas[paste0("hr", df[1,"hr"], ":over", over[j])]
    bx[5] <- betas[paste0("gender", df[1,"gender"])]
    bx[6] <- betas[paste0("race", df[1,"race"])]
    bx[7] <- betas[paste0("agegroup", df[1,"agegroup"])]
    bx[8] <- betas[paste0("party", df[1,"party"])]
    bx[9] <- df[1,"voted_08"] * betas["voted_08"]
    p[i,j] <- inv.logit(sum(bx,na.rm = T))
  }
  cat("\r", i, "of", n, "\r") 
}

p <- data.frame(hr = df$hr, p)

####################
## Another test
####################

df_no <- df
df_no$over <- rep(FALSE, times = nrow(df))

df_yes <- df
df_yes$over <- rep(TRUE, times = nrow(df))

p <- data.frame(line_no = rep(0, n), line_yes = rep(0, n))

nsim <- 100

for (sim in 1:nsim) {
    cat(sim,"\n")
    betas <- mvrnorm(mu = means, Sigma = varcov)
    mn$coef <- betas
    predict_no <- predict(mn, newdata = df_no, type = "response")
    predict_yes <- predict(mn, newdata = df_yes, type = "response")
    p$line_no = p$line_no + predict_no
    p$line_yes = p$line_yes + predict_yes
}

p$line_no <- p$line_no / nsim
p$line_yes <- p$line_yes / nsim

p <- data.frame(hr = df$hr, p)

table2use <- data.frame(nrow = 0, ncol = 2)
for (hr2use in sort(unique(p$hr))) {
    ate <- mean(p$line_no[p$hr == hr2use] - p$line_yes[p$hr == hr2use]) 
    cat (hr2use, ":  ", format(ate, digits = 2), "%", "\n", sep = "")
    table2use <- rbind(table2use, c(hr2use, ate))
}

table2print <- xtable(table2use,
                      include.rownames =  FALSE,
                      caption = "Average effect of waiting in line by time of 2012 early vote",
                      label = "tab:ate",
                      hline.after = TRUE,
                      align = "lcr",
                      type = "latex")
names(table2print) <- c("Time", "Difference in probability")


## End another test

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
  hr = factor(levels(vte$hr), levels(vte$hr)),
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
  hr = factor(levels(vte$hr), levels(vte$hr)),
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
  geom_vline(xintercept = 13, colour ="black", size = 1.25) +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=.5)) +
  scale_y_continuous(breaks = seq(0, 1, .02), labels = seq(0, 100, 2),limits = c(.75, .9), name = "Predicted probability of voting\n") +
  scale_x_discrete(limits = levels(plt3overunder$hr)[1:14], name = "") +
  scale_color_manual(values = c("grey50", "black"), name = "Among polling locations where...") +
  ggtitle("") +
  theme(legend.position = c(.3,.2))
ggsave(plot2save, filename = "../Plots/probability_of_voting_in_2016_over_under.pdf", height = 5, width = 7)




newdata <-  data.frame(
  hr = factor(levels(vte$hr), levels(vte$hr)),
  location = c("Bloomingdale Regional Public Library"),
  day = "SAT 10/27",
  gender = "M",
  race = "White",
  age = mean(vte$age, na.rm = T),
  agegroup = "[50,60)",
  party = "DEM",
  voted_08 = 0,
  over = TRUE
)

p2over <- predict(mn, newdata = newdata[as.integer(newdata$hr)<13,], type = "link", se = TRUE)

newdata <-  data.frame(
  hr = factor(levels(vte$hr), levels(vte$hr)),
  location = c("Bloomingdale Regional Public Library"),
  day = "SAT 10/27",
  gender = "M",
  race = "White",
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
  geom_vline(xintercept = 13, colour ="black", size = 1.25) +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=.5)) +
  scale_y_continuous(breaks = seq(0, 1, .02), labels = seq(0, 100, 2),limits = c(.75, .9), name = "Predicted probability of voting (%)\n") +
  scale_x_discrete(limits = levels(plt3overunder$hr)[1:14], name = "") +
  scale_color_manual(values = c("grey50", "black"), name = "Among polling locations where...") +
  ggtitle("") +
  theme(legend.position = c(.3,.2))
ggsave(plot2save, filename = "../Plots/probability_of_voting_in_2016_over_under_white.pdf", height = 5, width = 7)



# voted early -------------------------------------------------------------

mn2 <- glm(voted_early_16 ~ hr + over + hr:over +  gender + race + agegroup + party + voted_08, data = vte[vte$voted_16 == 1, ], family = "binomial")

summary(mn2)

orderedvar <- names(mn2$coefficients)[c(1:12, 26:36, 13:25)]
t <- stargazer(mn2,               
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

cat(t, file = "../plots/table_out_early.tex", sep = "\n")



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
  over = TRUE
)

p2over <- predict(mn2, newdata = newdata[as.integer(newdata$hr)<13,], type = "link", se = TRUE)

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
  over = FALSE
)

p2under <- predict(mn2, newdata = newdata[as.integer(newdata$hr)<13,], type = "link", se = TRUE)


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
  geom_vline(xintercept = 13, colour ="black", size = 1.25) +
  theme(axis.text.x = element_text(angle=90, hjust=1, vjust=.5)) +
  scale_y_continuous(breaks = seq(0, 1, .02), labels = seq(0, 100, 2),limits = c(.7, .77), name = "Predicted probability of voting early\n") +
  scale_x_discrete(limits = levels(plt3overunder$hr)[1:14], name = "") +
  scale_color_manual(values = c("grey50", "black"), name = "Among polling locations where...") +
  ggtitle("") +
  theme(legend.position = c(.3,.2))
ggsave(plot2save, filename = "../Plots/probability_of_voting_in_2016_over_under_early.pdf", height = 5, width = 7)


## Make combined table

t_combined <- stargazer(mn, mn2,
                        omit.stat = c("LL","ser","f"),
                        title = "Regression results predicting voter behavior in 2016",
                        label = "tab:reg",
                        type = "latex",
                        single.row=TRUE,
                        dep.var.labels =  c("Voted in 2016", "Voted early in 2016"),               
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

cat(t_combined, file = "../Plots/table_out_combined.tex", sep = "\n")

