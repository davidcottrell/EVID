## Make a table of SPAE Florida waiting fimes from 2012 and 2016
## General Elections

library(dplyr)
library(xtable)

if (!length(grep("MITU0022_OUTPUT", search())))
    attach(what = "../Data/SPAE2016/MITU0022_OUTPUT.RData", pos = 2)
dta2016 <- filter(x, inputstate == "Florida")
detach(pos = grep("MITU0022_OUTPUT", search()))
dim(dta2016)
dta2016$Q13 <- as.character(dta2016$Q13)
dta2016$Q13[grep("31", dta2016$Q13)] <- "31 minutes - 1 hour"
dta2016$Q13[grep("10-30 minutes", dta2016$Q13)] <- "10 - 30 minutes"
dta2016$Q13[grep("Less than 10 minutes", dta2016$Q13)] <- "Fewer than 10 minutes"

if (!length(grep("MITU0017_OUTPUT", search())))
    attach(what = "../data/SPAE2012/MITU0017_OUTPUT.RData")
dta2012 <- filter(x, grepl(" FL", x$countyname))
detach(pos = grep("MITU0017_OUTPUT", search()))
dim(dta2012)
dta2012$q10 <- as.character(dta2012$q10)

table2use2012 <- table(filter(dta2012, !is.na(dta2012$q10))$q10)
table2use2016 <- table(filter(dta2016, !is.na(dta2016$Q13))$Q13)


table2use2016 <- table2use2016[c(6, 3, 1, 2, 5, 4)]
names(table2use2012) <- names(table2use2016)


table2print <- xtable(t(rbind(table2use2012, table2use2016)),
                      row.names =  names(table2use2016),
                      caption = "Wait times of Florida respondents in the SPAE, 2012 and 2016 General Elections",
                      label = "tab:floridaspae",
                      hline.after = TRUE,
                      type = "latex")
names(table2print) <- c("2012", "2016")

latex2print <- print(table2print, caption.placement = "top")

cat(latex2print,
    file = "../Paper/spae_florida.tex", sep="\n")


