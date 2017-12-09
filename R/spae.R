## Make a table of SPAE Florida waiting fimes from 2016 General

library(dplyr)
library(xtable)
if (!length(grep("MITU0022_OUTPUT", search())))
    attach(what = "../Data/SPAE2016/MITU0022_OUTPUT.RData")
dta <- filter(x, inputstate == "Florida")
dim(dta)
dta$Q13 <- as.character(dta$Q13)

dta$Q13[grep("31", dta$Q13)] <- "31 minutes - 1 hour"
dta$Q13[grep("10-30", dta$Q13)] <- "10 - 30"
dta$Q13[grep("Less than 10 minutes", dta$Q13)] <- "Fewer than 10 minutes"

table2use <- table(filter(dta, !is.na(dta$Q13))$Q13)
table2print <- xtable(table2use[c(6, 3, 1, 2, 5, 4)],
                      caption = "Florida respondents in the 2016 SPAE",
                      label = "tab:floridaspae2016",
                      hline.after = TRUE,
                      type = "latex")
names(table2print) <- c("Number")

latex2print <- print(table2print, caption.placement = "top")

cat(latex2print,
    file = "../Paper/spae_florida_2016.tex", sep="\n")


