# convert SPSS to excel

library(dplyr)
library(foreign)
library(xlsx)

# load data
spss.path <- file.path("V:",
                  "Depts",
                  "CtrTransRsch",
                  "Dept Private",
                  "CTR Staff",
                  "PROJECT - IM Residency",
                  "RESIDENT - Brenda",
                  "DATA",
                  "FINAL DATA with collapsed dx and comorbidities 2014_2015 4_24_19 LR.sav")

OUT <- file.path("C:",
                 "Users",
                 "CarteB",
                 "OneDrive - BILLINGS CLINIC",
                 "projects",
                 "sdc",
                 "data")

spss <- read.spss(spss.path, to.data.frame = TRUE)

write.csv(spss, file = file.path(OUT, "SDC_2020.04.14.csv"))
