#####
# PREAMBLE
# Author: Benjamin Carter
# Objective: process output from the SQL script and create usable CSVs for the
#   SDC EDA and ML projects.
###

# packages ####
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(icd) # for reading in ICD10 classifying information and calculating risk scores

# paths ####
data.dir.path <- file.path("C:","Users","CarteB","BILLINGS CLINIC", 
                           "CSI & David Hedges - Same Day Care Project", "data")

df.path <- file.path(data.dir.path, "sdc.2017-2019.2020-07-21.csv")

out.dir.path <- file.path("C:","Users","CarteB","BILLINGS CLINIC", 
                          "CSI & David Hedges - Same Day Care Project", "data")


# load data ####

cols <- c(
  "DTS",
  "PersonID",
  "EncounterID",
  "AppointmentID",
  "ActiveIndicatorCD",
  "AdmitAge",
  "Ethnicity",
  "Language",
  "Race",
  "Marital_Status",
  "Sex",
  "Religion",
  "ZipCode",
  "Location",
  "Facility",
  "AdmitType",
  "EncounterType",
  "ReasonForVisit",
  "DiagnosisFreeTXT",
  "DiagnosisPrioritySEQ",
  "ICD",
  "DiagnosisType",
  "DiagnosisDSC",
  "DiagnosisNormDSC",
  "RespiratoryFailure",
  "CharlsonDeyoScore"
)

df <- read.csv2(
  df.path,
  header = FALSE,
  sep = ",",
  col.names = cols,
  stringsAsFactors = FALSE,
  na.strings = "NULL"
  )

ICD <- icd10cm2019

# correct bad entries ####
df[1,1] <- "2017-01-01 00:00:00.0000000"

df.processed <- df %>% 
  mutate(
    Race = na_if(
      Race,
      "0"
    ),
    Race = na_if(
      Race,
      " "
    ),
    Ethnicity = na_if(
      Ethnicity,
      "0"
    ),
    Ethnicity = na_if(
      Ethnicity,
      " "
    )
  ) %>% 
  mutate(
    Race = replace_na(
      Race,
      "Not Obtained"
    ),
    Ethnicity = replace_na(
      Ethnicity,
      "Not Obtained"
    ),
    ZipCode = gsub(   # only grab the zipcode, not the extension
      "(\\d{5}).*",
      "\\1",
      ZipCode
    ),
    ICD_block = gsub(
      "(.{3}).?(.*)",
      "\\1",
      ICD
    ),
    ICD_code = gsub(
      "([[:alnum:]]*)\\.?([[:alnum:]]*)",
      "\\1\\2",
      ICD
    )
  ) %>% 
  filter(
    !is.na(ICD_code) # get rid of entries with no ICD-10
  ) %>% 
  filter(
    !grepl("^\\d+", ICD_code)
  )


# Basic stats for all patient encounters from 2017-2019

length(unique(df$PersonID))
length(unique(df$EncounterID))
nrow(df[df$DiagnosisType == "Primary Diagnosis", ])
nrow(df[df$DiagnosisType == "Secondary Diagnosis", ])
nrow(df[df$DiagnosisType == "Admit Diagnosis", ])
nrow(df[df$DiagnosisType == "Clinical Diagnosis", ])
length(unique(df$ZipCode))
length(unique(df$ICD))
mean(df$CharlsonDeyoScore, na.rm = TRUE)

# Post preprocessing checks

length(unique(df.processed$PersonID))
length(unique(df.processed$EncounterID))
nrow(df.processed[df.processed$DiagnosisType == "Primary Diagnosis", ])
nrow(df.processed[df.processed$DiagnosisType == "Secondary Diagnosis", ])
nrow(df.processed[df.processed$DiagnosisType == "Admit Diagnosis", ])
nrow(df.processed[df.processed$DiagnosisType == "Clinical Diagnosis", ])
length(unique(df.processed$ZipCode))
length(unique(df.processed$ICD))
mean(df.processed$CharlsonDeyoScore, na.rm = TRUE)


# select SDC/EC individuals
sdcec.list <- c(
  "SDC Downtown",
  "SDC Downtown Nurse",
  "SDC Heights",
  "SDC West",
  "SDC West Nurse",
  "Billings Clinic Downtown EC",
  "Heights Express Care",
  "Grand Express Care",
  "Central Express Care"
  
)

sdc.persons <- df.processed %>% 
  filter(
    Location %in% sdcec.list
  ) %>% 
  distinct(
    PersonID
  ) %>% 
  left_join(
    df.processed[, c("PersonID",
                     "Sex",
                     "Ethnicity",
                     "Race",
                     "Language",
                     "Marital_Status",
                     "ZipCode",
                     "RespiratoryFailure",
                     "CharlsonDeyoScore")],
    by = "PersonID"
  ) %>% 
  distinct()


df.processed.sdc <- df.processed %>% 
  filter(
    PersonID %in% sdc.person.ids$PersonID
  )

# c("Location", "Facility", "AdmitType", "EncounterType",
#   +                 "ReasonForVisit", "DiagnosisFreeTXT", "DiagnosisPrioritySEQ",
#   +                 "ICD", "DiagnosisType", "DiagnosisDSC", "DiagnosisNormDSC",
#   +                 "RespiratoryFailure", "CharlsonDeyoScore", "ICD_block", "ICD_code")

# unique ICDs 
person.ICD_code <- df.processed.sdc %>% 
  select(
    PersonID,
    EncounterID,
    ICD_code
  ) %>% 
  distinct() %>% 
  group_by(
    PersonID,
    EncounterID
  ) %>% 
  summarise(
    ICD_code = paste(ICD_code, collapse = "; ")
  )

# unique reasons for visits
person.ReasonForVisit <- df.processed.sdc %>% 
  select(
    PersonID,
    EncounterID,
    ReasonForVisit
  ) %>% 
  distinct() %>% 
  group_by(
    PersonID,
    EncounterID
  ) %>% 
  summarise(
    ReasonForVisit = paste(ReasonForVisit, collapse = "; ")
  )





# variables to add
# first DTS for encounter, duration of encounter
# did they return?
