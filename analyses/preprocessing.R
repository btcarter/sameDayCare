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

df.path <- file.path(data.dir.path, "sdc.2017-2019.2020-06-29.csv")

out.dir.path <- file.path("C:","Users","CarteB","BILLINGS CLINIC", 
                          "CSI & David Hedges - Same Day Care Project", "data")


# load data ####

cols <- c(
  "DTS",
  "PersonID",
  "EncounterID",
  "AppointmentID",
  "ActiveIndicatorCD",
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

# Post preprocessing

length(unique(df.processed$PersonID))
length(unique(df.processed$EncounterID))
nrow(df.processed[df.processed$DiagnosisType == "Primary Diagnosis", ])
nrow(df.processed[df.processed$DiagnosisType == "Secondary Diagnosis", ])
nrow(df.processed[df.processed$DiagnosisType == "Admit Diagnosis", ])
nrow(df.processed[df.processed$DiagnosisType == "Clinical Diagnosis", ])
length(unique(df.processed$ZipCode))
length(unique(df.processed$ICD))
mean(df.processed$CharlsonDeyoScore, na.rm = TRUE)


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

sdc.person.ids <- df.processed %>% 
  filter(
    Location %in% sdcec.list
  ) %>% 
  select(
    PersonID
  ) %>% 
  distinct(
    PersonID
  )

df.processed.sdc <- df.processed %>% 
  filter(
    PersonID %in% sdc.person.ids$PersonID
  )



smash.it <- function(data = data.frame(), group = array(), smash = character()){
  data %>% 
    
}




# SDC/EC Descriptives

## Top 50 ICD-10 Codes for Primary Dx

## Top 50 ICD-10 Codes for Clinical Dx

## Top 50 ICD-10 Codes for Diagnosis Priority 1
