# PREAMBLE #####
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

df.path <- file.path(data.dir.path, "sdc.2017-2019.2020-08-19.csv")

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
  "Building",
  "NurseUnit",
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
df[1,1] <- "2017-01-01 08:00:00.0000000"

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
    ),
    ReasonForVisit = na_if(
      ReasonForVisit,
      "NA"
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


# Basic stats for all patient encounters from 2017-2019 ####
# raw <- c(
#   length(unique(df$PersonID)),
#   length(unique(df$EncounterID)),
#   nrow(df[df$DiagnosisType == "Primary Diagnosis", ]),
#   nrow(df[df$DiagnosisType == "Secondary Diagnosis", ]),
#   nrow(df[df$DiagnosisType == "Admit Diagnosis", ]),
#   nrow(df[df$DiagnosisType == "Clinical Diagnosis", ]),
#   length(unique(df$ZipCode)),
#   length(unique(df$ICD)),
#   mean(df$CharlsonDeyoScore, na.rm = TRUE)
# )
# 
# processed <- c(
#   length(unique(df.processed$PersonID)),
#   length(unique(df.processed$EncounterID)),
#   nrow(df.processed[df.processed$DiagnosisType == "Primary Diagnosis", ]),
#   nrow(df.processed[df.processed$DiagnosisType == "Secondary Diagnosis", ]),
#   nrow(df.processed[df.processed$DiagnosisType == "Admit Diagnosis", ]),
#   nrow(df.processed[df.processed$DiagnosisType == "Clinical Diagnosis", ]),
#   length(unique(df.processed$ZipCode)),
#   length(unique(df.processed$ICD)),
#   mean(df.processed$CharlsonDeyoScore, na.rm = TRUE)
# )
# 
# 
# df.prepost.comparison <- data.frame(
#   raw,
#   processed
# )

# FLATTEN DATA ####

pancake.stack <- list()
# unique ICDs 
pancake.stack$ICD_code <- df.processed %>% 
  select(
    PersonID,
    EncounterID,
    DiagnosisPrioritySEQ,
    ICD_code
  ) %>% 
  distinct() %>% 
  group_by(
    PersonID,
    EncounterID
  ) %>% 
  arrange(
    DiagnosisPrioritySEQ
  ) %>% 
  summarise(
    ICD_code = paste(ICD_code, collapse = "; ")
  ) %>% 
  ungroup()

# unique reasons for visits
pancake.stack$ReasonForVisit <- df.processed %>% 
  select(
    PersonID,
    EncounterID,
    DiagnosisPrioritySEQ,
    ReasonForVisit
  ) %>% 
  distinct() %>% 
  group_by(
    PersonID,
    EncounterID
  ) %>% 
  arrange(
    DiagnosisPrioritySEQ
  ) %>% 
  summarise(
    ReasonForVisit = paste(ReasonForVisit, collapse = "; ")
  ) %>% 
  ungroup()

# unique diagnosis free text
pancake.stack$DiagnosisFreeTXT <- df.processed %>% 
  select(
    PersonID,
    EncounterID,
    DiagnosisPrioritySEQ,
    DiagnosisFreeTXT
  ) %>% 
  distinct() %>% 
  group_by(
    PersonID,
    EncounterID
  ) %>% 
  arrange(
    DiagnosisPrioritySEQ
  ) %>% 
  summarise(
    DiagnosisFreeTXT = paste(DiagnosisFreeTXT, collapse = "; ")
  ) %>% 
  ungroup()

# unique diagnosis type
pancake.stack$DiagnosisType <- df.processed %>% 
  select(
    PersonID,
    EncounterID,
    DiagnosisPrioritySEQ,
    DiagnosisType
  ) %>% 
  distinct() %>% 
  group_by(
    PersonID,
    EncounterID
  ) %>% 
  arrange(
    DiagnosisPrioritySEQ
  ) %>% 
  summarise(
    DiagnosisType = paste(DiagnosisType, collapse = "; ")
  ) %>% 
  ungroup()

#unique diagnosisdsc
pancake.stack$DiagnosisDSC <- df.processed %>% 
  select(
    PersonID,
    EncounterID,
    DiagnosisPrioritySEQ,
    DiagnosisDSC
  ) %>% 
  distinct() %>% 
  group_by(
    PersonID,
    EncounterID
  ) %>% 
  arrange(
    DiagnosisPrioritySEQ
  ) %>% 
  summarise(
    DiagnosisDSC = paste(DiagnosisDSC, collapse = "; ")
  ) %>% 
  ungroup()

#unique diagnosisNormdsc
pancake.stack$DiagnosisNormDSC <- df.processed %>% 
  select(
    PersonID,
    EncounterID,
    DiagnosisPrioritySEQ,
    DiagnosisNormDSC
  ) %>% 
  distinct() %>% 
  group_by(
    PersonID,
    EncounterID
  ) %>% 
  arrange(
    DiagnosisPrioritySEQ
  ) %>% 
  summarise(
    DiagnosisNormDSC = paste(DiagnosisNormDSC, collapse = "; ")
  ) %>% 
  ungroup()

# unique ICD block
pancake.stack$ICD_block <- df.processed %>% 
  select(
    PersonID,
    EncounterID,
    DiagnosisPrioritySEQ,
    ICD_block
  ) %>% 
  distinct() %>% 
  group_by(
    PersonID,
    EncounterID
  ) %>% 
  arrange(
    DiagnosisPrioritySEQ
  ) %>% 
  summarise(
    ICD_block = paste(ICD_block, collapse = "; ")
  ) %>% 
  ungroup()

# unique encounters
pancake.stack$Encounters <- df.processed %>% 
  select(
    DTS,
    PersonID,
    EncounterID,
    AppointmentID,       
    ActiveIndicatorCD,
    AdmitAge,
    Ethnicity,
    Language,
    Race,
    Marital_Status,
    Sex,
    Religion,
    ZipCode,
    Location,
    Facility,
    AdmitType,           
    EncounterType
  ) %>% 
  distinct() %>% 
  group_by(
    PersonID
  ) %>% 
  arrange(
    PersonID,
    EncounterID,
    DTS
  ) %>% 
  ungroup()

pancake.stack$Encounters %>% 
  select(
    DTS,
    EncounterID,
    Location
  ) %>% 
  group_by(DTS, EncounterID) %>%  
  summarise(
    'total' = n()
  ) %>% 
  arrange(desc(`total`), EncounterID)
  

# variables to add
# first DTS for encounter, duration of encounter
# did they return?
