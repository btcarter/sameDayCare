# PREAMBLE #####
# Author: Benjamin Carter
# Objective: process output from the SQL script and create usable CSVs for the
#   SDC EDA and ML projects.
###

# packages ####
library(dplyr)
library(tidyr)
library(stringr)
library(tidytext)
library(tm)
library(widyr)
library(topicmodels)


# paths ####
data.dir.path <- file.path("C:","Users","CarteB","BILLINGS CLINIC", 
                           "CSI & David Hedges - Same Day Care Project", "data")

df.path <- file.path(data.dir.path, "sdc.2017-2019.2020-08-26.csv")

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
  "DiagnosisPrioritySEQ",
  "ICD",
  "DiagnosisType",
  "DiagnosisDSC",
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

# correct bad entries ####
df[1,1] <- "2017-01-25 08:00:00.0000000"

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
    ),
    DTS = as.POSIXct(DTS)
  ) %>% 
  filter(
    !is.na(ICD_code) # get rid of entries with no ICD-10
  ) %>% 
  filter(
    !grepl("^\\d+", ICD_code) # get rid of entries with incomplete ICD-10
  )




# FLATTEN DATA ####

pancake.stack <- list()

# unique ICDs - maybe expand this so it's wide?
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

# ICD by DiagnosisType
pancake.stack$ICD_DiagnosisType <- df.processed %>% 
  select(
    PersonID,
    EncounterID,
    DiagnosisType,
    ICD_code
  ) %>% 
  distinct() %>% 
  pivot_wider(
    names_from = DiagnosisType,
    values_from = ICD_code
  )


# priority ICD
pancake.stack$PriorityICD <-df.processed %>% 
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
  slice(
    1
  ) %>% 
  ungroup() %>% 
  rename(
    PriorityICD = ICD_code
  )

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

# unique ZipCode
pancake.stack$ZipCode <- df.processed %>% 
  select(
    PersonID,
    EncounterID,
    DiagnosisPrioritySEQ,
    ZipCode
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
    ZipCode = paste(ZipCode, collapse = "; ")
  ) %>% 
  ungroup()

# sdc/ec facilities
SDCEC <- c(
  'Express Care Central',
  'Express Care Grand',
  'Express Care Heights',
  'Heights Same Day Care',
  'HTS Same Day Care',
  'Miles City Same Day Care',
  'Same Day Care',
  'Same Day Care Lab Schedule',
  'SDC Downtown Nurse',
  'SDC West Nurse',
  'Virtual Same Day Care Miles City',
  'WE Same Day Care',
  'West End SDC'
)

# unique encounters
pancake.stack$Encounters <- df.processed %>% 
  select(
    DTS,
    PersonID,
    EncounterID,
    ActiveIndicatorCD,
    AdmitAge,
    Ethnicity,
    Language,
    Race,
    Marital_Status,
    Sex,
    Religion,
    Building,
    NurseUnit,
    AdmitType,           
    EncounterType,
    RespiratoryFailure
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
  ungroup() %>%  
  mutate(
    return_in_14 = if_else(
      lead(unclass(DTS))-unclass(DTS) <= 14*3600 & 
        PersonID == lead(PersonID) & 
        NurseUnit %in% SDCEC,
      TRUE,
      FALSE
    )
  )

# sew everything together
df.flat <- pancake.stack$Encounters %>% 
  left_join(
    pancake.stack$ZipCode,
    by = c("PersonID", "EncounterID")
  ) %>%
  left_join(
    pancake.stack$PriorityICD,
    by = c("PersonID", "EncounterID")
  ) %>% 
  left_join(
    pancake.stack$ICD_block,
    by = c("PersonID", "EncounterID")
  ) %>% 
  left_join(
    pancake.stack$ICD_code,
    by = c("PersonID", "EncounterID")
  ) %>% 
  left_join(
    pancake.stack$ReasonForVisit,
    by = c("PersonID", "EncounterID")
  ) %>%  
  left_join(
    pancake.stack$DiagnosisType,
    by = c("PersonID", "EncounterID")
  ) %>% 
  left_join(
    pancake.stack$DiagnosisDSC,
    by = c("PersonID", "EncounterID")
  ) %>% 
  distinct()

df.flat.sdc.only <- df.flat %>% 
  select(
    -ActiveIndicatorCD
  ) %>% 
  filter(
    NurseUnit %in% SDCEC
  )





# write final df to file for later use.

writexl::write_xlsx(df.flat.sdc.only,
                    path = file.path(out.dir.path, "sdc.flat.xlsx"))
