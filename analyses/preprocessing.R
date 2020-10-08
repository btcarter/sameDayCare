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
library(tidygeocoder)


# paths ####
data.dir.path <- file.path("C:","Users","CarteB","BILLINGS CLINIC", 
                           "CSI & David Hedges - Same Day Care Project", "data")

df.path <- file.path(data.dir.path, "sdc.2017-2019.2020-10-07.csv")

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
  "street",
  "city",
  "state",
  "ZipCode",
  "country",
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
  sep = ",",
  col.names = cols,
  stringsAsFactors = FALSE,
  na.strings = "NULL"
  )

# correct bad entries ####

df.processed <- df %>% 
  mutate(
    Race = gsub(
      "^\\s.*",
      "Not Obtained",
      Race
    ),
    Ethnicity = gsub(
      "^\\s.*",
      "Not Obtained",
      Ethnicity
    ),
    ReasonForVisit = na_if(
      ReasonForVisit,
      "NA"
    )
  ) %>% 
  mutate(
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

# manual clean up address information

bad <- c("10782", "US", "USA DUP")
df.processed$country[df.processed$country %in% bad] <- "USA"

bad <- c(state.abb, state.name, toupper(state.name))
df.processed$country[df.processed$state %in% bad] <- "USA"

bad <- c("GLENDIVE", "59330", "31", "MT4066979540", "US/MT", "MONTANA")
df.processed$state[df.processed$state %in% bad] <- "MT"

bad <- c(" ", 
         "   ", 
         "00", 
         "000", 
         "00000", 
         ".", 
         "XX", 
         "9999", 
         "99",
         "UNK",
         NULL, 
         NA)

df.processed <- df.processed %>% 
  mutate(
    street = if_else(
      street %in% bad,
      "UNKNOWN",
      street
    ),
    city = if_else(
      city %in% bad,
      "UNKNOWN",
      city
    ),
    state = if_else(
      state %in% bad,
      "UNKNOWN",
      state
    ),
    ZipCode = if_else(
      ZipCode %in% bad,
      "UNKNOWN",
      ZipCode
    )
  ) %>% 
  mutate(
    country = if_else(state == "UNKNOWN" & 
                        city == "UNKNOWN" & 
                        ZipCode == "UNKNOWN",
                      "UNKNOWN",
                      country
                      )
  )

df.processed <- df.processed %>% 
  mutate(
    city = if_else(
      street == paste(city,"  ",sep = ""),
      state,
      city
    ),
    state = if_else(
      street == paste(city,"  ",sep = ""),
      ZipCode,
      state
    )
  ) 

df.processed <- df.processed %>% 
  mutate(
    state = if_else(
      city == "BILLINGS",
      "MT",
      state
    )
  ) %>% 
  mutate(
    city = if_else(
      city == "BILLINGS",
      "Billings",
      city
    )
  )

bad <- c(state.abb, state.name, toupper(state.name))
df.processed$country[df.processed$state %in% bad] <- "USA"

df.processed$street[df.processed$street == "1234 Street "] <- "UNKNOWN"
df.processed$city[df.processed$street == "1234 Street "] <- "UNKNOWN"
df.processed$state[df.processed$street == "1234 Street "] <- "UNKNOWN"
df.processed$zip[df.processed$street == "1234 Street "] <- "UNKNOWN"
df.processed$country[df.processed$street == "1234 Street "] <- "UNKNOWN"
df.processed$country[df.processed$country == " "] <- "UNKNOWN"
df.processed$country[df.processed$country == "UK"] <- "United Kingdom"

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

batch_size <- 10000

for (section in 1:ceiling(nrow(pancake.stack$Encounters)/batch_size)){
  rows <- section*(1:batch_size)
  
  pancake.stack$Encounter[rows, ] <- 
    pancake.stack$Encounter[rows, ] %>% geocode(
    street = street,
    city = city,
    state = state,
    country = country,
    postalcode = ZipCode,
    method = 'census',
    full_results = TRUE,
    return_type = 'geographies'
  )

}

# GEOID is state_fips + county+fips + census_tract + census_block
  mutate(
    GEOID = get_geoid(
      street = street,
      city = city,
      state = state,
      country = country,
      postalcode = ZipCode
    )
  ) %>% 
  mutate(
    lat = GEOID[1],
    long = GEOID[2],
    GEOID = GEOID[3]
  )

# sew everything together for a flattened dataframe
df.flat <- pancake.stack$Encounters %>% 
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

df.long



# write final df to file for later use.

writexl::write_xlsx(df.flat.sdc.only,
                    path = file.path(out.dir.path, "sdc.flat.xlsx"))
