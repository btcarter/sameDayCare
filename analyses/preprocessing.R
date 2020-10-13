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

df.path <- file.path(data.dir.path, "sdc.2017-2019.2020-10-12.csv")

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
  "DiagnosisDSC"
)

df <- read.csv2(
  df.path,
  sep = ",",
  col.names = cols,
  stringsAsFactors = FALSE,
  na.strings = "NULL"
  )

# correct bad entries ####

tribes <- c("Chippewa",
            "Gros Ventre",
            "Crow",
            "Blackfeet",
            "Kootenai",
            "Northern Cheyenne",
            "Multiple",
            "Assiniboine",
            "Sioux",
            "Chippewa-Cree",
            "Flathead Salish",
            "Shoshone"
            )

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
    tribal_affil = if_else(
      Ethnicity %in% tribes,
      Ethnicity,
      "Not affiliated"
    ),
    Fed_Ethnicity = if_else(
      Ethnicity %in% c("Hispanic or Latino"),
      "Hispanic or Latino",
      "Non-Hispanic or Latino"
    )
  ) %>% 
  mutate(
    BC_Ethnicity = Ethnicity
  ) %>% 
  select(
    -Ethnicity
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
  ) %>% distinct()

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
df.processed$ZipCode[df.processed$street == "1234 Street "] <- "UNKNOWN"
df.processed$country[df.processed$street == "1234 Street "] <- "UNKNOWN"
df.processed$country[df.processed$country == " "] <- "UNKNOWN"
df.processed$country[df.processed$country == "UK"] <- "United Kingdom"

# get rid of c/o clauses and flag the PO Box addresses

df.processed <- df.processed %>% 
  mutate(
    street = toupper(street)
  ) %>% 
  mutate(
    street = gsub(
      "(.*)?(c\\/o|C\\/O)\\D*((\\d+.*)?)",
      "\\1 \\3",
      street
    )
  ) %>% 
  mutate(
    street = gsub(
      "^\\s(\\d+)$",
      "PO BOX \\1",
      street
    )
  ) %>% 
  mutate(
    po_box = if_else(
      grepl("(PO BOX)|(P.O. BOX)", street),
      TRUE,
      FALSE
    )
  )

df.processed <- df.processed %>% 
  distinct() %>% 
  group_by(
    PersonID,
    EncounterID,
    DTS
  ) %>%
  arrange(
    desc(DiagnosisPrioritySEQ)
  ) %>% 
  mutate(
    ICD_all = paste(ICD_code, collapse = "; "),
    ICD_block_all = paste(ICD_block, collapse = "; "),
    ReasonForVisit_all = paste(ReasonForVisit, collapse = "; "),
    DiagnosisDSC_all = paste(DiagnosisDSC, collapse = "; ")
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
Encounters <- df.flat %>% 
  select(
    DTS,
    PersonID,
    EncounterID,
    Building,
    NurseUnit,
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
  ungroup() %>%  
  mutate(
    return_in_14 = if_else(
      lead(unclass(DTS))-unclass(DTS) <= 14*3600 & 
        PersonID == lead(PersonID) & 
        NurseUnit %in% SDCEC,
      TRUE,
      FALSE
    )
  ) %>% 
  mutate(
    return_Building = if_else(
      PersonID = lead(PersonID) &
        DTS != lead(DTS),
      lead(Building),
      NA
    ),
    return_NurseUnit = if_else(
      PersonID = lead(PersonID) &
        DTS != lead(DTS),
      lead(NurseUnit),
      NA),
    days_to_return = if_else(
      PersonID = lead(PersonID) &
        DTS != lead(DTS),
      lead(unclass(DTS))-unclass(DTS)
    )
  )


# add location information
pancake.stack$Location <- df.processed %>% 
  select(
    street,
    city,
    state,
    country,
    ZipCode
  ) %>% 
  distinct()

batch_size <- 10000

for (section in 1:ceiling(nrow(pancake.stack$Location)/batch_size)){
  rows <- section*(1:batch_size)
  
  pancake.stack$Location[rows, ] <- 
    pancake.stack$Location[rows, ] %>% geocode(
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


# compute distance traveled to SDC/EC
# https://www.billingsclinic.com/maps-locations/search-results/?termId=50a19986-c81c-e411-903e-2c768a4e1b84&sort=13&page=1

building_coords <- data.frame(
  Building = character(),
  lat = numeric(),
  long = numeric()
)

# https://www.r-bloggers.com/2020/02/three-ways-to-calculate-distances-in-r/
sf::st_distance()

# add rucc codes - https://www.ers.usda.gov/data-products/rural-urban-continuum-codes.aspx

# create wide and long data frames
df.flat.sdc.only <- df.flat %>% 
  select(
    -ActiveIndicatorCD
  ) %>% 
  filter(
    NurseUnit %in% SDCEC
  )

df.long <- df.processed %>% 
  filter(
    NurseUnit %in% SDCEC
  ) %>% 
  left_join(
    pancake.stack$Location,
    by = c("street", "city", "state")
  )


# MAKE WIDE AND LONG OUTPUT ####
df.flat <- df.processed %>% 
  group_by(
    PersonID,
    EncounterID,
    DTS
  ) %>% 
  arrange(
    desc(DiagnosisPrioritySEQ)
  ) %>% 
  slice(1) %>% 
  ungroup()



# write final df to file for later use. ####

writexl::write_xlsx(df.flat.sdc.only,
                    path = file.path(out.dir.path, "sdc.flat.xlsx"))
