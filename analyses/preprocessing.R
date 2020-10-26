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
library(readxl)
library(tidycensus)


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
    ),
    Race = if_else(
      Race == "Native American",
      "Native American/Native Alaskan",
      Race
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
    street = toupper(street),
    city = toupper(city),
    state = toupper(state)
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
  select(
    PersonID,
    EncounterID,
    DTS,
    ICD_code,
    DiagnosisPrioritySEQ
  ) %>% 
  group_by(
    PersonID,
    EncounterID,
    DTS
  ) %>%
  arrange(
    desc(DiagnosisPrioritySEQ)
  ) %>% 
  mutate(
    ICD_all = paste(ICD_code, collapse = "; ")
  ) %>% 
  ungroup() %>% 
  select(
    -c(ICD_code, DiagnosisPrioritySEQ)
  ) %>% 
  left_join(
    df.processed,
    by = c("PersonID", "EncounterID", "DTS")
  ) %>% 
  distinct()

df.processed <- df.processed %>% 
  distinct() %>% 
  select(
    PersonID,
    EncounterID,
    DTS,
    ICD_block,
    DiagnosisPrioritySEQ
  ) %>% 
  group_by(
    PersonID,
    EncounterID,
    DTS
  ) %>%
  arrange(
    desc(DiagnosisPrioritySEQ)
  ) %>% 
  mutate(
    ICD_block_all = paste(ICD_block, collapse = "; ")
  ) %>% 
  ungroup() %>% 
  select(
    -c(ICD_block, DiagnosisPrioritySEQ)
  ) %>% 
  left_join(
    df.processed,
    by = c("PersonID", "EncounterID", "DTS")
  ) %>% 
  distinct()

df.processed <- df.processed %>% 
  select(
    PersonID,
    EncounterID,
    DTS,
    ReasonForVisit
  ) %>% 
  distinct() %>% 
  filter(
    !is.na(ReasonForVisit)
  ) %>% 
  group_by(
    PersonID,
    EncounterID,
    DTS
  ) %>% 
  mutate(
    ReasonForVisit_all = paste(ReasonForVisit, collapse = "; ")
  ) %>% 
  ungroup() %>% 
  select(
    -c(ReasonForVisit)
  ) %>% 
  right_join(
    df.processed,
    by = c("PersonID", "EncounterID", "DTS")
  ) %>% 
  distinct()

df.processed <- df.processed %>% 
  select(
    PersonID,
    EncounterID,
    DTS,
    DiagnosisDSC
  ) %>% 
  distinct() %>% 
  filter(
    !is.na(DiagnosisDSC)
  ) %>% 
  group_by(
    PersonID,
    EncounterID,
    DTS
  ) %>% 
  mutate(
    DiagnosisDSC_all = paste(DiagnosisDSC, collapse = "; ")
  ) %>% 
  ungroup() %>% 
  select(
    -c(DiagnosisDSC)
  ) %>% 
  right_join(
    df.processed,
    by = c("PersonID", "EncounterID", "DTS")
  ) %>% 
  distinct()

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
Encounters <- df.processed %>% 
  select(
    DTS,
    PersonID,
    EncounterID,
    Building,
    NurseUnit,
    AdmitType,           
    EncounterType,
    DiagnosisDSC_all,
    ReasonForVisit_all,
    ICD_all,
    ICD_block_all
  ) %>% 
  distinct() %>% 
  group_by(
    PersonID
  ) %>% 
  arrange(
    PersonID,
    DTS,
    EncounterID
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
    return_ICD_all = if_else(
      PersonID == lead(PersonID) & 
        DTS != lead(DTS),
      lead(ICD_all),
      NULL
    ),
    return_ICD_block_all = if_else(
      PersonID == lead(PersonID) & 
        DTS != lead(DTS),
      lead(ICD_block_all),
      NULL
    ),
    return_DiagnosisDSC_all = if_else(
      PersonID == lead(PersonID) & 
        DTS != lead(DTS),
      lead(DiagnosisDSC_all),
      NULL
    ),
    return_ReasonForVisit_all = if_else(
      PersonID == lead(PersonID) & 
        DTS != lead(DTS),
      lead(ReasonForVisit_all),
      NULL
    ),
    return_Building = if_else(
      PersonID == lead(PersonID) & 
        DTS != lead(DTS),
      lead(Building),
      NULL
    ),
    return_NurseUnit = if_else(
      PersonID == lead(PersonID) & 
        DTS != lead(DTS),
      lead(NurseUnit),
      NULL
    ),
    days_to_return = if_else(
      PersonID == lead(PersonID) & 
        DTS != lead(DTS),
      round((lead(unclass(DTS))-unclass(DTS))/(14*3600), 1),
      NULL
    )
  )

# Select SDC/EC only visits
Encounters <- Encounters %>% 
  filter(
    NurseUnit %in% SDCEC
  )

Addresses <- df.processed %>% 
  filter(
    po_box != TRUE
  ) %>% 
  select(
    street,
    city,
    state,
    ZipCode,
    country
  ) %>% 
  distinct()

# get coords for facilities
building_coords <- read_xlsx(file.path(
  data.dir.path,
  "locations.xlsx"
)) %>% 
  geocode(
    address = address,
    method = 'cascade',
    return_type = 'geographies',
    unique_only = FALSE
  ) 

# add home coordinates and GEOID ####

Addresses <- Addresses %>% geocode(
  street = street,
  city = city,
  state = state,
  country = country,
  postalcode = ZipCode,
  method = 'cascade',
  mode = 'single'
)

addresses_census <- Addresses[Addresses$geo_method == 'census', ] %>% 
  select(
    -c(lat, long)
  ) %>% 
  geocode(
          street = street,
          city = city,
          state = state,
          country = country,
          postalcode = ZipCode,
          method = 'census',
          full_results = TRUE,
          return_type = 'geographies',
          mode = 'single'
  )

Addresses <- addresses_census %>% right_join(
    Addresses,
    by = c("street", "city", "country", "ZipCode")
  )

# extract GEOID for census tract and FIPS numbers
# GEOID is state_fips + county+fips + census_tract + census_block

Addresses$entry <- c(1:nrow(Addresses))

getgeoid <- function(x){
  id <- Addresses$`geographies.2010 Census Blocks`[[x]]$GEOID[1]
  if (is.null(id)){
    return("NA")
  } else {
    return(id)
  }
}

Addresses$rows <- c(1:nrow(Addresses))

Addresses$geoid_block <- sapply(Addresses$rows, getgeoid)
Addresses <- Addresses %>% 
  mutate(
    geoid_tract = gsub(
      "(\\d{11})\\d{4}",
      "\\1",
      geoid_block
    )
  )

Addresses <- Addresses %>% 
  select(
    street,
    city,
    state = state.x,
    ZipCode,
    country,
    lat = lat.y,
    long = long.y,
    geo_method = geo_method.y,
    geoid_block,
    geoid_tract
  ) 

Addresses <- Addresses %>% 
  mutate(
    geoid_block = na_if(geoid_block, "NA"),
    geoid_tract = na_if(geoid_tract, "NA")
  ) %>% 
  mutate(
    geoid_block = unlist(geoid_block), 
    geoid_tract = unlist(geoid_tract)
  ) %>% 
  mutate(
    state_fips = gsub(
      "(\\d{2}).*",
      "\\1",
      geoid_block
    ),
    county_fips = gsub(
      "\\d{2}(\\d{3}).*",
      "\\1",
      geoid_block
    )
  )

# add median household income data, and median home value.
state_fips <- unique(Addresses$state_fips) %>% 
  as.numeric() %>% na.omit()

county_fips <- unique(Addresses$county_fips) %>% 
  as.numeric() %>% na.omit()


tract_data <- get_acs(geography = "tract", 
                         variables = c(median_income_2018 = "B06011_001", median_home_value_2018 = "B25077_001"), 
                         state = state_fips, 
                         county = county_fips
                      ) %>% 
  select(-moe) %>% 
  pivot_wider(
    names_from = variable,
    values_from = estimate
  )

tract_data$GEOID <- as.character(tract_data$GEOID)

Addresses <- Addresses %>% 
  left_join(
    tract_data,
    by = c("geoid_tract" = "GEOID")
  )

# combine addresses and building locations with Encounters
df.processed <- df.processed %>% 
  left_join(
    Addresses,
    by = c(
      "street",
      "city",
      "state",
      "ZipCode",
      "country"
    )
  ) %>% 
  left_join(
    building_coords,
    by = c("Building"),
    suffix = c("_pt", "_building")
  )

# compute distance traveled to SDC/EC and remove lat/long for pts and buildings
# https://www.billingsclinic.com/maps-locations/search-results/?termId=50a19986-c81c-e411-903e-2c768a4e1b84&sort=13&page=1
# get_distance <- function(lat_pt, long_pt, lat_building, long_building){
#   
#   distance <- raster::pointDistance(
#     c(long_pt, lat_pt),
#     c(long_building, lat_building),
#     lonlat = TRUE
#     )
#     
#   return(distance)
# }
# 
# The below for loop is super slow (it takes ~10 hours). Try below options in future.
# try distGeo() from geosphere
# can also use Euclidian distance, sqrt((lat-lat)^2 + (long-long)^2)

for (index in 1:nrow(df.processed)){
  df.processed$distance_travelled[index] <-
    raster::pointDistance(
      c(df.processed$long_pt[index],df.processed$lat_pt[index]),
      c(df.processed$long_building[index], df.processed$lat_building[index]),
      lonlat = TRUE
    )/1000

}


# add rucc codes - https://www.ers.usda.gov/data-products/rural-urban-continuum-codes.aspx
rucc <- read_xls(
  file.path(data.dir.path, "ruralurbancodes2013.xls")
)

df.processed <- df.processed %>% 
  mutate(
    FIPS = paste(state_fips, county_fips, sep="")
  ) %>% 
  left_join(
    rucc[c("FIPS", "RUCC_2013")],
    by = "FIPS"
  )


# bind Encounters for return variables

df.processed <- df.processed %>% 
  left_join(
    Encounters[c("DTS", 
                 "PersonID", 
                 "EncounterID",
                 "return_in_14",
                 "return_Building", 
                 "return_NurseUnit", 
                 "days_to_return", 
                 "return_DiagnosisDSC_all", 
                 "return_ReasonForVisit_all", 
                 "return_ICD_all", 
                 "return_ICD_block_all")],
    by = c("PersonID", "EncounterID", "DTS")
  ) %>% 
  filter(
    NurseUnit %in% SDCEC
  )

df.processed <- df.processed %>% 
  select(
    -street,
    -address,
    -lat_pt,
    -lat_building,
    -long_pt,
    -long_building
  )

df.processed.flat <- df.processed %>% 
  arrange(
    PersonID,
    DTS,
    EncounterID
  ) %>% 
  group_by(
    PersonID,
    EncounterID,
    DTS
  ) %>% 
  slice(1) %>% 
  ungroup()

# write final df to file for later use. ####

writexl::write_xlsx(df.processed.flat,
                    path = file.path(out.dir.path, "sdc.flat.xlsx"))

writexl::write_xlsx(df.processed,
                    path = file.path(out.dir.path, "sdc.long.xlsx"))

