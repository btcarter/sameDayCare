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
    ReasonForVisit = tolower(ReasonForVisit),
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
    ),
    Religion = if_else(
      is.na(Religion),
      'Unknown',
      Religion
    ),
    Religion_binary = if_else(
      Religion %in% c("Unknown", "No religious affiliation"),
      "Undeclared",
      "Declared"
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
      lead(unclass(DTS))-unclass(DTS) <= 14*24*3600 & 
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
      round((lead(unclass(DTS))-unclass(DTS))/(24*3600), 1),
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

# get_geo <- function(x){
#   URL <- paste("https://geocoding.geo.census.gov/geocoder/geographies/address?street=",
#         street,"4600+Silver+Hill+Rd"
#         "&city=",
#         city,
#         "&state=",
#         state,
#         "&benchmark=Public_AR_Census2010&vintage=Census2010_Census2010&layers=14&format=json")
#   info <- jsonlite::read_json(path = URL, simplifyVector = TRUE)
#   census_block,
#   fips
#   lat
#   long
# }

ROWS <- nrow(Addresses)
group_size <- 5000
iters <- ROWS/group_size
Addresses_geocoded <- data.frame()

for (i in 1:iters){
  range <- (i*group_size-group_size)+(1:group_size)
  Addresses_geocoded <- Addresses[c(range),] %>%
    geocode(
      street = street,
      city = city,
      state = state,
      country = country,
      postalcode = ZipCode,
      method = 'census',
      full_results = TRUE,
      return_type = 'geographies',
      unique_only = FALSE
    ) %>% 
    rbind(Addresses_geocoded)
  
  Sys.sleep(3)
}

range <- c(nrow(Addresses_geocoded):nrow(Addresses))
Addresses_geocoded <- Addresses[c(range),] %>%
  geocode(
    street = street,
    city = city,
    state = state,
    country = country,
    postalcode = ZipCode,
    method = 'census',
    full_results = TRUE,
    return_type = 'geographies'
  ) %>% 
  rbind(Addresses_geocoded)

Addresses_geocoded <- Addresses_geocoded %>% 
  filter(
    !(street %in% c("NA  ", "UNKNOWN", "UNKNOWN ", "UPDATE "))
  )

# make GEOID for census tract and FIPS numbers
# GEOID is state_fips (2 digits) + county_fips (3 digits) 
# + census_tract (6 digits) + census_block (4 digits)

add_zeros <- function(x, z){
  n <- nchar(x)
    while(n<z){
      x <- paste("0", x, sep="")
      n <- nchar(x)
    }
    return(x)
}

Addresses_geocoded <- Addresses_geocoded %>% 
  filter(
    !is.na(state_fips)
  ) %>% 
  mutate(
    state_fips = as.character(state_fips),
    county_fips = as.character(county_fips),
    census_tract = as.character(census_tract),
    census_block = as.character(census_block)
  ) 

Addresses_geocoded$state_fips <- 
  sapply(Addresses_geocoded$state_fips, add_zeros, z=2)
Addresses_geocoded$county_fips <- 
  sapply(Addresses_geocoded$county_fips, add_zeros, z=3)
Addresses_geocoded$census_tract <- 
  sapply(Addresses_geocoded$census_tract, add_zeros, z=6)
Addresses_geocoded$census_block <- 
  sapply(Addresses_geocoded$census_block, add_zeros, z=4)


Addresses_geocoded <- Addresses_geocoded %>% 
  mutate(
    geoid_block = paste(state_fips, county_fips, census_tract, census_block,sep = ""),
    geoid_tract = paste(state_fips, county_fips, census_tract,sep = "")
  )

# add median household income data, and median home value.
state_fips <- unique(Addresses_geocoded$state_fips) %>% 
  as.numeric() %>% na.omit()

county_fips <- unique(Addresses_geocoded$county_fips) %>% 
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

Addresses_geocoded <- Addresses_geocoded %>% 
  left_join(
    tract_data,
    by = c("geoid_tract" = "GEOID")
  )

# combine addresses and building locations with Encounters for both SDC and return locations

Encounters.geocoded <- Encounters %>% 
  left_join(
    building_coords,
    by = "Building"
  ) %>% 
  left_join(
    building_coords,
    by = c("return_Building" = "Building"),
    suffix = c("_sdc", "_return")
  )

# add participant addresses
df.processed.geocoded <- df.processed %>% 
  left_join(
    Addresses_geocoded,
    by = c(
      "street",
      "city",
      "state",
      "ZipCode",
      "country"
    )
  ) %>% 
  left_join(
    Encounters.geocoded[c("PersonID",
                          "EncounterID",
                          "DTS",
                          "Building",
                          "NurseUnit",
                          "return_in_14",
                          "return_ICD_all",
                          "return_ICD_block_all",
                          "return_DiagnosisDSC_all",
                          "return_ReasonForVisit_all",
                          "return_Building",
                          "return_NurseUnit",
                          "days_to_return",
                          "lat_sdc",
                          "long_sdc",
                          "lat_return",
                          "long_return")],
    by = c("PersonID", "EncounterID", "DTS", "NurseUnit", "Building")
  ) %>% 
  select(
    -c(input_address, match_indicator, match_type, matched_address, tiger_line_id, tiger_side)
  ) %>% 
  filter(
    NurseUnit %in% SDCEC
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

# for (index in 1:nrow(df.processed)){
#   df.processed$distance_travelled[index] <-
#     raster::pointDistance(
#       c(df.processed$long_pt[index],df.processed$lat_pt[index]),
#       c(df.processed$long_building[index], df.processed$lat_building[index]),
#       lonlat = TRUE
#     )/1000
# 
# }

# Euclidian distance between locations

df.processed.geocoded <- df.processed.geocoded %>% 
  mutate(
    dist_eu_pt_sdc = sqrt((lat-lat_sdc)^2 + (long-long_sdc)^2),
    dist_eu_pt_return = sqrt((lat-lat_return)^2 + (long-long_return)^2)
  )

# add rucc codes - https://www.ers.usda.gov/data-products/rural-urban-continuum-codes.aspx
rucc <- read_xls(
  file.path(data.dir.path, "ruralurbancodes2013.xls")
)

df.processed.geocoded <- df.processed.geocoded %>% 
  select(
    DTS,
    PersonID,
    EncounterID,
    state_fips,
    county_fips
  ) %>% 
  filter(
    !is.na(state_fips)
  ) %>% 
  mutate(
    FIPS = paste(state_fips, county_fips, sep="")
  ) %>% 
  left_join(
    rucc[c("FIPS", "RUCC_2013")],
    by = "FIPS"
  ) %>% 
  right_join(
    df.processed.geocoded,
    by = c("PersonID", "EncounterID", "DTS", "state_fips", "county_fips")
  )

# deidentify
df.processed.geocoded <- df.processed.geocoded %>% 
  select(
    -street,
    -lat,
    -lat_sdc,
    -long,
    -long_sdc,
    -lat_return,
    -long_return,
    -AppointmentID,
    -ActiveIndicatorCD
  )

df.processed.flat <- df.processed.geocoded %>% 
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

writexl::write_xlsx(df.processed.geocoded,
                    path = file.path(out.dir.path, "sdc.long.xlsx"))

