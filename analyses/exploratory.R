# reproducing SDC
# environment ####
library(readxl)
library(dplyr)

# get data ####
WB <- file.path("C:",
                "Users",
                "CarteB",
                "OneDrive - BILLINGS CLINIC",
                "projects",
                "sdc",
                "data",
                "Same Day Care_Express Care 2014_2015 patients 3_25_2020-2.xlsx")
df <- read_xlsx(WB)

# preprocessing ####
df$Tribe <- na_if(df$Tribe, ".")

df.processed <- df %>%
  select(
    "id" = `Person MPI number`,
    "lungDx" = `Lung disease Diagnosis`,
    "diabetesDx" = `Diabetes Diagnosis`,
    "heartFailureDx" = `Heart Failure Diagnosis`,
    "sex" = Sex,
    "race" = Race,
    "ethnicity" = Ethnicity,
    "tribe" = Tribe,
    "age" = `Age in Years at Visit`,
    "SdcDate" = `SDC_EC Admit Date`,
    ICD9_SDC_EC,
    "Sdc_reason" = `SDC_EC Reason for Visit`,
    "SdcCollapsedReason" = `Collapsed Reason for visit`,
    "categorization" = `Categorization: Hospitalization, No hospitalization, Referred by SDC, Not noticed at SDC`,
    "SdcLocation" = `SDC_EC Location`,
    "Sdc_or_Ec" = `SDC or EC visit?`,
    "return_in_14" = `did pt have hospital visit within 14 days`,
    "PCP" = `Did patient have a PCP?`,
    "Encounter_type" = `Encounter Type`
  ) %>%
  mutate(
    lungDx = as.logical(lungDx),
    diabetesDx = as.logical(diabetesDx),
    heartFailureDx = as.logical(heartFailureDx),
    sex = as.factor(sex),
    race = as.factor(race),
    ethnicity = as.factor(ethnicity),
    tribe = as.factor(tribe),
    SdcCollapsedReason = as.factor(SdcCollapsedReason),
    categorization = as.factor(categorization),
    SdcLocation = as.factor(SdcLocation),
    Sdc_or_Ec = as.factor(Sdc_or_Ec),
    return_in_14 = as.logical(return_in_14),
    Encounter_type = as.factor(Encounter_type),
    age = log10(age), # log10'd because youngest and oldest differ by power of 10.
    PCP = ifelse(PCP = "Yes", TRUE, FALSE)
  )

# Exploring the data ####
df.processed %>%
  group_by(
    SdcCollapsedReason
  ) %>%
  summarise(
    n = n()
  )


# Reproducing Hedges ####
  # set explanatory and response variables ####
expl_vars <- c(
  "lungDx",
  "diabetesDx",
  "heartFailureDx",
  "sex",
  "race",
  "ethnicity",
  "age",
  "SdcDate",
  "ICD9_SDC_EC",
  "SdcCollapsedReason",
  "categorization",
  "SdcLocation",
  "Sdc_or_Ec",
  "PCP",
  "Encounter_type" 
)

resp_var <- "return_in_14"


  # balance data, bin into development, test and validation sets ####
balance_vars <- c(
  "lungDx",
  "diabetesDx",
  "heartFailureDx",
  "sex",
  "race",
  "ethnicity",
  "age",
  "SdcDate",
  "ICD9_SDC_EC" 
)

  # create models ####


# test and validate