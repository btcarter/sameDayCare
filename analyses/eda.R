# PREAMBLE
# Author: Benjamin T. Carter, PhD
# Objective:

# ENVIRONMENT ####
# packages
library(dplyr)
library(tidyr)
library(lubridate)
library(icd) # for reading in ICD10 classifying information and calculating risk scores
library(ggplot2)
library(compareGroups)

# paths
data.dir.path <- file.path("C:","Users","CarteB","BILLINGS CLINIC", 
                           "CSI & David Hedges - Same Day Care Project", "data")

out.dir.path <- file.path("C:","Users","CarteB","BILLINGS CLINIC", 
                          "CSI & David Hedges - Same Day Care Project", "results")

# data

df.flat.name <- file.path(data.dir.path, "sdc.flat.xlsx")

df.flat <- readxl::read_xlsx(df.flat.name)

ICD <- icd10cm2019 %>% 
  mutate(
    code = as.character(code)
  )

# ANALYSIS ####

# table 1 for all visits ####
df.comp <- df.flat %>% 
  mutate(
    return_in_14 = as.factor(return_in_14)
  ) %>% 
  select(
    AdmitAge,
    Marital_Status,
    Sex,
    NurseUnit,
    AdmitType,
    DiagnosisType,
    Race,
    Ethnicity,
    Religion,
    return_in_14
  )

comp.obj <- compareGroups(
  return_in_14 ~ .,
  df.comp,
  max.xlev = 55
)

createTable(comp.obj) %>% 
  export2xls(file = file.path(out.dir.path,
                              "table1-encounters.xlsx"))

# table for patients ####
df.comp <- df.flat %>% 
  group_by(
    PersonID
  ) %>% 
  slice(1) %>% 
  ungroup() %>% 
  mutate(
    return_in_14 = as.factor(return_in_14)
  ) %>% 
  select(
    AdmitAge,
    Marital_Status,
    Sex,
    Race,
    Ethnicity,
    Religion,
    return_in_14
  )

comp.obj <- compareGroups(
    return_in_14 ~ .,
    df.comp,
    max.xlev = 55
  )

createTable(comp.obj) %>% 
  export2xls(file = file.path(out.dir.path,
                              "table1-patients.xlsx"))


# most common Priority ICD10
df.icd.counts <- df.flat %>% 
  group_by(PriorityICD, return_in_14) %>% 
  summarise(
    n = n()
  ) %>% 
  ungroup() %>% 
  arrange(
    desc(return_in_14),
    desc(n)
  ) %>% 
  pivot_wider(
    names_from = return_in_14,
    values_from = n,
    values_fill = 0
  ) %>% 
  mutate(
    ratio = `TRUE`/`FALSE`,
    percentage = `TRUE`/(`FALSE` + `TRUE`)
  ) %>% 
  arrange(
    desc(percentage),
    desc(`TRUE`)
  )



# priority ICD-10 with most significant association with readmission
df.chi <- df.flat %>% 
  mutate(
    n = 1
  ) %>% 
  pivot_wider(
    names_from = PriorityICD,
    values_from = n,
    values_fill = 0
  )

df.chi.results <- data.frame(ICD = as.character(),
                             Chi2 = as.numeric(),
                             p = as.numeric())

for (i in seq(24, length(colnames(df.chi)))){
  chiTest <- chisq.test(
    df.chi$return_in_14,
    df.chi[[i]]
  )
  
  a <- colnames(df.chi)[i]
  
  res <- data.frame(ICD = a,
                    Chi2 = chiTest$statistic,
                    p = chiTest$p.value)
  
  df.chi.results <- rbind(df.chi.results, res)
}

row.names(df.chi.results) <- NULL

df.icd.sig <- df.icd.counts %>% 
  left_join(
    df.chi.results,
    by = c("PriorityICD" = "ICD")
  )


df.icd.counts <- NULL
df.chi.results <- NULL

# top 50 ICD-10 codes ####
top50 <- df.icd.sig %>% 
  arrange(
    desc(Chi2),
    p
  ) %>% 
  head(50) %>% 
  left_join(
    ICD[c(1,3,6:8)],
    by = c("PriorityICD" = "code")
  ) %>% 
  arrange(
    desc(ratio)
  )

writexl::write_xlsx(top50,
            path = file.path(out.dir.path,
                             "top50.xlsx"))

# plot: average number of visits ####
df.flat %>% 
  group_by(
    PersonID
  ) %>% 
  summarise(
    n = n()
  ) %>% 
  ggplot(
    aes(n)
  ) +
  geom_histogram() +
  scale_y_log10() +
  theme_classic() +
  labs(
    title = "Distribution of Average Number of Visits to SDC/EC",
    xlab = "Number of Visits",
    ylab = "Number of Individuals (log transformed)"
  ) %>% 
  ggsave(
    filename = file.path(out.dir.path,
                         "sdcDistribution.png")
  )

# plot: timeseries ####

df.flat %>% 
  ggplot(
    aes(DTS, fill=return_in_14)
  ) +
  geom_histogram() +
  theme_classic() +
  labs(
    title = "Visits to SDC/EC Timeseries",
    x = "Date",
    ylab = "Total Visits"
  )

# plot: Month
df.flat %>% 
  mutate(
    Month = month(DTS)
  ) %>% 
  ggplot(
    aes(Month, fill=return_in_14)
  ) +
  geom_histogram() +
  theme_classic() +
  labs(
    title = "Visits to SDC/EC Timeseries",
    x = "Month",
    ylab = "Total Visits"
  )

# plot: Weekday
df.flat %>% 
  mutate(
    Wday = wday(DTS)
  ) %>% 
  ggplot(
    aes(Wday, fill=return_in_14)
  ) +
  geom_histogram() +
  theme_classic() +
  labs(
    title = "Visits to SDC/EC Timeseries",
    x = "Week Day",
    ylab = "Total Visits"
  )

# # logistic model ####
# df.flat.model <- df.flat %>% 
#   select(
#     -c(DTS, 
#        ICD_block, 
#        ICD_code, 
#        PersonID, 
#        DiagnosisPrioritySEQ, 
#        Building, 
#        AdmitType, 
#        EncounterType, 
#        RespiratoryFailure,
#        DiagnosisDSC)
#   )
# 
# fmla <- "return_in_14 ~ ."
# sdc.model <- glm(fmla, 
#                  df.flat.model,
#                  family = "binomial"
#                  )
# 
