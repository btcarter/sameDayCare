# PREAMBLE
# Author: Benjamin T. Carter, PhD
# Objective:

# ENVIRONMENT ####
# packages
library(dplyr)
library(icd) # for reading in ICD10 classifying information and calculating risk scores
library(ggplot2)
library(compareGroups)

# paths
data.dir.path <- file.path("C:","Users","CarteB","BILLINGS CLINIC", 
                           "CSI & David Hedges - Same Day Care Project", "data")

out.dir.path <- file.path("C:","Users","CarteB","BILLINGS CLINIC", 
                          "CSI & David Hedges - Same Day Care Project", "data")

# data

df.flat.name <- file.path(data.dir.path, "sdc.flat.xlsx")

df.flat <- readxl::read_xlsx(df.flat.name)

ICD <- icd10cm2019

# ANALYSIS ####

# demographics
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

createTable(comp.obj)


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


top50 <- df.icd.sig %>% 
  arrange(
    desc(ratio),
    p
  ) %>% 
  head(50)

bottom50 <- df.icd.sig %>% 
  arrange(
    ratio,
    p
  ) %>% 
  head(50)


top50.filtered <- df.icd.sig %>% 
  filter(
    `TRUE` + `FALSE` >= 100
  ) %>% 
  arrange(
    desc(ratio),
    p
  ) %>% 
  head(50)







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
