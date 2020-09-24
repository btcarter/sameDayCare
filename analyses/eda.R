# PREAMBLE
# Author: Benjamin T. Carter, PhD
# Objective:  Explore the data for the SDC project. 
#             Generate objects to build the descriptive analysis.

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

# AIMS ####
# The aims of this analysis are as follows
# - How much traffic are we seeing in SDC/EC?
# - What are the top ICD-10 codes associated with a readmission?
# - Who returned?
  # - What relationship does this have with our predictors?
# - Who returned more than once (frequent fliers)?
  # - How many times are they coming back?
  # - How often are they coming back?
  # - Why are they coming back?
  # - Where/who are they returning too?
  # - Which sites are they coming from?
# - Can we predict return?

# ANALYSIS ####

# Aim 1 What does SDC/EC traffic look like? ####

# plot: number of visits to SDC/EC
traffic <- df.flat %>% 
  group_by(
    PersonID
  ) %>% 
  summarise(
    n = n()
  ) %>% 
  ungroup()

traffic.plot <- traffic %>% 
  ggplot(
    aes(n)
  ) +
  geom_histogram() +
  scale_y_log10() +
  theme_classic() +
  labs(
    title = "Distribution of Number of Visits to SDC/EC",
    x = "Number of Visits",
    y = "Number of Individuals"
  )

traffic.plot %>% 
  ggsave(
    filename = file.path(out.dir.path,
                         "sdcDistribution.png")
  )

# plot: timeseries
traffic.tsplot <- df.flat %>% 
  ggplot(
    aes(DTS, fill=return_in_14)
  ) +
  geom_histogram(bins = 36) +
  theme_classic() +
  labs(
    title = "Visits to SDC/EC Timeseries",
    x = "Date",
    ylab = "Total Visits"
  )

traffic.tsplot %>% 
  ggsave(
    filename = file.path(out.dir.path,
                         "sdcTimeSeries.png")
  )

# table 1 for all visits
df.comp <- df.flat %>% 
  mutate(
    return_in_14 = as.factor(return_in_14)
  ) %>% 
  select(
    AdmitAge,
    Marital_Status,
    Sex,
    Building,
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

comp.obj.table.encounters <- createTable(comp.obj)

comp.obj.table.encounters %>% export2xls(file = file.path(out.dir.path,
                              "table1-encounters.xlsx"))

## How are individual predictors related to returning?

cat_vars <- c("Ethnicity",
              "Language",
              "Race",
              "Marital_Status",
              "Sex",
              "Religion",
              "Building",
              "NurseUnit",
              "AdmitType",
              "EncounterType",
              "RespiratoryFailure",
              "ZipCode",
              "DiagnosisDSC")

CatChi <- data.frame(
  variable = as.character(),
  Xsq = as.numeric(),
  df = as.numeric(),
  p = as.numeric(),
  row.names = NULL
)

CatChiList <- list()

for (cat in cat_vars){
  res <- chisq.test(
    df.flat[[cat]],
    df.flat$return_in_14
  )
  
  if (res$p.value < 0.05){
    res.df <- data.frame(
      variable = cat,
      Xsq = res$statistic,
      df = res$parameter,
      p = res$p.value,
      row.names = NULL
    )
    
    CatChi <- rbind(CatChi, res.df)
    
    CatChiList[[cat]] <- res
    
    residuals <- data.frame(res$residuals) %>% 
      rename(
        Residual = Freq
      )
    
    Zscores <- data.frame(100*res$residuals^2/res$statistic) %>% 
      rename(
        Z = Freq
      ) %>% 
      mutate(
        Significant = if_else(
          abs(Z) > 1.96,
          TRUE,
          FALSE
        )
      )
    
    residuals <- residuals %>% 
      left_join(Zscores)
    
    CatChiList[[paste(cat,"_plot", sep = "")]] <- residuals %>% 
      ggplot(
        aes(
          df.flat.return_in_14,
          df.flat..cat..,
          size = Z,
          shape = Significant,
          color = Residual
        )
      ) +
      geom_point(alpha = 0.7) +
      theme_classic() +
      scale_color_gradient2(low = "#0000ff", mid = "#008000", high = "#ff0000") +
      labs(
        title = paste(cat, "Significance Plot"),
        y = cat,
        x = "Returned in 14 days"
      )

  }

}

# Aim 2 What are the top ICD-10 codes associated with readmission? ####
## most common Priority ICD10
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



## priority ICD-10 with most significant association with readmission
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

## top 50 ICD-10 codes
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

# Aim 3 Describing patients who returned ####

df.comp <- df.flat %>%
  group_by(
    PersonID
  ) %>% 
  summarise(
    `Total Visits` = n(),
    `Visit Frequency` = n()/3,
    Returned = if_else(sum(return_in_14) > 0,
                     TRUE,
                     FALSE),
    `Return Count` = sum(return_in_14)
  ) %>% 
  ungroup() %>% 
  left_join(
    df.flat,
    by = "PersonID"
  ) %>% 
  group_by(
    PersonID
  ) %>% 
  slice(1) %>% 
  ungroup() %>% 
  mutate(
    Returned = as.factor(Returned)
  ) %>% 
  select(
    PersonID,
    `Total Visits`,
    `Visit Frequency`,
    `Return Count`,
    AdmitAge,
    Marital_Status,
    Sex,
    Race,
    Ethnicity,
    Religion,
    Returned
  )

## plot of visit frequency
df.comp %>% 
  ggplot(
    aes(`Visit Frequency`, fill = Sex)
    ) +
  geom_histogram() +
  scale_y_log10() +
  theme_classic() +
  labs(
    title = "Sex and Visit Frequency",
    y = "Log of Patient Count"
    )

## plot of total visits and sex
df.comp %>% 
  ggplot(
    aes(`Total Visits`, fill = Sex)
  ) +
  geom_histogram(binwidth = 1) +
  scale_y_log10() +
  theme_classic() +
  labs(
    title = "Sex and Total Visits",
    y = "Log of Patient Count"
  )

## table for patient demographics
comp.obj <- compareGroups(
  Returned ~ . - PersonID,
    df.comp,
    max.xlev = 55
  )

comp.obj.table.patients <- createTable(comp.obj)

comp.obj.table.patients

comp.obj.table.patients %>% export2xls(file = file.path(out.dir.path,
                              "table1-patients.xlsx"))




# Aim 4 Describing frequent fliers ####
# how to identify?
a <- round(nrow(df.comp)*0.05)

df.comp.top5percent <- df.comp %>% 
  arrange(
    desc(
      `Total Visits`
    )
  ) %>% 
  head(a)

df.comp.5orMore <- df.comp %>% 
  filter(
    `Total Visits` >= 5
  )

ff.comp <- compareGroups(
  Returned ~ . -PersonID,
  df.comp.5orMore, max.xlev = 55)

ff.comp.table <- createTable(ff.comp)
ff.comp.table
ff.comp.table %>% export2xls(file = file.path(out.dir.path,
                                              "table-frequentFlier.xlsx"))

df.comp %>% 
  ggplot(
    aes(`Return Count`,`Total Visits`)
  ) +
  geom_jitter() +
  geom_smooth(method = "lm")


# Aim N: Can we predict a return? ####
## logistic model
df.flat.model <- df.flat %>%
  select(
    -c(DTS,
       ICD_block,
       ICD_code,
       PersonID,
       DiagnosisPrioritySEQ,
       Building,
       AdmitType,
       EncounterType,
       RespiratoryFailure,
       DiagnosisDSC)
  )

fmla <- "return_in_14 ~ ."
sdc.model <- glm(fmla,
                 df.flat.model,
                 family = "binomial"
                 )

