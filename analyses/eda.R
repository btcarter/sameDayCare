# PREAMBLE
# Author: Benjamin T. Carter, PhD
# Objective:

# ENVIRONMENT ####
# packages
library(dplyr)
library(icd) # for reading in ICD10 classifying information and calculating risk scores
library(ggplot2)

# paths
data.dir.path <- file.path("C:","Users","CarteB","BILLINGS CLINIC", 
                           "CSI & David Hedges - Same Day Care Project", "data")

out.dir.path <- file.path("C:","Users","CarteB","BILLINGS CLINIC", 
                          "CSI & David Hedges - Same Day Care Project", "data")

# data

df.flat.name <- file.path(data.dir.path, "sdc.flat.xlsx")

df.flat <- readxl::read_xlsx(df.flat.name)

ICD <- icd10cm2019

# variables




# ANALYSIS ####