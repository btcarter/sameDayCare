# This is a script for the ML portion of the SDC project
# Author: Benjamin T. Carter, PhD

# ENVIRONMENT ####

# packages
library(mltools) # library for encoding?
library(xgboost)
library(dplyr)
library(caret)

  # paths
data.dir.path <- file.path("C:",
                           "Users",
                           "CarteB",
                           "BILLINGS CLINIC", 
                           "CSI & David Hedges - Same Day Care Project", 
                           "data")

out.dir.path <- file.path("C:",
                          "Users",
                          "CarteB",
                          "BILLINGS CLINIC", 
                          "CSI & David Hedges - Same Day Care Project", 
                          "results")

  # data

df.name <- file.path(data.dir.path, "sdc.long.xlsx")

df <- readxl::read_xlsx(df.name)

df <- df %>% 
  mutate(
    return_in_14 = as.numeric(return_in_14)
  ) %>% 
  select(
    -c(ICD, 
       ICD_code, 
       ICD_block, 
       ICD_all, 
       ICD_block_all, 
       NAME, 
       Fed_Ethnicity,
       city,
       state,
       ZipCode,
       census_block,
       census_tract,
       geoid_block,
       Building,
       NurseUnit,
       return_ICD_all,
       return_ICD_block_all,
       return_DiagnosisDSC_all,
       ReasonForVisit,
       ReasonForVisit_all,
       DiagnosisPrioritySEQ,
       DiagnosisType,
       DiagnosisDSC,
       days_to_return,
       return_Building,
       return_NurseUnit,
       dist_eu_pt_return,
       id,
       FIPS,
       EncounterID,
       PersonID,
       DTS,
       state_fips,
       county_fips,
       DiagnosisDSC_all,
       return_ReasonForVisit_all,
       geoid_tract,
       country,
       Religion)
  )


# One-hot encoding ####

dummy <- dummyVars(" ~ .", data=df)
newdata <- data.frame(predict(dummy, newdata = df))



# # xgboost ####
# 
#   # test and training data & labels
# test <- list()
# 
# train <- list()
# 
#   # train model
# xg.model <- xgboost(
#   data = train$data,
#   label = train$label,
#   max.depth = 2,
#   eta = 1,
#   nthread = 2,
#   nrounds = 2,
#   objective = "binary:logistic"
# )
# 
#   # test model
# 
# # save the output for insertion in report ####

df <- newdata

# David's code ####

# Balance the Dataset

# Create Training Data

input_ones <- df[which(df$return_in_14 == 1), ] # all 1's
input_zeros <- df[which(df$return_in_14 == 0), ] # all 0's
nrow(input_ones)
nrow(input_zeros)

set.seed(100) # for repeatability of samples

input_ones_training_rows <- sample(1:nrow(input_ones), 0.7*nrow(input_ones)) # 1's for training
input_zeros_training_rows <- sample(1:nrow(input_zeros), 0.7*nrow(input_ones)) # 0's for training. Pick as many 0's as 1's
training_ones <- input_ones[input_ones_training_rows, ]
training_zeros <- input_zeros[input_zeros_training_rows, ]
X_train <- rbind(training_ones, training_zeros) # row bind the 1's and 0's

# Create Test Data
test_ones <- input_ones[-input_ones_training_rows, ]
test_zeros <- input_zeros[-input_zeros_training_rows, ]
X_test <- rbind(test_ones, test_zeros) # row bind the 1's and 0's

# Shuffle
X_train <- X_train[sample(nrow(X_train)),]
X_test <- X_test[sample(nrow(X_test)),]

# Extract y
y_train <- X_train %>%
  select(return_in_14)

y_test <- X_test %>%
  select(return_in_14)



# Drop y from X
label_drop <- c('return_in_14')

X_train <- X_train[ , !(names(X_train) %in% label_drop)]
X_test <- X_test[ , !(names(X_test) %in% label_drop)]

nrow(X_train)

nrow(y_train)

# Prepare Matrix

X_train <- as.matrix(X_train)
X_test <- as.matrix(X_test)

y_train <- as.matrix(y_train)
y_test <- as.matrix(y_test)

# train model
xg.model <- xgboost(
  data = X_train,
  label = y_train,
  max.depth = 10,
  eta = 0.1,
  # nthread = 2,
  nrounds = 30,
  objective = "binary:logistic",
  eval_metric = 'auc'
)

# evaluate
pred_prob_train <- predict(xg.model, X_train)
pred_prob_test <- predict(xg.model, X_test)

# AUC
library(ROCR)
pred_ROCR <- prediction(pred_prob_train, y_train[,1])
auc_ROCR <- performance(pred_ROCR, measure = 'auc')
auc_ROCR <- auc_ROCR@y.values[[1]]
auc_ROCR
pred_ROCR <- prediction(pred_prob_test, y_test[,1])
auc_ROCR <- performance(pred_ROCR, measure = 'auc')
auc_ROCR <- auc_ROCR@y.values[[1]]
auc_ROCR
# Accuracy
pred_ROCR <- prediction(pred_prob_train, y_train[,1])
acc_ROCR <- performance(pred_ROCR, measure = 'acc')
acc_ROCR <- max(acc_ROCR@y.values[[1]])
acc_ROCR
pred_ROCR <- prediction(pred_prob_test, y_test[,1])
acc_ROCR <- performance(pred_ROCR, measure = 'acc')
acc_ROCR <- max(acc_ROCR@y.values[[1]])
acc_ROCR

# take a peak at deciding factors
mat <- xgb.importance(feature_names=colnames(X_train), model=xg.model)
xgb.plot.importance (importance_matrix = mat[1:32])







