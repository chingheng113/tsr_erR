rm(list=ls())
cat("\014")
library(glmnet)
library(dplyr)
library(pROC)
training_data = read.csv("training_data_processed.csv", header = TRUE)
training_id_data <- dplyr::select(training_data, ICASE_ID, IDCASE_ID)
training_y_data <- dplyr::select(training_data, ICD_ID)
training_X_data <- dplyr::select(training_data, -one_of(colnames(training_id_data)), -one_of(colnames(training_y_data)))

testing_data = read.csv("testing_data_processed.csv", header = TRUE)
testing_id_data <- dplyr::select(testing_data, ICASE_ID, IDCASE_ID)
testing_y_data <- dplyr::select(testing_data, ICD_ID)
testing_X_data <- dplyr::select(testing_data, -one_of(colnames(testing_id_data)), -one_of(colnames(testing_y_data)))


tr_y = training_y_data[['ICD_ID']]
tr_x = as.matrix(training_X_data)
#glmmod <- glmnet(tr_x, tr_y, alpha=1, family="binomial")
#plot(glmmod, xvar="lambda", label = TRUE)
set.seed(1011)
cv.glmmod <- cv.glmnet(tr_x, tr_y, alpha=1, nfold=10, family="binomial", type.measure='auc')
plot(cv.glmmod)
# feature selection
coefs <- as.matrix(coef(cv.glmmod, s = "lambda.1se"))
coefs <- as.matrix(coefs[-1,]) # Remove Intercept...
selected_coefs <- subset(coefs, coefs >0)

te_x = as.matrix(testing_X_data)
te_y = testing_y_data[['ICD_ID']]
pre_y <- predict(cv.glmmod, te_x, s = "lambda.min", type = "response")
roc_obj <- roc(te_y, as.numeric(pre_y))
auc(roc_obj)