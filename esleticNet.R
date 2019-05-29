rm(list=ls())
cat("\014")
library(glmnet)
library(dplyr)
library(pROC)
training_data <- read.csv("training_data_og.csv", header = TRUE)
training_data <- training_data[sample(1:nrow(training_data)), ]
training_id_data <- dplyr::select(training_data, ICASE_ID, IDCASE_ID)
training_y_data <- dplyr::select(training_data, ICD_ID)
training_X_data <- dplyr::select(training_data, -one_of(colnames(training_id_data)), -one_of(colnames(training_y_data)))

testing_data = read.csv("testing_data_og.csv", header = TRUE)
testing_id_data <- dplyr::select(testing_data, ICASE_ID, IDCASE_ID)
testing_y_data <- dplyr::select(testing_data, ICD_ID)
testing_X_data <- dplyr::select(testing_data, -one_of(colnames(testing_id_data)), -one_of(colnames(testing_y_data)))


tr_y = training_y_data[['ICD_ID']]
tr_x = as.matrix(training_X_data)
validate_size <- floor(dim(tr_x)[1]*0.3)


list.of.fits <- list()
for (i in 0:10){
  fit.name <- paste0("alpha", i/10)
  list.of.fits[[fit.name]] <- cv.glmnet(tr_x[0:validate_size, ], tr_y[0:validate_size], alpha=i/10, 
                                        nfold=10, family="binomial", 
                                        type.measure='deviance', standardize=TRUE) 
}
#
results <- data.frame()
for(i in 0:10){
  fit.name <- paste0("alpha", i/10)
  predicted <- predict(list.of.fits[[fit.name]], 
                       s=list.of.fits[[fit.name]]$lambda.1se, 
                       newx=tr_x[(validate_size+1):dim(tr_x)[1],]
                       )
  mse <- mean((tr_y[(validate_size+1):dim(tr_x)[1]]-predicted)^2)
  temp <- data.frame(alpha=i/10, mse=mse, fit.name=fit.name)
  results <- rbind(results, temp)
}

best.fit.name <- results[results$mse == min(results$mse),]$fit.name
best.model <- list.of.fits[[best.fit.name]]
plot(best.model)
# feature selection
coefs <- as.matrix(coef(best.model, s = "lambda.min"))
coefs <- as.matrix(coefs[-1,]) # Remove Intercept...
sorted.coefs <- coefs[order(coefs),]

# prediction
te_x = as.matrix(testing_X_data)
te_y = testing_y_data[['ICD_ID']]
pre_y <- predict(best.model, te_x, s = "lambda.min", type = "response")
roc_obj <- roc(te_y, as.numeric(pre_y))
auc(roc_obj)

write.csv(pre_y,'pre_y.csv')