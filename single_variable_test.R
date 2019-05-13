rm(list=ls())
cat("\014")

training_data <- read.csv("training_data_og.csv", header = TRUE)
attach(training_data)

nih_1a <- dplyr::select(training_data, ICD_ID, NIHS_1a_in)
chisq.test(table(nih_1a), simulate.p.value = TRUE)

nih_1b <- dplyr::select(training_data, ICD_ID, NIHS_1b_in)
chisq.test(table(nih_1b), simulate.p.value = TRUE)

nih_1c <- dplyr::select(training_data, ICD_ID, NIHS_1c_in)
chisq.test(table(nih_1c), simulate.p.value = TRUE)

nih_2 <- dplyr::select(training_data, ICD_ID, NIHS_2_in)
chisq.test(table(nih_2), simulate.p.value = TRUE)

gender <- dplyr::select(training_data, ICD_ID, GENDER_TX)
chisq.test(table(gender), simulate.p.value = TRUE)