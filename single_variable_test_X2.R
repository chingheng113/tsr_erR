rm(list=ls())
cat("\014")

all_data <- read.csv("tsr_er_og.csv", header = TRUE)
all_data$ICD_ID <- replace(all_data$ICD_ID, all_data$ICD_ID<3, 0)
all_data$ICD_ID <- replace(all_data$ICD_ID, all_data$ICD_ID>2, 1)
attach(all_data)

# nih_1a <- dplyr::select(all_data, ICD_ID, NIHS_1a_in)
# nih_1a.table <- as.matrix(table(nih_1a))
# nih_1a.table
# nih_1a.p <- chisq.test(nih_1a.table, simulate.p.value = TRUE)$p.value
# nih_1a.p
# 
# nih_1b <- dplyr::select(all_data, ICD_ID, NIHS_1b_in)
# nih_1b.table <- as.matrix(table(nih_1b))
# nih_1b.table
# nih_1b.p <- chisq.test(nih_1b.table, simulate.p.value = TRUE)$p.value
# nih_1b.p
# 
# nih_1c <- dplyr::select(all_data, ICD_ID, NIHS_1c_in)
# nih_1c.table <- as.matrix(table(nih_1c))
# nih_1c.table
# nih_1c.p <- chisq.test(nih_1c.table, simulate.p.value = TRUE)$p.value
# nih_1c.p
# 
# nih_2 <- dplyr::select(all_data, ICD_ID, NIHS_2_in)
# nih_2.table <- as.matrix(table(nih_2))
# nih_2.table
# nih_2.p <- chisq.test(nih_2.table, simulate.p.value = TRUE)$p.value
# nih_2.p
# 
# nih_3 <- dplyr::select(all_data, ICD_ID, NIHS_3_in)
# nih_3.table <- as.matrix(table(nih_3))
# nih_3.table
# nih_3.p <- chisq.test(nih_3.table, simulate.p.value = TRUE)$p.value
# nih_3.p
# 
# nih_4 <- dplyr::select(all_data, ICD_ID, NIHS_4_in)
# nih_4.table <- as.matrix(table(nih_4))
# nih_4.table
# nih_4.p <- chisq.test(nih_4.table, simulate.p.value = TRUE)$p.value
# nih_4.p
# 
# nih_5a <- dplyr::select(all_data, ICD_ID, NIHS_5aL_in)
# nih_5a.table <- as.matrix(table(nih_5a))
# nih_5a.table
# nih_5a.p <- chisq.test(nih_5a.table, simulate.p.value = TRUE)$p.value
# nih_5a.p
# 
# nih_5b <- dplyr::select(all_data, ICD_ID, NIHS_5bR_in)
# nih_5b.table <- as.matrix(table(nih_5b))
# nih_5b.table
# nih_5b.p <- chisq.test(nih_5b.table, simulate.p.value = TRUE)$p.value
# nih_5b.p
# 
# nih_6a <- dplyr::select(all_data, ICD_ID, NIHS_6aL_in)
# nih_6a.table <- as.matrix(table(nih_6a))
# nih_6a.table
# nih_6a.p <- chisq.test(nih_6a.table, simulate.p.value = TRUE)$p.value
# nih_6a.p
# 
# nih_6b <- dplyr::select(all_data, ICD_ID, NIHS_6bR_in)
# nih_6b.table <- as.matrix(table(nih_2))
# nih_6b.table
# nih_6b.p <- chisq.test(nih_6b.table, simulate.p.value = TRUE)$p.value
# nih_6b.p
# 
# nih_7 <- dplyr::select(all_data, ICD_ID, NIHS_7_in)
# nih_7.table <- as.matrix(table(nih_7))
# nih_7.table
# nih_7.p <- chisq.test(nih_7.table, simulate.p.value = TRUE)$p.value
# nih_7.p
# 
# nih_8 <- dplyr::select(all_data, ICD_ID, NIHS_8_in)
# nih_8.table <- as.matrix(table(nih_8))
# nih_8.table
# nih_8.p <- chisq.test(nih_8.table, simulate.p.value = TRUE)$p.value
# nih_8.p
# 
# nih_8 <- dplyr::select(all_data, ICD_ID, NIHS_8_in)
# nih_8.table <- as.matrix(table(nih_8))
# nih_8.table
# nih_8.p <- chisq.test(nih_8.table, simulate.p.value = TRUE)$p.value
# nih_8.p
# 
# nih_9 <- dplyr::select(all_data, ICD_ID, NIHS_9_in)
# nih_9.table <- as.matrix(table(nih_9))
# nih_9.table
# nih_9.p <- chisq.test(nih_9.table, simulate.p.value = TRUE)$p.value
# nih_9.p
# 
# nih_10 <- dplyr::select(all_data, ICD_ID, NIHS_10_in)
# nih_10.table <- as.matrix(table(nih_10))
# nih_10.table
# nih_10.p <- chisq.test(nih_10.table, simulate.p.value = TRUE)$p.value
# nih_10.p
# 
# nih_11 <- dplyr::select(all_data, ICD_ID, NIHS_11_in)
# nih_11.table <- as.matrix(table(nih_11))
# nih_11.table
# nih_11.p <- chisq.test(nih_11.table, simulate.p.value = TRUE)$p.value
# nih_11.p

gender <- dplyr::select(all_data, ICD_ID, GENDER_TX)
gender.p <- chisq.test(table(gender), simulate.p.value = TRUE)$p.value
gender.p

HT <- dplyr::select(all_data, ICD_ID, HT_ID)
HT.p <- chisq.test(table(HT), simulate.p.value = TRUE)$p.value
HT.p

HC <- dplyr::select(all_data, ICD_ID, HC_ID)
HC.p <- chisq.test(table(HC), simulate.p.value = TRUE)$p.value
HC.p

DM <- dplyr::select(all_data, ICD_ID, DM_ID)
DM.p <- chisq.test(table(DM), simulate.p.value = TRUE)$p.value
DM.p

PISCH <- dplyr::select(all_data, ICD_ID, PISCH_ID)
PISCH.p <- chisq.test(table(PISCH), simulate.p.value = TRUE)$p.value
PISCH.p

HD <- dplyr::select(all_data, ICD_ID, HD_ID)
HD.p <- chisq.test(table(HD), simulate.p.value = TRUE)$p.value
HD.p

PAD <- dplyr::select(all_data, ICD_ID, PAD_ID)
PAD.p <- chisq.test(table(PAD), simulate.p.value = TRUE)$p.value
PAD.p
