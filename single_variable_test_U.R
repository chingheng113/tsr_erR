rm(list=ls())
cat("\014")

all_data <- read.csv("tsr_er_og.csv", header = TRUE)
all_data$ICD_ID <- replace(all_data$ICD_ID, all_data$ICD_ID<3, 0)
all_data$ICD_ID <- replace(all_data$ICD_ID, all_data$ICD_ID>2, 1)
attach(all_data)

# boxplot(AGE~ICD_ID)
age.p<-wilcox.test(AGE~ICD_ID, alt='two.sided', paired = FALSE)$p.value
age.p

age2.p <- t.test(AGE~ICD_ID)$p.value
age2.p

height.p<-wilcox.test(HEIGHT_NM~ICD_ID, alt='two.sided', paired = FALSE)$p.value
height.p

height2.p <- t.test(HEIGHT_NM~ICD_ID)$p.value
height2.p

weight.p<-wilcox.test(WEIGHT_NM~ICD_ID, alt='two.sided', paired = FALSE)$p.value
weight.p

weight2.p <- t.test(WEIGHT_NM~ICD_ID)$p.value
weight2.p

sbp.p<-wilcox.test(SBP_NM~ICD_ID, alt='two.sided', paired = FALSE)$p.value
sbp.p

sbp2.p <- t.test(SBP_NM~ICD_ID)$p.value
sbp2.p

dbp.p<-wilcox.test(DBP_NM~ICD_ID, alt='two.sided', paired = FALSE)$p.value
dbp.p

dbp2.p <- t.test(DBP_NM~ICD_ID)$p.value
dbp2.p

plate.p<-wilcox.test(PLATELET_NM~ICD_ID, alt='two.sided', paired = FALSE)$p.value
plate.p

plate2.p <- t.test(PLATELET_NM~ICD_ID)$p.value
plate2.p

ptt.p<-wilcox.test(PTT1_NM~ICD_ID, alt='two.sided', paired = FALSE)$p.value
ptt.p

ptt2.p <- t.test(PTT1_NM~ICD_ID)$p.value
ptt2.p

ptinr.p<-wilcox.test(PTINR_NM~ICD_ID, alt='two.sided', paired = FALSE)$p.value
ptinr.p

ptinr2.p <- t.test(PTINR_NM~ICD_ID)$p.value
ptinr2.p

er.p<-wilcox.test(ER_NM~ICD_ID, alt='two.sided', paired = FALSE)$p.value
er.p

er2.p <- t.test(ER_NM~ICD_ID)$p.value
er2.p

cer.p<-wilcox.test(CRE_NM~ICD_ID, alt='two.sided', paired = FALSE)$p.value
cer.p

cer2.p <- t.test(CRE_NM~ICD_ID)$p.value
cer2.p


hbac.p<-wilcox.test(HBAC_NM~ICD_ID, alt='two.sided', paired = FALSE)$p.value
hbac.p

hbac2.p <- t.test(HBAC_NM~ICD_ID)$p.value
hbac2.p