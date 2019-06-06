rm(list=ls())
cat("\014")
library(tableone)

all_data <- read.csv("tsr_er_og.csv", header = TRUE)
all_data$ICD_ID <- replace(all_data$ICD_ID, all_data$ICD_ID<3, 0)
all_data$ICD_ID <- replace(all_data$ICD_ID, all_data$ICD_ID>2, 1)
attach(all_data)
													
all_data <- all_data %>%
  mutate(
    NIHS_1a_in=factor(NIHS_1a_in, labels=c('0','1','2','3')),
    NIHS_1b_in=factor(NIHS_1b_in, labels=c('0','1','2')),
    NIHS_1c_in=factor(NIHS_1c_in, labels=c('0','1','2')),
    NIHS_2_in=factor(NIHS_2_in, labels=c('0','1','2')),
    NIHS_3_in=factor(NIHS_3_in, labels=c('0','1','2','3')),
    NIHS_4_in=factor(NIHS_4_in, labels=c('0','1','2','3')),
    NIHS_5aL_in=factor(NIHS_5aL_in, labels=c('0','1','2','3','4')),
    NIHS_5bR_in=factor(NIHS_5bR_in, labels=c('0','1','2','3','4')),
    NIHS_6aL_in=factor(NIHS_6aL_in, labels=c('0','1','2','3','4')),
    NIHS_6bR_in=factor(NIHS_6bR_in, labels=c('0','1','2','3','4')),
    NIHS_7_in=factor(NIHS_7_in, labels=c('0','1','2')),
    NIHS_8_in=factor(NIHS_8_in, labels=c('0','1','2')),
    NIHS_9_in=factor(NIHS_9_in, labels=c('0','1','2','3')),
    NIHS_10_in=factor(NIHS_10_in, labels=c('0','1','2')),
    NIHS_11_in=factor(NIHS_11_in, labels=c('0','1','2')),
    HT_ID=factor(HT_ID, labels=c('0','1')),
    HC_ID=factor(HC_ID, labels=c('0','1')),
    DM_ID=factor(DM_ID, labels=c('0','1')),
    PISCH_ID=factor(PISCH_ID, labels=c('0','1')),
    HD_ID=factor(HD_ID, labels=c('0','1')),
    PAD_ID=factor(PAD_ID, labels=c('0','1'))
         )
					
vars <- c('GENDER_TX', 'AGE','HEIGHT_NM',	'WEIGHT_NM', 'NIHS_1a_in','NIHS_1b_in',	'NIHS_1c_in',	'NIHS_2_in',
          'NIHS_3_in', 'NIHS_4_in',	'NIHS_5aL_in',	'NIHS_5bR_in',	'NIHS_6aL_in',	'NIHS_6bR_in',
          'NIHS_7_in', 'NIHS_8_in',	'NIHS_9_in',	'NIHS_10_in',	'NIHS_11_in',
          'SBP_NM',	'DBP_NM',	'PLATELET_NM',	'PTT1_NM',	'PTINR_NM',	'ER_NM',	'CRE_NM',	'HBAC_NM',
          'HT_ID',	'HC_ID',	'DM_ID',	'PISCH_ID',	'HD_ID',	'PAD_ID')
tabUnmatched <- CreateTableOne(vars = vars, strata = "ICD_ID", data = all_data, test = FALSE)
print(tabUnmatched, smd = TRUE)