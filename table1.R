rm(list=ls())
cat("\014")
library(tableone)

all_data <- read.csv("tsr_er_og.csv", header = TRUE)
all_data$ICD_ID <- replace(all_data$ICD_ID, all_data$ICD_ID<3, 0)
all_data$ICD_ID <- replace(all_data$ICD_ID, all_data$ICD_ID>2, 1)
attach(all_data)

all_data <- all_data %>%
  mutate(
    NIHS_1a_in=factor(NIHS_1a_in, labels=c('0','1','2','3'))
         )
vars <- c('HEIGHT_NM',	'WEIGHT_NM', 'NIHS_1a_in')
tabUnmatched <- CreateTableOne(vars = vars, strata = "ICD_ID", data = all_data, test = FALSE)
print(tabUnmatched, smd = TRUE)
