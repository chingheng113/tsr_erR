rm(list=ls())
cat("\014")
library(cluster)
library(dplyr)
library(ggplot2)
library(Rtsne)


training_data <- read.csv("training_data_processed.csv", header = TRUE)
training_id_data <- dplyr::select(training_data, ICASE_ID, IDCASE_ID)
training_y_data <- dplyr::select(training_data, ICD_ID)
training_X_data <- dplyr::select(training_data, -one_of(colnames(training_id_data)), -one_of(colnames(training_y_data)))
attach(training_X_data)
training_X_data <- training_X_data %>%
  mutate(
    GENDER_0=factor(GENDER_0, labels=c('0','1')),
    GENDER_1=factor(GENDER_1, labels=c('0','1')),
    HT_ID=factor(HT_ID, labels=c('0','1')),
    HC_ID=factor(HC_ID, labels=c('0','1')),
    DM_ID=factor(DM_ID, labels=c('0','1')),
    PISCH_ID=factor(PISCH_ID, labels=c('0','1')),
    HD_ID=factor(HD_ID, labels=c('0','1')),
    PAD_ID=factor(PAD_ID, labels=c('0','1'))
  )

gower_dist <- daisy(training_X_data, metric = "gower")
gower_mat <- as.matrix(gower_dist)

# number of cluster
sil_width <- c(NA)
for(i in 2:8){  
  pam_fit <- pam(gower_dist, diss = TRUE, k = i)  
  sil_width[i] <- pam_fit$silinfo$avg.width  
}
plot(1:8, sil_width,
     xlab = "Number of clusters",
     ylab = "Silhouette Width")
lines(1:8, sil_width)

# Summary of each cluster
k <- 3
pam_fit <- pam(gower_dist, diss = TRUE, k)
pam_results <- training_X_data %>%
  mutate(cluster = pam_fit$clustering) %>%
  group_by(cluster) %>%
  do(the_summary = summary(.))
pam_results$the_summary

# T-sne
tsne_obj <- Rtsne(gower_dist, is_distance = TRUE)
tsne_data <- tsne_obj$Y %>%
  data.frame() %>%
  setNames(c("X", "Y")) %>%
  mutate(cluster = factor(pam_fit$clustering))
ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))