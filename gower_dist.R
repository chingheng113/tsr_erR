rm(list=ls())
cat("\014")
library(cluster)
library(dplyr)
library(ggplot2)
library(Rtsne)


training_data <- read.csv("training_data.csv", header = TRUE)
training_id_data <- dplyr::select(training_data, ICASE_ID, IDCASE_ID)
training_y_data <- dplyr::select(training_data, ICD_ID)
training_X_data <- dplyr::select(training_data, -one_of(colnames(training_id_data)), -one_of(colnames(training_y_data)))

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
k <- 2
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
  mutate(cluster = factor(training_y_data$ICD_ID))
ggplot(aes(x = X, y = Y), data = tsne_data) +
  geom_point(aes(color = cluster))