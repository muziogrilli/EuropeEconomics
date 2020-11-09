library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(dplyr)

# Define Working directory
setwd("C:/Users/MG53060/Documents/GitHub/EuropeEconomics/")
# Read processed dataset
data <- readRDS(file = "./CleanData.rds")

###################
# Data preparation

# Remove companies featuring zero employee
data_c <-subset(data, employment>=1.0)

# Define subset of variables relating to stocks
stocks_var    <- c("n_ep_pats_stock", 
                   "n_nat_pats_stock",
                   "n_eu_tm_stock", 
                   "n_nat_tm_stock",
                   "n_eu_des_stock",
                   "n_nat_des_stock")

# Define stocks per employee
data_c$n_ep_pats_stock_empl  <- data_c$n_ep_pats_stock / data_c$employment
data_c$n_nat_pats_stock_empl <- data_c$n_nat_pats_stock / data_c$employment
data_c$n_eu_tm_stock_empl    <- data_c$n_eu_tm_stock / data_c$employment
data_c$n_nat_tm_stock_empl   <- data_c$n_nat_tm_stock / data_c$employment
data_c$n_eu_des_stock_empl   <- data_c$n_eu_des_stock / data_c$employment
data_c$n_nat_des_stock_empl  <- data_c$n_nat_des_stock / data_c$employment

# Define subset of variables relating to stocks per employee
stocks_empl_var <- c("n_ep_pats_stock_empl",
                   "n_nat_pats_stock_empl",
                   "n_eu_tm_stock_empl", 
                   "n_nat_tm_stock_empl",
                   "n_eu_des_stock_empl",
                   "n_nat_des_stock_empl")

# Define dataframe to be used to perform clustering analysis
cluster_data <- data_c %>% filter(ip_owner == 1) 

# Remove single outlier for stocks (10451)
cluster_data <- cluster_data[-c(2898,10451), ]

# Perform scaling
cluster_data_scaled <- scale(cluster_data[,stocks_empl_var])

# Remove single outlier for stocks (10451)
#cluster_data_scaled <- cluster_data_scaled[-c(2898,10451), ]

# Remove single outlier for per employee stocks (2898)
#cluster_data_scaled <- cluster_data_scaled[-c(2898), ]


# Elbow method for the choice of the optimal number of clusters

# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(cluster_data_scaled, k, nstart = 25 )$tot.withinss
}

# Compute and plot wss for k = 2 to k = 15
k.values <- 2:15

# extract wss for 2-15 clusters
set.seed(123) # initialize random numbers
wss_values <- map_dbl(k.values, wss)

p1 <- plot(k.values, wss_values,
           type="b", pch = 19, frame = FALSE, 
           xlab="Number of clusters K",
           ylab="Total within-clusters sum of squares")
print(p1)

# Silhouette
# function to compute average silhouette for k clusters
avg_sil <- function(k) {
  km.res <- kmeans(cluster_data_scaled, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(cluster_data_scaled))
  mean(ss[, 3])
}

# Compute and plot wss for k = 2 to k = 15
k.values <- 2:15

# extract avg silhouette for 2-15 clusters
set.seed(123) # initialize random numbers 
avg_sil_values <- map_dbl(k.values, avg_sil)

p2 <- plot(k.values, avg_sil_values,
           type = "b", pch = 19, frame = FALSE, 
           xlab = "Number of clusters K",
           ylab = "Average Silhouettes")

print(p2)
  
# Gap statistic method 
set.seed(123)
gap_stat <- clusGap(cluster_data_scaled, FUN = kmeans, nstart = 25, K.max = 10, B = 50)


# Perform again the k-means clustering for the optimal value

# Compute k-means clustering with k = 5
set.seed(123)
final_clustering <- kmeans(cluster_data_scaled, 5, nstart = 25)

cluster_plot <- fviz_cluster(final_clustering, data = cluster_data_scaled)
print(cluster_plot)

# Print cluster features
cluster_data[,stocks_empl_var] %>%
  mutate(Cluster = final_clustering$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")
