library(dplyr)
library(factoextra)
library(ggplot2)

# Convert columns (except Code column) from chr to numeric
std_perf_table <- diff_table %>% mutate(across(where(is.numeric), scale))

# Remove the "Codes" and "Prov_Name" column
test_table <- std_perf_table[-c(1, 2)]

# Optimal number of clusters 
fviz_nbclust(test_table, kmeans, method = "wss") + labs(subtitle = "Elbow Method")
fviz_nbclust(test_table, kmeans, method = "silhouette") + labs(subtitle = "Silhouette Method")

# k-means clustering
set.seed(1605)
k_data <- kmeans(test_table, centers = 5, nstart = 25)
str(k_data)

# Visualisation 
A_cluster <- fviz_cluster(k_data, data = test_table)
A_cluster_data <- A_cluster$data 