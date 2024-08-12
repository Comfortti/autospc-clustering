# combine c_clustering and p_clustering 
# rename columns and select relevant columns 
final_clustering <- dplyr::inner_join(c_clustering, p_clustering, by = c("Code" = "Code"))

colnames(final_clustering) <- c("Code", "Prov_name", "C_first_diff", "C_second_diff", "teaching_hosp", "Prov_Name_2","P_first_diff", "P_second_diff", "Teaching_hospital")
final_clustering <- final_clustering %>% dplyr::select(Code, Prov_name, C_first_diff, C_second_diff, P_first_diff, P_second_diff, Teaching_hospital)

# drop na rows 

final_clustering <- final_clustering %>% drop_na(C_first_diff, C_second_diff, P_first_diff, P_second_diff, Teaching_hospital)

# ============== K Means Clustering ==============

# convert columns (except Code column) from chr to numeric and standardise results
std_final_clust <- final_clustering %>% dplyr::mutate(across(where(is.numeric), scale))
AE_clustering <- std_final_clust[-c(1, 2)]

# optimal number of clusters 
factoextra::fviz_nbclust(AE_clustering, kmeans, method = "wss") + labs(subtitle = "Elbow Method")
factoextra::fviz_nbclust(AE_clustering, kmeans, method = "silhouette") + labs(subtitle = "Silhouette Method")

# k-means clustering
set.seed(1605)
k_data <- kmeans(AE_clustering, centers = 5, nstart = 25)
str(k_data)

# visualisation 
factoextra::fviz_cluster(k_data, data = AE_clustering)
final_cluster <- factoextra::fviz_cluster(k_data, data = AE_clustering)
final_cluster_data <- final_cluster$data 

# ============== Analysis ==============

# convert the name column in final_cluster_data to numeric 
final_cluster_data$name <- as.numeric(final_cluster_data$name)

# make the row number in final_clustering a column 
final_clustering <- final_clustering %>% mutate(row_number = row_number())

# left join both tibbles together 
cluster_analysis <- final_clustering %>% 
                    left_join(final_cluster_data %>%
                                select(name, x, y, coord, cluster), by = c("row_number" = "name"))
cluster_analysis <- cluster_analysis %>% select(-row_number)

# sort by cluster 
cluster_analysis <- cluster_analysis %>% arrange(cluster)