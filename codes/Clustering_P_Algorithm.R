# Filtered table to show only monthly and P algorithm data
# aided in the decision to have 4 columns to represent the max number if recalcs
p_full_results  <- full_results_all %>% 
                    dplyr::filter(weeklyOrMonthly == "Monthly", results_set == "P algorithm") %>% 
                    dplyr::group_by(Code)

# create and store data containing monthly data for the P algorithm from limits table
# this data is to eventually be used for kmeans clustering 
# Filtered to only show unique cl for each hosp code 
# Filtered to only include the code and cl
p_limits_results <- limits_table_output_Monthly_P_algorithm %>% 
                    dplyr::filter(weeklyOrMonthly == "Monthly") %>% 
                    dplyr::distinct(cl, .keep_all = TRUE) %>%
                    dplyr::group_by(Code, plotPeriod) %>%
                    dplyr::select(Code, plotPeriod, cl)

# Function to replicate most recent column until 4 columns for the cl exist 
new_cl_columns <- function(cl_values) {
  n <- length(cl_values)
  cl_values <- as.character(cl_values)
  if (n == 1) {
    return(c(cl_values[1], cl_values[1], cl_values[1], cl_values[1]))
  } else if (n == 2) {
    return(c(cl_values[1], cl_values[2], cl_values[2], cl_values[2]))
  } else if (n == 3) {
    return(c(cl_values[1], cl_values[2], cl_values[3], cl_values[3]))
  } else if (n >= 4) {
    return(c(cl_values[n-3], cl_values[n-2], cl_values[n-1], cl_values[n]))
  }
}

# Creation of table to show changes in cl across different AE departments  
p_limit_changes <- p_limits_results %>%
                    dplyr::group_by(Code) %>%
                    dplyr::summarise(cl_values = list(new_cl_columns(cl))) %>%
                    tidyr::unnest_wider(cl_values, names_sep = "_") %>%
                    dplyr::mutate(across(starts_with("cl_values_"), as.numeric)) %>%
                    dplyr::rename(`N-3 cl` = cl_values_1,
                      `N-2 cl` = cl_values_2,
                      `N-1 cl` = cl_values_3,
                      `N cl` = cl_values_4)

# Inner join the codes list with the p_limit_changes to filter out the 
# national and regional codes which are included in the p_limit_changes tibble
# as the AE_Data table where codes is derived from only contains provider level codes 
p_limit_changes <- dplyr::inner_join(p_limit_changes, codes, by = c("Code" = "Prov_Code"))
p_limit_changes <- p_limit_changes %>% dplyr::relocate(Prov_Name, .after = Code)


# calculate and store the differences between the central lines in the p_limit_changes tibble
first_diff <- p_limit_changes[, 4] - p_limit_changes[, 3]
second_diff <- p_limit_changes[, 5] - p_limit_changes[, 4]
third_diff <- p_limit_changes[, 6] - p_limit_changes[, 5]
p_limit_diff <- data.frame(Code = p_limit_changes[, 1],
                           Prov_Name = p_limit_changes[, 2],
                           "N-2 cl Diff" = first_diff, 
                           "N-1 cl Diff" = second_diff, 
                           "N cl Diff" = third_diff)

# all values in N cl column are 0 therefore column is removed and heading changed accordingly
# na rows are dropped
p_limit_diff <- p_limit_diff[-c(5)]
colnames(p_limit_diff) <- c("Code", "Prov_name", "N-1_cl_diff", "N_cl_diff")
p_limit_diff <- p_limit_diff %>% tidyr::drop_na(`N-1_cl_diff`)

# Add a column to p_limit_diff called teaching_hospital where 1 means it is a 
# teaching or university hospital and 0 means it is not
p_limit_diff <- p_limit_diff %>%
  dplyr::mutate(Teaching_hospital = as.integer(Code %in% teaching_hospital_codes$teaching_hosp_codes)) 

# ============= K-means Clustering =================

# Convert columns (except Code column) from chr to numeric and standardise results
std_p_diff <- p_limit_diff %>% dplyr::mutate(across(where(is.numeric), scale))
p_clustering <- std_p_diff[-c(1, 2)]

# Optimal number of clusters 
factoextra::fviz_nbclust(p_clustering, kmeans, method = "wss") + labs(subtitle = "Elbow Method")
factoextra::fviz_nbclust(p_clustering, kmeans, method = "silhouette") + labs(subtitle = "Silhouette Method")

# k-means clustering
set.seed(1605)
k_data_p <- kmeans(p_clustering, centers = 6, nstart = 25)
str(k_data_p)

# Visualisation 
factoextra::fviz_cluster(k_data_p, data = p_clustering)
p_cluster <- factoextra::fviz_cluster(k_data_p, data = p_clustering)
p_cluster_data <- p_cluster$data 