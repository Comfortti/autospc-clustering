library(dplyr)
library(tidyr)

# create and store data containing monthly data for the P algorithm from full results
p_full_results  <- full_results_all %>% 
                        filter(weeklyOrMonthly == "Monthly", results_set == "P algorithm") %>% 
                        group_by(Code)

# create and store data containing monthly data for the P algorithm from limits table
p_limits_results <- limits_table_output_Monthly_P_algorithm %>% 
                      filter(weeklyOrMonthly == "Monthly") %>% 
                      group_by(Code, plotPeriod)

# create and store only the hospital code, plot period, ucl and lcl from the limits table
p_codes <- p_limits_results %>% select(Code, plotPeriod, ucl, lcl)

# create and store a new column with the calculated inter cl range 
p_codes <- p_codes %>% 
            mutate(icl_range = ucl - lcl)

# create and store the mean of the grouped hospital codes and plot periods 
p_means <- p_codes %>% 
            group_by(Code) %>%
            summarise(icl_mean = mean(icl_range)) %>%
            ungroup()

# create a new column in p_code that contains the means calculated in p_means 
p_codes <- p_codes %>%
            left_join(p_means, by = c("Code"))

# remove region and national results as only interested at the trust and provider level 
p_means <- p_means[(which(nchar(p_means$Code) > 2)),]

# add a column to p_means that indicates whether the A&E department is a teaching hospital or not
p_means <- p_means %>%
  mutate(teaching_hospital = as.integer(Code %in% teaching_hospital_codes$teaching_hosp_codes))

# remove NaN and NA values 
p_means <- p_means %>% drop_na(icl_mean)

# Standardise results for kmeans clustering 
std_p_means <- p_means %>% mutate(across(where(is.numeric), scale))

# Remove first column as it is not used in the clustering 
p_final <- std_p_means[-c(1)]

# Optimal number of clusters 
fviz_nbclust(p_final, kmeans, method = "wss") + labs(subtitle = "Elbow Method")
fviz_nbclust(p_final, kmeans, method = "silhouette") + labs(subtitle = "Silhouette Method")

# k-means clustering
set.seed(1605)
k_data_p <- kmeans(p_final, centers = 4, nstart = 25)
str(k_data_p)

# Visualisation 
p_cluster <- fviz_cluster(k_data_p, data = p_final)
p_cluster_data <- p_cluster$data 