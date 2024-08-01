# Filtered table to show only monthly and C algorithm data 
# aided in the decision to have 4 columns to represent the max number if recalcs
c_full_results <- full_results_all %>% 
                  dplyr::filter(weeklyOrMonthly == "Monthly", results_set == "C algorithm") %>% 
                  dplyr::group_by(Code)

# create and store data containing monthly data for the P algorithm from limits table
# this data is to eventually be used for kmeans clustering 
# Filtered to only show unique cl for each hosp code 
# Filtered to only include the code and cl
c_limits_results <- limits_table_output_Monthly_C_algorithm %>% 
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

# Creation of table to show changes in cl across different NHS Providers 
c_limit_changes <- c_limits_results %>%
                   dplyr::group_by(Code) %>%
                   dplyr::summarise(cl_values = list(new_cl_columns(cl))) %>%
                   tidyr::unnest_wider(cl_values, names_sep = "_") %>%
                   dplyr::mutate(across(starts_with("cl_values_"), as.numeric)) %>%
                   dplyr::rename(`N-3 cl` = cl_values_1,
                      `N-2 cl` = cl_values_2,
                      `N-1 cl` = cl_values_3,
                      `N cl` = cl_values_4)

# Inner join the codes list with the c_limit_changes to filter out the 
# national and regional codes which are included in the c_limit_changes tibble
# as the AE_Data table where codes is derived from only contains provider level codes  
c_limit_changes <- dplyr::inner_join(c_limit_changes, codes, by = c("Code" = "Prov_Code"))
c_limit_changes <- c_limit_changes %>% dplyr::relocate(Prov_Name, .after = Code)

# calculate and store the differences between the central lines in the c_limit_changes tibble
first_diff <- c_limit_changes[, 4] - c_limit_changes[, 3]
second_diff <- c_limit_changes[, 5] - c_limit_changes[, 4]
third_diff <- c_limit_changes[, 6] - c_limit_changes[, 5] 
c_limit_diff <- data.frame(Code = c_limit_changes[, 1],
                         Prov_Name = c_limit_changes[, 2],
                         'N-2 cl Diff' = first_diff,
                         'N-1 cl Diff' = second_diff, 
                         'N cl Diff' = third_diff) 

# Remove rightmost column as only 1 hospital has a non 0 value in it, heading are changed accordingly
c_limit_diff <- c_limit_diff[-c(5)]
colnames(c_limit_diff) <- c("Code", "Prov_name", "N-1_cl_diff", "N_cl_diff")

# ============= Creation of Teaching Hospital Codes Tibble =================

# Create df of all teaching hospitals in England and Scotland 
# sourced from wiki list of teaching hospitals 
teaching_hospitals <- data.frame('N161H', 
                                 'N101H',
                                 'T113H',
                                 'S206H',
                                 'G504H',
                                 'G106H',
                                 'G107H',
                                 'D102H', 
                                 'G306H', 
                                 'T101H', 
                                 'G405H',
                                 'H202H',
                                 'N121H',
                                 'S308H',
                                 'G207H', 
                                 'G516H',
                                 'S116H',
                                 'RGT', 
                                 'RP5',
                                 'RA7',
                                 'RQ6',
                                 'RDZ',
                                 'RTH',
                                 'REN',
                                 'RK9',
                                 'RTD',
                                 'RVJ', 
                                 'RR1', 
                                 'RTD',
                                 'RTR',
                                 'RR8',
                                 'RWE',
                                 'AEJ', 
                                 'RW3', 
                                 'RD8',
                                 'RM1',
                                 'RHQ',
                                 'RX1',
                                 'RD3',
                                 'RXW',
                                 'NN4',
                                 'REF',
                                 'RTG',
                                 'NX0',
                                 'RTX',
                                 'RQ6',
                                 'R1E',
                                 'RM3',
                                 'RXK',
                                 'RHM',
                                 'RVJ',
                                 'RJ2',
                                 'RVW',
                                 'RXA',
                                 'RCB',
                                 'R1K',
                                 'RYJ',
                                 'RQM',
                                 'RJ1',
                                 'RJZ',
                                 'RAL',
                                 'R1H', 
                                 'RJ7',
                                 'RRV',
                                 'RAE',
                                 'RBL',
                                 'RXL',
                                 'RXN',
                                 'A111H',
                                 'A210H'
                                 )

# Change df from short to long 
colnames(teaching_hospitals) <- "teaching_hosp_codes"

# Transpose the df to make each code a row
teaching_hospitals <- t(teaching_hospitals)

# Convert the transposed matrix back to a df
teaching_hospitals <- data.frame(teaching_hosp_codes = teaching_hospitals[,1])

# Read csv file containing Prov Names with associated hospital code  
hospital_codes <- utils::read.csv("Clustering Codes/England and Scotland Hospital Codes.csv")

# create and store teaching hospitals with NA provider names 
na_rows <- teaching_hospitals %>% dplyr::filter(is.na(Prov_Name))

# create and store the hospital code and provider name from the csv file
hospital_mapping <- hospital_codes %>% dplyr::select(ID_Name, Name_of_Hospital)

# left join the teaching hosp codes with the id name 
# populate the missing na values 
# drops the name of hospital column
filled_na_rows <- na_rows %>%
                  dplyr::left_join(hospital_mapping, by = c("teaching_hosp_codes" = "ID_Name")) %>%
                  dplyr::mutate(Prov_Name = ifelse(is.na(Prov_Name), Name_of_Hospital, Prov_Name)) %>%
                  dplyr::select(-Name_of_Hospital)

# filter the provider names that are not NA and combine the dfs
teaching_hospitals <- teaching_hospitals %>%
                      dplyr::filter(!is.na(Prov_Name)) %>%
                      dplyr::bind_rows(filled_na_rows)

# sorts the df by teaching hosp code 
teaching_hospitals <- teaching_hospitals %>% dplyr::arrange(teaching_hosp_codes)

# Filter c_limit_diff for rows with "University" in Prov_Name
university_hospitals <- c_limit_diff %>%
                        dplyr::filter(grepl("University", Prov_Name.x)) %>%
                        dplyr::select(Code, Prov_Name.x)

# Rename columns for consistency
university_hospitals <- university_hospitals %>%
                        dplyr::rename(teaching_hosp_codes = Code, Prov_Name = Prov_Name.x)

# Combine with teaching_hospital_codes
teaching_hospital_codes <- dplyr::bind_rows(teaching_hospital_codes, university_hospitals)

# Add a column to c_limit_diff called teaching_hospital where 1 means it is a 
# teaching or university hospital and 0 means it is not
c_limit_diff <- c_limit_diff %>%
                dplyr::mutate(teaching_hospital = as.integer(Code %in% teaching_hospital_codes$teaching_hosp_codes))

# ============= K-means Clustering =================

# Convert columns (except Code column) from chr to numeric and standardise results
std_c_diff <- c_limit_diff %>% dplyr::mutate(across(where(is.numeric), scale))

# drop "Codes" and "Prov_name" column
c_clustering <- std_c_diff[-c(1, 2)]

# Optimal number of clusters 
factoextra::fviz_nbclust(c_clustering, kmeans, method = "wss") + labs(subtitle = "Elbow Method")
factoextra::fviz_nbclust(c_clustering, kmeans, method = "silhouette") + labs(subtitle = "Silhouette Method")

# k-means clustering
set.seed(1605)
k_data_c <- kmeans(c_clustering, centers = 3, nstart = 25)
str(k_data_c)

# Visualisation 
factoextra::fviz_cluster(k_data_c, data = c_clustering)
c_cluster <- factoextra::fviz_cluster(k_data_c, data = c_clustering)
c_cluster_data <- c_cluster$data 