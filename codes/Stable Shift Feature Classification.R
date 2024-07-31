library(dplyr)
library(tidyr)

# Filtered table to show only monthly and C algorithm data 
analysis_table <- full_results_all %>% 
  filter(weeklyOrMonthly == "Monthly", results_set == "C algorithm") %>% 
  group_by(Code)

# Filtered original limits table to only show unique cl for each prov code 
unique_limits_table_output_C_algorithm <- limits_table_output_Monthly_C_algorithm %>% 
                                            filter(weeklyOrMonthly == "Monthly") %>% 
                                            distinct(cl, .keep_all = TRUE) %>%
                                            group_by(Code, plotPeriod)


# Filtered table to only include the unique columns Code and unique cl 
trends_limits_table <- unique_limits_table_output_C_algorithm %>%
                        select(Code, cl)

# Function to replicate previous column until 4 columns for the cl exist 
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
perf_table <- trends_limits_table %>%
  group_by(Code) %>%
  summarise(cl_values = list(new_cl_columns(cl))) %>%
  unnest_wider(cl_values, names_sep = "_") %>%
  mutate(across(starts_with("cl_values_"), as.numeric)) %>%
  rename(`N-3 cl` = cl_values_1,
         `N-2 cl` = cl_values_2,
         `N-1 cl` = cl_values_3,
         `N cl` = cl_values_4)

# Filter to only include NHS Providers 
# perf_table[perf_table$Code %in% codes$Prov_Code]
perf_table <- inner_join(perf_table, codes, by = c("Code" = "Prov_Code"))
perf_table <- perf_table %>% relocate(Prov_Name, .after = Code)

# Difference table calculation to show difference between rows in perf_table instead of raw value 
first_diff <- perf_table[, 4] - perf_table[, 3]
second_diff <- perf_table[, 5] - perf_table[, 4]
third_diff <- perf_table[, 6] - perf_table[, 5] 
diff_table <- data.frame(Code = perf_table[, 1],
                         Prov_Name = perf_table[, 2],
                         'N-2 cl Diff' = first_diff,
                         'N-1 cl Diff' = second_diff, 
                         'N cl Diff' = third_diff) 

# Remove rightmost column as only 1 hospital has a non 0 value in it 
diff_table <- diff_table[-c(5)]

# Create df of all teaching hospitals in England and Scotland 
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

# Transpose the data frame to make each code a row
teaching_hospitals <- t(teaching_hospitals)

# Convert the transposed matrix back to a data frame
teaching_hospitals <- data.frame(teaching_hosp_codes = teaching_hospitals[,1])

# Populate missing Prov Names 
hospital_codes <- read.csv("C:/Users/niffi/OneDrive/Documents/AI and Data Science/Semester 3 - Placement/Codes/Clustering Codes/England and Scotland Hospital Codes.csv")

na_rows <- teaching_hospitals %>% filter(is.na(Prov_Name))

hospital_mapping <- hospital_codes %>% select(ID_Name, Name_of_Hospital)

filled_na_rows <- na_rows %>%
  left_join(hospital_mapping, by = c("teaching_hosp_codes" = "ID_Name")) %>%
  mutate(Prov_Name = ifelse(is.na(Prov_Name), Name_of_Hospital, Prov_Name)) %>%
  select(-Name_of_Hospital)

teaching_hospitals <- teaching_hospitals %>%
  filter(!is.na(Prov_Name)) %>%
  bind_rows(filled_na_rows)

teaching_hospitals <- teaching_hospitals %>% arrange(teaching_hosp_codes)

# Filter diff_table for rows with "University" in Prov_Name
university_hospitals <- diff_table %>%
  filter(grepl("University", Prov_Name.x)) %>%
  select(Code, Prov_Name.x)

# Rename columns for consistency
university_hospitals <- university_hospitals %>%
  rename(teaching_hosp_codes = Code, Prov_Name = Prov_Name.x)

# Combine with teaching_hospital_codes
teaching_hospital_codes <- bind_rows(teaching_hospital_codes, university_hospitals)

# Add a column to diff_table called teaching_hospital where 1 means it is a 
# teaching or university hospital and 0 means it is not
diff_table <- diff_table %>%
  mutate(teaching_hospital = as.integer(Code %in% teaching_hospital_codes$teaching_hosp_codes))