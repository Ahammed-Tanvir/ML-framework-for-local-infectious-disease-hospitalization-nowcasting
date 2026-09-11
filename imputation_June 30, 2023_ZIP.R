library(dplyr)
library(readxl)
library(openxlsx)
library(ggplot2)
library(sf)
library(missForest)

# Load the demographic dataset--------
#summary_data <- read_excel("/Users/tanvirahammed/Library/CloudStorage/Box-Box/BoxSecure-DPHS-Opiod/COVID registry/Tanvir/Results/SC_demographic_summary_data.xlsx")


# Load the test dataset--------
#Final_test_data_imp <- read_excel("/Users/tanvirahammed/Library/CloudStorage/Box-Box/BoxSecure-DPHS-Opiod/COVID registry/Tanvir/Results/Final_test_data_imp_ZIP_June.xlsx")

# Select only the specified columns
Final_test_data_imp <- Final_test_data %>%
  dplyr::select(ZIP, SEX, age_group, race_group, hospitalisation_RFA, 
                log_subpopulation_size, predicted_hospitalisation2)

Final_test_data_imp$ZIP <- as.numeric(as.character(Final_test_data_imp$ZIP))


## Merge the datasets based on the ZIP column 
Final_test_data_imp<- Final_test_data_imp  %>%
  left_join(selected_dataset, by = "ZIP")




# Load the removed_ZIP_data dataset--------
#removed_ZIP_data <- read_excel("/Users/tanvirahammed/Library/CloudStorage/Box-Box/BoxSecure-DPHS-Opiod/COVID registry/Tanvir/Code/imputation/removed_ZIP_data.xlsx")

# Finding the ZIPs for imputation-------------------
unique_ZIP_data_before <- unique(RFA_test_grouped_data$ZIP)
unique_ZIP_data_after <- unique(Final_test_data_imp$ZIP)

# Find the set difference 
removed_ZIP <- setdiff(unique_ZIP_data_before, unique_ZIP_data_after)
# Convert removed_ZIP to a data frame
removed_ZIP <- data.frame(ZIP = removed_ZIP)

# Filter filtered_data_rfa to keep only rows where ZIP is in removed_ZIP_values
removed_ZIP_data <- RFA_test_data %>% filter(ZIP %in% removed_ZIP$ZIP)




#Cleanning
# removed_ZIP_data$SEX <- ifelse(is.na(removed_ZIP_data$SEX), NA, 
#                                ifelse(removed_ZIP_data$SEX == "M", "Male", "Female"))
# removed_ZIP_data  <- removed_ZIP_data %>%
#   mutate(RACE = case_when(
#     RACE == 1 ~ "White",
#     RACE == 2 ~ "African-American",
#     RACE == 3 ~ "Asian",
#     RACE == 4 ~ "American Indian",
#     RACE == 5 ~ "Other",
#     RACE == 6 ~ "Hispanic",
#     RACE == 7 ~ NA_character_,
#     TRUE ~ as.character(RACE)  # Keep other values as is
#   ))
# 

# Create age_group variable
removed_ZIP_data <- removed_ZIP_data %>%
   mutate(age_group = case_when(
      AGRP %in% c("AGE 00-04", "AGE 05-11", "AGE 12-15", "AGE 16-17") ~ "<18",
      AGRP %in% c("AGE 18-24", "AGE 25-29", "AGE 30-34", "AGE 35-39", "AGE 40-44") ~ "18-44",
      AGRP %in% c("AGE 45-49", "AGE 50-54", "AGE 55-59", "AGE 60-64") ~ "45-64",
      AGRP %in% c("AGE 65-69", "AGE 70-74", "AGE 75-79", "AGE 80-84", "AGE 85+") ~ "65+",
      TRUE ~ NA_character_  # For any unexpected values
   ))


# 
# # Create race_group variable
# removed_ZIP_data <- removed_ZIP_data %>%
#   filter(!is.na(RACE)) %>%
#   mutate(race_group = case_when(
#     RACE == "White" ~ "White",
#     RACE != "White" ~ "Non-white"
#   ))
# 
# # Convert 'Year_Month' to Date format
# removed_ZIP_data$Date <- as.Date(paste0(removed_ZIP_data$Year_Month, "-01"), format = "%Y-%m-%d")
# 
# # Filter data for the test set (July 1 and Dec 31, 2021)
# removed_ZIP_data <- removed_ZIP_data %>%
#   filter(Date >= as.Date("2023-07-01") & Date <= as.Date("2023-12-31"))


removed_ZIP_data <- removed_ZIP_data %>%
  group_by(ZIP, SEX, age_group,  race_group) %>%
  summarize(
    hospitalisation_RFA = n()
  )

# Define all possible combinations
all_combinationsR <- expand.grid(
   ZIP = unique(removed_ZIP_data$ZIP),
   SEX = unique(removed_ZIP_data$SEX),
   age_group = unique(removed_ZIP_data$age_group),
   race_group = unique(removed_ZIP_data$race_group)
)


# Left join with test_data to fill missing combinations
removed_ZIP_data <- left_join(all_combinationsR, removed_ZIP_data, by = c("ZIP", "SEX", "age_group", "race_group"))


## Merge the datasets based on the ZIP column 
removed_ZIP_data<- removed_ZIP_data  %>%
   left_join(selected_dataset, by = "ZIP")


# Subpopulation size 
removed_ZIP_data <- removed_ZIP_data %>%
   mutate(
      new_subpopulation_size = case_when(
         SEX == "Male" & age_group == "<18" & race_group == "White" ~ Population * (Percent_male ) * (`Percent age 0-17` ) * (`Percent_white` ),
         SEX == "Male" & age_group == "18-44" & race_group == "White" ~ Population * (Percent_male ) * (`Percent age 18-44` ) * (`Percent_white` ),
         SEX == "Male" & age_group == "45-64" & race_group == "White" ~ Population * (Percent_male ) * (`Percent age 45-64` ) * (`Percent_white` ),
         SEX == "Male" & age_group == "65+" & race_group == "White" ~ Population * (Percent_male ) * (`Percent age 65 years and over` ) * (`Percent_white` ),
         SEX == "Female" & age_group == "<18" & race_group == "White" ~ Population * (`Percent female` ) * (`Percent age 0-17` ) * (`Percent_white` ),
         SEX == "Female" & age_group == "18-44" & race_group == "White" ~ Population * (`Percent female` ) * (`Percent age 18-44` ) * (`Percent_white` ),
         SEX == "Female" & age_group == "45-64" & race_group == "White" ~ Population * (`Percent female` ) * (`Percent age 45-64` ) * (`Percent_white` ),
         SEX == "Female" & age_group == "65+" & race_group == "White" ~ Population * (`Percent female` ) * (`Percent age 65 years and over` ) * (`Percent_white` ),
         SEX == "Male" & age_group == "<18" & race_group == "Non-white" ~ Population * (Percent_male ) * (`Percent age 0-17` ) * (`Percent Non-white` ),
         SEX == "Male" & age_group == "18-44" & race_group == "Non-white" ~ Population * (Percent_male ) * (`Percent age 18-44` ) * (`Percent Non-white` ),
         SEX == "Male" & age_group == "45-64" & race_group == "Non-white" ~ Population * (Percent_male ) * (`Percent age 45-64` ) * (`Percent Non-white` ),
         SEX == "Male" & age_group == "65+" & race_group == "Non-white" ~ Population * (Percent_male ) * (`Percent age 65 years and over` ) * (`Percent Non-white` ),
         SEX == "Female" & age_group == "<18" & race_group == "Non-white" ~ Population * (`Percent female` ) * (`Percent age 0-17` ) * (`Percent Non-white` ),
         SEX == "Female" & age_group == "18-44" & race_group == "Non-white" ~ Population * (`Percent female` ) * (`Percent age 18-44` ) * (`Percent Non-white` ),
         SEX == "Female" & age_group == "45-64" & race_group == "Non-white" ~ Population * (`Percent female` ) * (`Percent age 45-64` ) * (`Percent Non-white` ),
         SEX == "Female" & age_group == "65+" & race_group == "Non-white" ~ Population * (`Percent female` ) * (`Percent age 65 years and over` ) * (`Percent Non-white` ),
         TRUE ~ NA_real_  # Assign NA for other cases
      )
   )

removed_ZIP_data$new_subpopulation_size<-round(removed_ZIP_data$new_subpopulation_size, 0)

# Replace NA values in occurrences with 0
removed_ZIP_data <- removed_ZIP_data %>%
   mutate(hospitalisation_RFA = replace_na(hospitalisation_RFA, 0))

removed_ZIP_data$log_subpopulation_size <- log(removed_ZIP_data$new_subpopulation_size)
removed_ZIP_data$new_subpopulation_size <- NULL









# Merge Final_test_data_imp and removed_ZIP_data
# full join to combine both datasets
# Renaming columns (if necessary) and joining the datasets
Final_test_data_imp <- Final_test_data_imp %>%
   rename(
      `Percent female` = `Percent female`,
      `Percent Non-white` = `Percent Non-white`,
      `Percent age 0-17` = `Percent age 0-17`,
      `Percent age 18-44` = `Percent age 18-44`,
      `Percent age 45-64` = `Percent age 45-64`,
      `Percent age 65 years and over` = `Percent age 65 years and over`
   )

# Perform the full join using correct column names (with spaces)
imputation_data <- full_join(Final_test_data_imp, removed_ZIP_data, by = c(
   "ZIP", "SEX", "age_group", "race_group", "hospitalisation_RFA", "Population",
   "Percent female", "Percent Non-white", "Percent age 0-17", "Percent age 18-44",
   "Percent age 45-64", "Percent age 65 years and over", "Percent_male", "Percent_white",
   "log_subpopulation_size"
))


imputation_data <- imputation_data %>%
  dplyr::select(-Population, -Percent_male, -Percent_white)

imputation_data$SEX <- as.factor(imputation_data$SEX)
imputation_data$age_group <- as.factor(imputation_data$age_group)
imputation_data$race_group <- as.factor(imputation_data$race_group)



library(ggplot2)
library(dplyr)
library(cowplot)
library(missForest) 
library(mice)

# Check for missing values in the dataset
summary(imputation_data)
# Identify columns with missing values other than 'total_predicted_hospitalizations7'
cols_with_na <- colnames(imputation_data)[colSums(is.na(imputation_data)) > 0]
cols_with_na <- cols_with_na[cols_with_na != "predicted_hospitalisation2"]

# Print columns with NAs other than the target column
print(cols_with_na)


# Step 3: Remove rows with NAs in these columns
imputation_data_clean <- imputation_data[complete.cases(imputation_data[, cols_with_na]), ]

# Confirm that the cleaned dataset has no missing values in other columns
summary(imputation_data_clean)
imputation_data_clean<- as.data.frame(imputation_data_clean)
# Remove rows with Inf or -Inf in log_subpopulation_size
imputation_data_clean <- imputation_data_clean[is.finite(imputation_data_clean$log_subpopulation_size), ]
imputation_data_clean1 <- imputation_data_clean
imputation_data_clean$hospitalisation_RFA <- NULL



# # Identify ZIP codes where log_subpopulation_size is Inf
# zip_codes_with_inf <- unique(imputation_data_clean$ZIP[is.infinite(imputation_data_clean$log_subpopulation_size)])
# # Remove rows with the problematic ZIP codes
# imputation_data_clean <- imputation_data_clean[!imputation_data_clean$ZIP %in% zip_codes_with_inf, ]
# 



# Run missForest on the cleaned dataset including the column with missing values
set.seed(1234)
missForest_result <- missForest(imputation_data_clean)

# imputation_data_clean_numeric <- imputation_data_clean %>%
#   dplyr::select(ZIP, hospitalisation_RFA, predicted_hospitalisation2, mean_Percent_female, mean_Percent_Non_white,  mean_Percent_age_0_19,  mean_Percent_age_20_44, 
#          mean_Percent_age_45_64, mean_Percent_age_65_over)

# Automatically clean column names to be valid R names
colnames(imputation_data_clean) <- make.names(colnames(imputation_data_clean))

set.seed(1234)
mice_imputed <- data.frame(
  ZIP = imputation_data_clean$ZIP,
  SEX = imputation_data_clean$SEX,
  age_group = imputation_data_clean$age_group,
  race_group = imputation_data_clean$race_group,
  #original = imputation_data_clean$hospitalisation_RFA,
  total_predicted_hospitalizations_before_imputation = imputation_data_clean$predicted_hospitalisation2,
  total_predicted_hospitalizations1 = missForest_result$ximp$predicted_hospitalisation2,
  imputed_pmm = complete(mice(imputation_data_clean, method = "pmm"))$predicted_hospitalisation2,
  imputed_cart = complete(mice(imputation_data_clean, method = "cart"))$predicted_hospitalisation2,
  imputed_lasso = complete(mice(imputation_data_clean, method = "lasso.norm"))$predicted_hospitalisation2
)

mice_imputed <- mice_imputed %>%
   left_join(imputation_data_clean1 %>%
                dplyr::select(ZIP, SEX, age_group, race_group, hospitalisation_RFA), by = c("ZIP", "SEX", "age_group", "race_group"))




total_hospitalizations <- mice_imputed %>%
  group_by(ZIP) %>%
  summarise(
    total_observed_hospitalizations = sum(hospitalisation_RFA, na.rm = TRUE),
    total_predicted_hospitalizations_before_imputation = sum(total_predicted_hospitalizations_before_imputation, na.rm = TRUE),
    total_predicted_hospitalizations1 = sum(total_predicted_hospitalizations1, na.rm = TRUE),
    total_predicted_hospitalizations2 = sum(imputed_pmm, na.rm = TRUE),
    total_predicted_hospitalizations3 = sum(imputed_cart, na.rm = TRUE),
    total_predicted_hospitalizations4 = sum(imputed_lasso, na.rm = TRUE)
  )


# Filter rows where total_observed_hospitalizations is greater than 50
total_hospitalizations <- total_hospitalizations[total_hospitalizations$total_observed_hospitalizations > 50, ]

#write.xlsx(total_hospitalizations, "/Users/tanvirahammed/Library/CloudStorage/Box-Box/BoxPHI-PHMR Projects/Tanvir/Data/Estimating state-wide COVID-19 hospitalizations using local health system data/Final Data/total_hospitalizations_ZIP_June_imputed.xlsx")


# Ai
predicted_variables <- c("total_predicted_hospitalizations1", "total_predicted_hospitalizations2", 
                         "total_predicted_hospitalizations3", "total_predicted_hospitalizations4"
)

for (i in seq_along(predicted_variables)) {
  col_name <- paste0("A_i_", i)
  total_hospitalizations[[col_name]] <- with(total_hospitalizations, pmin(total_observed_hospitalizations, get(predicted_variables[i])) /
                                               pmax(total_observed_hospitalizations, get(predicted_variables[i])))
}



A_i_columns <- paste0("A_i_", 1:4)
# Subset the data frame to include only the A_i columns
A_i_data <- total_hospitalizations[A_i_columns]
# Use the summary function to get the five-number summary for each A_i column
summary_A_i <- apply(A_i_data, 2, summary)
# Print the result
print(summary_A_i)
n_distinct(total_hospitalizations$ZIP)


# 
# 
# #MAPE-------------
# # Loop through each predicted variable to compute MAPE
# for (i in seq_along(predicted_variables)) {
#    col_name <- paste0("MAPE_", i)
#    total_hospitalizations[[col_name]] <- with(total_hospitalizations, 
#                                               abs(get(predicted_variables[i]) - total_observed_hospitalizations) / total_observed_hospitalizations)
# }
# 
# # Extract only the MAPE columns
# MAPE_columns <- paste0("MAPE_", 1:4)
# MAPE_data <- total_hospitalizations[MAPE_columns]
# 
# # Compute the mean MAPE for each prediction column
# (mean_MAPE <- apply(MAPE_data, 2, mean, na.rm = TRUE))
# 
# 
## ZIPs in census_data but not in total_hospitalizations
missing_in_hospitalizations <- setdiff(unique(census_data$ZIP), unique(total_hospitalizations$ZIP))

# Get the population for these ZIPs
missing_zip_population <- census_data %>%
   filter(ZIP %in% missing_in_hospitalizations) %>%
   dplyr::select(ZIP, Population)

# Calculate total population of census_data
total_population_census <- sum(census_data$Population, na.rm = TRUE)

# Calculate total population of missing ZIPs
total_population_missing_zips <- sum(missing_zip_population$Population, na.rm = TRUE)
(total_population_missing_zips/total_population_census)*100





## Map of hospitalization observed/estimates/accuracy in all 352 ZIP codes ------------------------
#download the shapefile for zip codes of a state using tigris package. 
sc_zcta_sf = tigris::zctas(state = "SC", class = "sf", year = '2010')

#This this shapefile has a column for ZCTAs. 
# Create a data frame with the required variables
hosp_ZIP_esti_impu <- data.frame(ZIP = as.character(total_hospitalizations$ZIP),
                                 Observed = total_hospitalizations$total_observed_hospitalizations,
                                 Estimated = total_hospitalizations$total_predicted_hospitalizations3,
                                 Accuracy = total_hospitalizations$A_i_3)


#Transform zip codes to ZCTAs first and merge your data with sc_zcta_sf by the ZCTA. 
sc_zcta_sf = left_join(sc_zcta_sf, hosp_ZIP_esti_impu 
                       %>% dplyr::select(Observed, Estimated, Accuracy, ZCTA5CE10 = ZIP), by = 'ZCTA5CE10')


###observed------------
sc_zcta_sf = mutate(sc_zcta_sf, 
                    Observed_grouped = case_when(Observed < 500 ~ '1',
                                                 Observed >= 500 & Observed <1000 ~ '2',
                                                 Observed >= 1000 & Observed <2000 ~ '3',
                                                 Observed >= 2000 & Observed <3000 ~ '4',
                                                 Observed >= 3000 ~ '5'
                                                 ))

# Convert 'Observed_grouped' to a factor with levels ordered from largest to smallest
sc_zcta_sf$Observed_grouped <- factor(sc_zcta_sf$Observed_grouped, 
                                      levels = c("5", "4", "3", "2", "1"),
                                      labels = c("3,000 and above", "2,000 - 3,000", "1,000 - 2,000", "500 - 1,000", "0 - 500"))

# Plot using ggplot2
ggplot(sc_zcta_sf) + 
   geom_sf(aes(fill = Observed_grouped), color = 'gray20') + 
   scale_fill_manual(values = c('#4d004b', '#810f7c', '#8c96c6',  '#bfd3e6', '#d0d1e6'), 
                     name = "Hospitalizations", 
                     na.translate = TRUE, 
                     na.value = 'white', 
                     labels = c("3,000 and above", "2,000 - 3,000", "1,000 - 2,000", "500 - 1,000", "0 - 500", "No Data")) + 
   ggtitle("Total observed hospitalization counts in 352 ZIP codes based on SC RFA data") + 
   theme_void() + 
   theme(
      plot.title = element_text(hjust = 0.5, size = 30),
      legend.justification = c(0.1, 0.1), 
      legend.position = c(0.1, 0.1),
      legend.title = element_text(size = 28),
      legend.key.size = unit(1, 'cm'),
      legend.text = element_text(size = 28)
   )






###estimates-----------
sc_zcta_sf = mutate(sc_zcta_sf, 
                    Estimated_grouped = case_when(Estimated < 500 ~ '1',
                                                 Estimated >= 500 & Estimated <1000 ~ '2',
                                                 Estimated >= 1000 & Estimated <2000 ~ '3',
                                                 Estimated >= 2000 & Estimated <3000 ~ '4',
                                                 Estimated >= 3000 ~ '5'))


# Convert 'Estimated_grouped' to a factor with levels ordered from largest to smallest
sc_zcta_sf$Estimated_grouped <- factor(sc_zcta_sf$Estimated_grouped, 
                                      levels = c("5", "4", "3", "2", "1"),
                                      labels = c("3,000 and above", "2,000 - 3,000", "1,000 - 2,000", "500 - 1,000", "0 - 500"))

# Plot using ggplot2
ggplot(sc_zcta_sf) + 
   geom_sf(aes(fill = Estimated_grouped), color = 'gray20') + 
   scale_fill_manual(values = c('#4d004b', '#810f7c',  '#8c96c6',  '#bfd3e6', '#d0d1e6'), 
                     name = "Hospitalizations", 
                     na.translate = TRUE, 
                     na.value = 'white', 
                     labels = c("3,000 and above", "2,000 - 3,000", "1,000 - 2,000", "500 - 1,000", "0 - 500", "No Data")) + 
   ggtitle("Total estimated hospitalization counts in 352 ZIP codes") + 
   theme_void() + 
   theme(
      plot.title = element_text(hjust = 0.5, size = 30),
      legend.justification = c(0.1, 0.1), 
      legend.position = c(0.1, 0.1),
      legend.title = element_text(size = 28),
      legend.key.size = unit(1, 'cm'),
      legend.text = element_text(size = 28)
   )



###Accuracy------------
sc_zcta_sf = mutate(sc_zcta_sf, 
                    Accuracy_grouped = case_when(Accuracy >= .45 & Accuracy <.50 ~ '1',
                                                 Accuracy >= .50 & Accuracy <.60 ~ '2',
                                                 Accuracy >= .60 & Accuracy <.70 ~ '3',
                                                 Accuracy >= .70 & Accuracy <.80 ~ '4',
                                                 Accuracy >= .80 & Accuracy <.90 ~ '5',
                                                 Accuracy >= .90 & Accuracy <1.00 ~ '6'))

#scale_fill_manual(values = c('#C099CC', '#FF99CC', '#FF9999', '#CC6666', '#993333', '#660000'), name="Opioid \nHospitalizations", 

# Convert 'Accuracy_grouped' to a factor with levels ordered from largest to smallest
sc_zcta_sf$Accuracy_grouped <- factor(sc_zcta_sf$Accuracy_grouped, 
                                      levels = c("6", "5", "4", "3", "2", "1"),
                                      labels = c("90%-100%", "80%-90%", "70%-80%", "60%-70%", "50%-60%", "40%-50%"))

# Plot using ggplot2
ggplot(sc_zcta_sf) + 
   geom_sf(aes(fill = Accuracy_grouped), color = 'gray0') + 
   scale_fill_manual(values = c('#016c59', '#1c9099','#d9ef8b', '#ffffcc', '#fdd49e', '#fc9272'),# '#238b45', '#41ae76', '#a1d99b','#d9ef8b', '#ffffcc', '#fdd49e 
                     name = "Accuracy", 
                     na.translate = TRUE, 
                     na.value = 'white', 
                     labels = c("90%-100%", "80%-90%", "70%-80%", "60%-70%", "50%-60%", "40%-50%", "No Data")) + 
   ggtitle("Percent agreement accuracy of hospitalization estimates in 352 ZIP codes") + 
   theme_void() + 
   theme(
      plot.title = element_text(hjust = 0.5, size = 30),
      legend.justification = c(0.1, 0.1), 
      legend.position = c(0.1, 0.1),
      legend.title = element_text(size = 28),
      legend.key.size = unit(1, 'cm'),
      legend.text = element_text(size = 28)
   )










#Top 100 ZIP Codes with Highest Differences in Observed and Estimated COVID-19 Hospitalizations
# Create a data frame with the required variables
data <- data.frame(ZIP = total_hospitalizations$ZIP,
                   Observed = total_hospitalizations$total_observed_hospitalizations,
                   Estimated = total_hospitalizations$total_predicted_hospitalizations3)

# Calculate the difference between Observed and Estimated
data <- data %>%
  mutate(Difference = abs(Observed - Estimated))

# Filter top 50 ZIP codes based on the difference
data_top <- data %>%
  arrange(desc(Difference)) %>%
  head(100) %>%
  mutate(ZIP = factor(ZIP, levels = ZIP))

# Convert data from wide to long format
data_long <- pivot_longer(data_top, cols = c(Observed, Estimated), names_to = "Type", values_to = "Hospitalizations")

# Ensure 'Observed' is first
data_long$Type <- factor(data_long$Type, levels = c("Observed", "Estimated"))


# Plot using ggplot
p <- ggplot(data_long, aes(x = ZIP, y = Hospitalizations, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "ZIP Code", y = "COVID-19 Hospitalizations",
       title = "Top 100 ZIP Codes with Highest Differences in Observed and Estimated COVID-19 Hospitalizations") +
  scale_fill_manual(values = c("blue", "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8),   # Increase x-axis tick label size
        axis.text.y = element_text(size = 12),                         # Increase y-axis tick label size
        axis.title.x = element_text(size = 14, margin = margin(t = 15)), # Increase x-axis label size with a margin
        axis.title.y = element_text(size = 14, margin = margin(r = 15)), # Increase y-axis label size with a margin
        legend.text = element_text(size = 14),                         # Increase legend text size
        legend.title = element_text(size = 16),                        # Increase legend title size
        plot.title = element_text(size = 14))                          # Increase plot title size

# Display the plot
print(p)


# Add extra text annotations
# p + annotate("text", x = 71, y = 3450, label = "For training: Dec 1, 2020 - Feb 28, 2021", size = 6.5, color = "black") +
#   annotate("text", x = 71, y = 3250, label = "For testing: Aug 1, 2021 - Nov 30, 2021", size = 6.5, color = "black")





#Top 100 ZIP Codes with Lowest Differences in Observed and Estimated COVID-19 Hospitalizations
# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Assume total_hospitalizations is your data frame
# Create a data frame with the required variables
data <- data.frame(ZIP = total_hospitalizations$ZIP,
                   Observed = total_hospitalizations$total_observed_hospitalizations,
                   Estimated = total_hospitalizations$total_predicted_hospitalizations3)

# Calculate the difference between Observed and Estimated
data <- data %>%
  mutate(Difference = abs(Observed - Estimated))

# Filter top 100 ZIP codes based on the difference
data_top <- data %>%
  arrange(Difference) %>%
  head(100) %>%
  mutate(ZIP = factor(ZIP, levels = ZIP))

# Convert data from wide to long format
data_long <- pivot_longer(data_top, cols = c(Observed, Estimated), names_to = "Type", values_to = "Hospitalizations")

# Ensure 'Observed' is first
data_long$Type <- factor(data_long$Type, levels = c("Observed", "Estimated"))

# Plot using ggplot
p <- ggplot(data_long, aes(x = ZIP, y = Hospitalizations, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "ZIP Code", y = "COVID-19 Hospitalizations",
       title = "Top 100 ZIP Codes with Lowest Differences in Observed and Estimated COVID-19 Hospitalizations") +
  scale_fill_manual(values = c("blue", "red")) +
  ylim(0, 4000) +  # Set the Y scale to 0-4500
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8),   # Increase x-axis tick label size
        axis.text.y = element_text(size = 12),                         # Increase y-axis tick label size
        axis.title.x = element_text(size = 14, margin = margin(t = 15)), # Increase x-axis label size with a margin
        axis.title.y = element_text(size = 14, margin = margin(r = 15)), # Increase y-axis label size with a margin
        legend.text = element_text(size = 14),                         # Increase legend text size
        legend.title = element_text(size = 16),                        # Increase legend title size
        plot.title = element_text(size = 14))                          # Increase plot title size

# Display the plot
print(p)

























######Accuracy for the imputed data--------------

# Remove the specified columns
total_hospitalizations <- total_hospitalizations[, !(names(total_hospitalizations) %in% c("A_i_1", "A_i_2", "A_i_3", "A_i_4"))]

# Remove the rows where "total_predicted_hospitalizations_before_imputation" is not equal to 0
total_hospitalizations <- total_hospitalizations[total_hospitalizations$total_predicted_hospitalizations_before_imputation == 0, ]



# Ai
predicted_variables <- c("total_predicted_hospitalizations1", "total_predicted_hospitalizations2", 
                         "total_predicted_hospitalizations3", "total_predicted_hospitalizations4"
)

for (i in seq_along(predicted_variables)) {
  col_name <- paste0("A_i_", i)
  total_hospitalizations[[col_name]] <- with(total_hospitalizations, pmin(total_observed_hospitalizations, get(predicted_variables[i])) /
                                               pmax(total_observed_hospitalizations, get(predicted_variables[i])))
}



A_i_columns <- paste0("A_i_", 1:4)
# Subset the data frame to include only the A_i columns
A_i_data <- total_hospitalizations[A_i_columns]
# Use the summary function to get the five-number summary for each A_i column
summary_A_i <- apply(A_i_data, 2, summary)
# Print the result
print(summary_A_i)
n_distinct(total_hospitalizations$ZIP)







#MAPE-------------
# Loop through each predicted variable to compute MAPE
for (i in seq_along(predicted_variables)) {
   col_name <- paste0("MAPE_", i)
   total_hospitalizations[[col_name]] <- with(total_hospitalizations, 
                                              abs(get(predicted_variables[i]) - total_observed_hospitalizations) / total_observed_hospitalizations)
}

# Extract only the MAPE columns
MAPE_columns <- paste0("MAPE_", 1:4)
MAPE_data <- total_hospitalizations[MAPE_columns]

# Compute the mean MAPE for each prediction column
(mean_MAPE <- apply(MAPE_data, 2, mean, na.rm = TRUE))








# Create a data frame with the required variables
data <- data.frame(ZIP = total_hospitalizations$ZIP,
                   Observed = total_hospitalizations$total_observed_hospitalizations,
                   Imputed = total_hospitalizations$total_predicted_hospitalizations3)

# Calculate the difference between Observed and Imputed
data <- data %>%
  mutate(Difference = abs(Observed - Imputed))

# Filter top 100 ZIP codes based on the difference
data_top <- data %>%
  arrange(desc(Difference)) %>%
  head(100) %>%
  mutate(ZIP = factor(ZIP, levels = ZIP))

# Convert data from wide to long format
data_long <- pivot_longer(data_top, cols = c(Observed, Imputed), names_to = "Type", values_to = "Hospitalizations")

# Ensure 'Observed' is first
data_long$Type <- factor(data_long$Type, levels = c("Observed", "Imputed"))


# Plot using ggplot
p <- ggplot(data_long, aes(x = ZIP, y = Hospitalizations, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "ZIP Code", y = "COVID-19 Hospitalizations",
       title = "Top 100 ZIP Codes with Highest Differences in Observed and Imputed COVID-19 Hospitalizations") +
  scale_fill_manual(values = c("blue", "red")) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8),   # Increase x-axis tick label size
        axis.text.y = element_text(size = 12),                         # Increase y-axis tick label size
        axis.title.x = element_text(size = 14, margin = margin(t = 15)), # Increase x-axis label size with a margin
        axis.title.y = element_text(size = 14, margin = margin(r = 15)), # Increase y-axis label size with a margin
        legend.text = element_text(size = 14),                         # Increase legend text size
        legend.title = element_text(size = 16),                        # Increase legend title size
        plot.title = element_text(size = 14))                          # Increase plot title size

# Display the plot
print(p)


# Add extra text annotations
# p + annotate("text", x = 71, y = 3450, label = "For training: Dec 1, 2020 - Feb 28, 2021", size = 6.5, color = "black") +
#   annotate("text", x = 71, y = 3250, label = "For testing: Aug 1, 2021 - Nov 30, 2021", size = 6.5, color = "black")






#Top 50 ZIP Codes with Lowest Differences in Observed and Imputed COVID-19 Hospitalizations
# Create a data frame with the required variables
data <- data.frame(ZIP = total_hospitalizations$ZIP,
                   Observed = total_hospitalizations$total_observed_hospitalizations,
                   Imputed = total_hospitalizations$total_predicted_hospitalizations3)

# Calculate the difference between Observed and Imputed
data <- data %>%
  mutate(Difference = abs(Observed - Imputed))

# Filter top 100 ZIP codes based on the difference
data_top <- data %>%
  arrange(Difference) %>%
  head(100) %>%
  mutate(ZIP = factor(ZIP, levels = ZIP))

# Convert data from wide to long format
data_long <- pivot_longer(data_top, cols = c(Observed, Imputed), names_to = "Type", values_to = "Hospitalizations")

# Ensure 'Observed' is first
data_long$Type <- factor(data_long$Type, levels = c("Observed", "Imputed"))

# Plot using ggplot
p <- ggplot(data_long, aes(x = ZIP, y = Hospitalizations, fill = Type)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "ZIP Code", y = "COVID-19 Hospitalizations",
       title = "Top 100 ZIP Codes with Lowest Differences in Observed and Imputed COVID-19 Hospitalizations") +
  scale_fill_manual(values = c("blue", "red")) +
  ylim(0, 4000) +  # Set the Y scale to 0-4000
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8),   # Increase x-axis tick label size
        axis.text.y = element_text(size = 12),                         # Increase y-axis tick label size
        axis.title.x = element_text(size = 14, margin = margin(t = 15)), # Increase x-axis label size with a margin
        axis.title.y = element_text(size = 14, margin = margin(r = 15)), # Increase y-axis label size with a margin
        legend.text = element_text(size = 14),                         # Increase legend text size
        legend.title = element_text(size = 16),                        # Increase legend title size
        plot.title = element_text(size = 14))                          # Increase plot title size

# Display the plot
print(p)


























