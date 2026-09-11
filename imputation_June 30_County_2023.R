library(dplyr)
library(readxl)
library(openxlsx)




## adding demographic variables
# Load the county dataset--------
census_data <- read_csv("/Users/tanvirahammed/Library/CloudStorage/Box-Box/BoxPHI-PHMR Projects/Tanvir/Project 2 (Prediction)/Data/Age and Sex by ZCTA.csv")
census_data_race <- read_csv("/Users/tanvirahammed/Library/CloudStorage/Box-Box/BoxPHI-PHMR Projects/Tanvir/Project 2 (Prediction)/Data/Race by ZCTA.csv")
census_data <- left_join(census_data, census_data_race, by = "ZCTA") # Left join 

census_data <- mutate(census_data,
                      age.18.under = under5 + `5-9 years` + `10-14 years` + `15-19 years` * 0.6,                          
                      age.18.44 = `15-19 years` * 0.4 + `20-24 years` + `25-29 years` + `30-34 years` + `35-39 years` + `40-44 years`,
                      age.45.64 = `45-49 years` + `50-54 years` + `55-59 years` + `60-64 years`,
                      age.65.over = `65-69 years` + `70-74 years` + `75-79 years` + `80-84 years` + over85,
                      ## total study population can be specified differently 
                      total.population.study = age.18.under+age.18.44 + age.45.64 + age.65.over)


census_data <- mutate(census_data,
                      `Percent age 0-17` = age.18.under / Total_population,
                      `Percent age 18-44` = age.18.44 / Total_population,
                      `Percent age 45-64` = age.45.64 / Total_population,
                      `Percent age 65 years and over` = age.65.over / Total_population,
                      Percent_white = Race_white/ Total_population,
                      `Percent female` = Total_Female/Total_population)


# Rename the variable from "zip_code" to "ZIP"
census_data <- census_data %>% rename(ZIP = ZCTA, Population=Total_population)


# Select only the desired variables
selected_dataset <- as.data.frame(census_data %>%
                                     dplyr::select(ZIP, Population, `Percent female`, Percent_white, `Percent age 0-17`, `Percent age 18-44`, `Percent age 45-64`, `Percent age 65 years and over`))

selected_dataset <- selected_dataset %>%
   mutate_at(vars(`Percent_white`, `Percent age 0-17`, `Percent age 18-44`, `Percent age 45-64`, `Percent age 65 years and over`), as.numeric)

#creating Percent_male & Percent_white
selected_dataset <- selected_dataset %>%
   mutate(
      Percent_male = 1 - `Percent female`,
      `Percent Non-white` = 1 - `Percent_white`
   )



## adding county dataset--------
dataset1 <- read_excel("/Users/tanvirahammed/Library/CloudStorage/Box-Box/BoxSecure-DPHS-Opiod/COVID registry/Tanvir/Data/12.28.21 SC Rural Healthcare data by Zip.xlsx")

# Rename the variable from "county_code" to "county"
dataset1 <- dataset1 %>%
   rename(ZIP = zip_code)


selected_dataset <- selected_dataset  %>%
   left_join(dataset1 %>%
                dplyr::select(ZIP, county), 
             by = "ZIP")

selected_dataset$county[selected_dataset$ZIP == 29486] <- "Berkeley County"

summary_data <- selected_dataset %>%
   group_by(county) %>%
   summarize(
      Total_Population = sum(Population, na.rm = TRUE),
      mean_Percent_female = mean(`Percent female`, na.rm = TRUE),
      mean_Percent_Non_white = mean(`Percent Non-white`, na.rm = TRUE),
      mean_Percent_age_0_17 = mean(`Percent age 0-17`, na.rm = TRUE),
      mean_Percent_age_18_44 = mean(`Percent age 18-44`, na.rm = TRUE),
      mean_Percent_age_45_64 = mean(`Percent age 45-64`, na.rm = TRUE),
      mean_Percent_age_65_over = mean(`Percent age 65 years and over`, na.rm = TRUE),
      mean_Percent_male = mean(`Percent_male`, na.rm = TRUE),
      mean_Percent_white = mean(`Percent_white`, na.rm = TRUE)
   )

# Remove rows with NA in the county column
summary_data <- summary_data[!is.na(summary_data$county), ]






# # Load the test dataset--------
Final_test_data <- read_excel("/Users/tanvirahammed/Library/CloudStorage/Box-Box/BoxSecure-DPHS-Opiod/COVID registry/Tanvir/Results/Final_test_data_county_June.xlsx")

# Select only the specified columns
Final_test_data <- Final_test_data %>%
   dplyr::select(county, SEX, age_group, race_group, hospitalisation_RFA, log_subpopulation_size,
                 predicted_hospitalisation6)


# Left join
Final_test_data <- left_join(Final_test_data , summary_data, by = "county")

# Load the removed_county_data dataset--------
removed_county_data <- read_excel("/Users/tanvirahammed/Library/CloudStorage/Box-Box/BoxPHI-PHMR Projects/Tanvir/Data/Estimating state-wide COVID-19 hospitalizations using local health system data/removed_county_data.xlsx")

# Apply the function to the "county" column-----
# Define a function to add "County" if it's missing
add_County_if_missing <- function(county_name) {
   if (!grepl(" County$", county_name)) {
      return(paste(county_name, "County"))
   }
   return(county_name)
}
removed_county_data$county <- sapply(removed_county_data$county, add_County_if_missing)


removed_county_data$SEX <- ifelse(is.na(removed_county_data$SEX), NA, 
                                  ifelse(removed_county_data$SEX == "M", "Male", "Female"))
removed_county_data  <- removed_county_data %>%
   mutate(RACE = case_when(
      RACE == 1 ~ "White",
      RACE == 2 ~ "African-American",
      RACE == 3 ~ "Asian",
      RACE == 4 ~ "American Indian",
      RACE == 5 ~ "Other",
      RACE == 6 ~ "Hispanic",
      RACE == 7 ~ NA_character_,
      TRUE ~ as.character(RACE)  # Keep other values as is
   ))


# Create age_group variable
removed_county_data <- removed_county_data %>%
   mutate(age_group = case_when(
      AGRP %in% c("AGE 00-04", "AGE 05-11", "AGE 12-15", "AGE 16-17") ~ "<18",
      AGRP %in% c("AGE 18-24", "AGE 25-29", "AGE 30-34", "AGE 35-39", "AGE 40-44") ~ "18-44",
      AGRP %in% c("AGE 45-49", "AGE 50-54", "AGE 55-59", "AGE 60-64") ~ "45-64",
      AGRP %in% c("AGE 65-69", "AGE 70-74", "AGE 75-79", "AGE 80-84", "AGE 85+") ~ "65+",
      TRUE ~ NA_character_  # For any unexpected values
   ))

# Create race_group variable
removed_county_data <- removed_county_data %>%
   filter(!is.na(RACE)) %>%
   mutate(race_group = case_when(
      RACE == "White" ~ "White",
      RACE != "White" ~ "Non-white"
   ))

# Convert 'Year_Month' to Date format
removed_county_data$Date <- as.Date(paste0(removed_county_data$Year_Month, "-01"), format = "%Y-%m-%d")

# Filter data for the test set (July 1 and Dec 31, 2021)
removed_county_data <- removed_county_data %>%
   filter(Date >= as.Date("2023-07-01") & Date <= as.Date("2023-12-31"))


removed_county_data <- removed_county_data %>%
   group_by(county, SEX, age_group,  race_group) %>%
   summarize(
      hospitalisation_RFA = n()
   )
removed_county_data$predicted_hospitalisation6 <- NA

removed_county_data <- removed_county_data %>%
   filter(county != "NA County")


# Left join
removed_county_data <- left_join(removed_county_data , summary_data, by = "county")


removed_county_data <- removed_county_data %>%
   mutate(
      new_subpopulation_size = case_when(
         SEX == "Male" & age_group == "<18" & race_group == "White" ~ Total_Population * (mean_Percent_male) * (mean_Percent_age_0_17) * (mean_Percent_white),
         SEX == "Male" & age_group == "18-44" & race_group == "White" ~ Total_Population * (mean_Percent_male) * (mean_Percent_age_18_44) * (mean_Percent_white),
         SEX == "Male" & age_group == "45-64" & race_group == "White" ~ Total_Population * (mean_Percent_male) * (mean_Percent_age_45_64) * (mean_Percent_white),
         SEX == "Male" & age_group == "65+" & race_group == "White" ~ Total_Population * (mean_Percent_male) * (mean_Percent_age_65_over) * (mean_Percent_white),

         SEX == "Female" & age_group == "<18" & race_group == "White" ~ Total_Population * (mean_Percent_female) * (mean_Percent_age_0_17) * (mean_Percent_white),
         SEX == "Female" & age_group == "18-44" & race_group == "White" ~ Total_Population * (mean_Percent_female) * (mean_Percent_age_18_44) * (mean_Percent_white),
         SEX == "Female" & age_group == "45-64" & race_group == "White" ~ Total_Population * (mean_Percent_female) * (mean_Percent_age_45_64) * (mean_Percent_white),
         SEX == "Female" & age_group == "65+" & race_group == "White" ~ Total_Population * (mean_Percent_female) * (mean_Percent_age_65_over) * (mean_Percent_white),

         SEX == "Male" & age_group == "<18" & race_group == "Non-white" ~ Total_Population * (mean_Percent_male) * (mean_Percent_age_0_17) * (mean_Percent_Non_white),
         SEX == "Male" & age_group == "18-44" & race_group == "Non-white" ~ Total_Population * (mean_Percent_male) * (mean_Percent_age_18_44) * (mean_Percent_Non_white),
         SEX == "Male" & age_group == "45-64" & race_group == "Non-white" ~ Total_Population * (mean_Percent_male) * (mean_Percent_age_45_64) * (mean_Percent_Non_white),
         SEX == "Male" & age_group == "65+" & race_group == "Non-white" ~ Total_Population * (mean_Percent_male) * (mean_Percent_age_65_over) * (mean_Percent_Non_white),

         SEX == "Female" & age_group == "<18" & race_group == "Non-white" ~ Total_Population * (mean_Percent_female) * (mean_Percent_age_0_17) * (mean_Percent_Non_white),
         SEX == "Female" & age_group == "18-44" & race_group == "Non-white" ~ Total_Population * (mean_Percent_female) * (mean_Percent_age_18_44) * (mean_Percent_Non_white),
         SEX == "Female" & age_group == "45-64" & race_group == "Non-white" ~ Total_Population * (mean_Percent_female) * (mean_Percent_age_45_64) * (mean_Percent_Non_white),
         SEX == "Female" & age_group == "65+" & race_group == "Non-white" ~ Total_Population * (mean_Percent_female) * (mean_Percent_age_65_over) * (mean_Percent_Non_white),

         TRUE ~ NA_real_  # Assign NA for any other case
      )
   )


removed_county_data$new_subpopulation_size<-round(removed_county_data$new_subpopulation_size, 0)
removed_county_data$log_subpopulation_size<-log(removed_county_data$new_subpopulation_size)
removed_county_data$new_subpopulation_size<- NULL


#
# 
# removed_county_data <- removed_county_data %>%
#    dplyr::select(county, SEX, age_group, race_group, hospitalisation_RFA, 
#                  )


# # Define all possible combinations
# all_combinations <- expand.grid(
#    county = unique(removed_county_data$county),
#    SEX = unique(removed_county_data$SEX),
#    age_group = unique(removed_county_data$age_group),
#    race_group = unique(removed_county_data$race_group)
# )
# 
# # Left join with test_data to fill missing combinations
# removed_county_data <- left_join(all_combinations, removed_county_data, by = c("county", "SEX", "age_group", "race_group"))
# 
# 
# # Replace NA values in occurrences with 0
# removed_county_data <- removed_county_data %>%
#    mutate(hospitalisation_RFA = replace_na(hospitalisation_RFA, 0))
# 

# # full join to combine both datasets
# 
# removed_county_data <- removed_county_data %>%
#    filter(!is.na(Total_Population))

# Automatically clean column names to be valid R names
colnames(removed_county_data) <- make.names(colnames(removed_county_data))
colnames(Final_test_data) <- make.names(colnames(Final_test_data))

# Perform the full join using correct column names (with spaces)
imputation_data <- full_join(Final_test_data, removed_county_data, by = c(
   "county", "SEX", "age_group", "race_group", "hospitalisation_RFA", 
   "Total_Population", "mean_Percent_female", "mean_Percent_Non_white", 
   "mean_Percent_age_0_17", "mean_Percent_age_18_44", "mean_Percent_age_45_64", 
   "mean_Percent_age_65_over", "mean_Percent_male", "mean_Percent_white",
   "log_subpopulation_size", "predicted_hospitalisation6"
))

imputation_data <- imputation_data %>%
   dplyr::select( -mean_Percent_male, -mean_Percent_white, -Total_Population
   )

imputation_data$county <- as.factor(imputation_data$county)
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
cols_with_na <- cols_with_na[cols_with_na != "predicted_hospitalisation6"]

# Print columns with NAs other than the target column
print(cols_with_na)


# Step 3: Remove rows with NAs in these columns
imputation_data_clean <- imputation_data[complete.cases(imputation_data[, cols_with_na]), ]

# Confirm that the cleaned dataset has no missing values in other columns
summary(imputation_data_clean)
imputation_data_clean<- as.data.frame(imputation_data_clean)
imputation_data_clean1 <- imputation_data_clean


imputation_data_clean$hospitalisation_RFA <- NULL


# Run missForest on the cleaned dataset including the column with missing values
set.seed(1234)
missForest_result <- missForest(imputation_data_clean)

# imputation_data_clean_numeric <- imputation_data_clean %>%
#   dplyr::select(county, hospitalisation_RFA, predicted_hospitalisation6, mean_Percent_female, mean_Percent_Non_white,  mean_Percent_age_0_19,  mean_Percent_age_20_44, 
#          mean_Percent_age_45_64, mean_Percent_age_65_over)
set.seed(1234)
mice_imputed <- data.frame(
   county = imputation_data_clean$county,
   SEX = imputation_data_clean$SEX,
   age_group = imputation_data_clean$age_group,
   race_group = imputation_data_clean$race_group,
   #original = imputation_data_clean$hospitalisation_RFA,
   total_predicted_hospitalizations_before_imputation = imputation_data_clean$predicted_hospitalisation6,
   total_predicted_hospitalizations1 = missForest_result$ximp$predicted_hospitalisation6,
   imputed_pmm = complete(mice(imputation_data_clean, method = "pmm"))$predicted_hospitalisation6,
   imputed_cart = complete(mice(imputation_data_clean, method = "cart"))$predicted_hospitalisation6,
   imputed_lasso = complete(mice(imputation_data_clean, method = "lasso.norm"))$predicted_hospitalisation6
)

mice_imputed <- mice_imputed %>%
   left_join(imputation_data_clean1 %>%
                dplyr::select(county, SEX, age_group, race_group,
                              hospitalisation_RFA), by = c("county", "SEX", "age_group", "race_group"))




total_hospitalizations <- mice_imputed %>%
   group_by(county) %>%
   summarise(
      total_observed_hospitalizations = sum(hospitalisation_RFA, na.rm = TRUE),
      total_predicted_hospitalizations_before_imputation = sum(total_predicted_hospitalizations_before_imputation, na.rm = TRUE),
      total_predicted_hospitalizations1 = sum(total_predicted_hospitalizations1, na.rm = TRUE),
      total_predicted_hospitalizations2 = sum(imputed_pmm, na.rm = TRUE),
      total_predicted_hospitalizations3 = sum(imputed_cart, na.rm = TRUE),
      total_predicted_hospitalizations4 = sum(imputed_lasso, na.rm = TRUE)
   )


# # Round all numeric columns to 0 decimal places
# total_hospitalizations <- total_hospitalizations %>%
#    mutate(across(where(is.numeric), ~ round(., 0)))


#write.xlsx(total_hospitalizations, "/Users/tanvirahammed/Library/CloudStorage/Box-Box/BoxPHI-PHMR Projects/Tanvir/Data/Final Data/total_hospitalizations_county_June_imputation.xlsx", rowNames = FALSE)
## Filter rows where total_observed_hospitalizations is greater than 50
#total_hospitalizations <- total_hospitalizations[total_hospitalizations$total_observed_hospitalizations > 100, ]



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

n_distinct(total_hospitalizations$county)




#Accuracy for the imputed data--------------

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

n_distinct(total_hospitalizations$county)




## Map of hospitalization observed/estimates/accuracy in all 352 county codes ------------------------
#download the shapefile for county codes of a state using tigris package. 
sc_counties <- tigris::counties(state = "SC", year = 2021, class = "sf")


# Create a data frame with the required variables
hosp_county_esti_impu <- data.frame(county = as.character(total_hospitalizations$county),
                                    Observed = total_hospitalizations$total_observed_hospitalizations,
                                    Estimated = total_hospitalizations$total_predicted_hospitalizations3,
                                    Accuracy = total_hospitalizations$A_i_3)


#Transform county codes to ZCTAs first and merge your data with sc_counties by the ZCTA. 
sc_counties = left_join(sc_counties, hosp_county_esti_impu 
                        %>% dplyr::select(Observed, Estimated, Accuracy, NAMELSAD = county), by = 'NAMELSAD')


###observed------------
sc_counties = mutate(sc_counties, 
                     Observed_grouped = case_when(Observed < 1000 ~ '1',
                                                  Observed >= 1000 & Observed <5000 ~ '2',
                                                  Observed >= 5000 & Observed <10000 ~ '3',
                                                  Observed >= 10000 & Observed <20000 ~ '4',
                                                  Observed >= 20000 & Observed <30000 ~ '5'))

# Convert 'Observed_grouped' to a factor with levels ordered from largest to smallest
sc_counties$Observed_grouped <- factor(sc_counties$Observed_grouped, 
                                       levels = c("5", "4", "3", "2", "1"),
                                       labels = c("20,000 - 30,000", "10,000 - 20,000", "5,000 - 10,000", "1,000 - 5,000", "0 - 1,000"))

# Plot using ggplot2
ggplot(sc_counties) + 
   geom_sf(aes(fill = Observed_grouped), color = 'gray20') + 
   scale_fill_manual(values = c('#4d004b', '#810f7c', '#8c96c6',  '#bfd3e6', '#d0d1e6'), #'#023858','#2b8cbe', '#7bccc4', '#bae4bc', '#f0f9e8'
                     name = "Hospitalizations", 
                     na.translate = TRUE, 
                     na.value = 'white', 
                     labels = c("20,000 - 30,000", "10,000 - 20,000", "5,000 - 10,000", "1,000 - 5,000", "0 - 1,000", "No Data")) + 
   ggtitle("Total observed hospitalization counts in all 46 counties based on SC RFA data") + 
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
sc_counties = mutate(sc_counties, 
                     Estimated_grouped = case_when(Estimated < 1000 ~ '1',
                                                   Estimated >= 1000 & Estimated <5000 ~ '2',
                                                   Estimated >= 5000 & Estimated <10000 ~ '3',
                                                   Estimated >= 10000 & Estimated <20000 ~ '4',
                                                   Estimated >= 20000 & Estimated <30000 ~ '5'))

# Convert 'Estimated_grouped' to a factor with levels ordered from largest to smallest
sc_counties$Estimated_grouped <- factor(sc_counties$Estimated_grouped, 
                                        levels = c("5", "4", "3", "2", "1"),
                                        labels = c("20,000 - 30,000", "10,000 - 20,000", "5,000 - 10,000", "1,000 - 5,000", "0 - 1,000"))

# Plot using ggplot2
ggplot(sc_counties) + 
   geom_sf(aes(fill = Estimated_grouped), color = 'gray20') + 
   scale_fill_manual(values = c('#4d004b', '#810f7c', '#8c96c6', '#bfd3e6', '#d0d1e6'), 
                     name = "Hospitalizations", 
                     na.translate = TRUE, 
                     na.value = 'white', 
                     labels = c("20,000 - 30,000", "10,000 - 20,000", "5,000 - 10,000", "1,000 - 5,000", "0 - 1,000", "No Data")) + 
   ggtitle("Total estimated hospitalization counts in all 46 counties") + 
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
sc_counties = mutate(sc_counties, 
                     Accuracy_grouped = case_when(Accuracy >= .50 & Accuracy <.60 ~ '1',
                                                  Accuracy >= .60 & Accuracy <.70 ~ '2',
                                                  Accuracy >= .70 & Accuracy <.80 ~ '3',
                                                  Accuracy >= .80 & Accuracy <.90 ~ '4',
                                                  Accuracy >= .90 & Accuracy <1.00 ~ '5'))


# Convert 'Accuracy_grouped' to a factor with levels ordered from largest to smallest
sc_counties$Accuracy_grouped <- factor(sc_counties$Accuracy_grouped, 
                                       levels = c("5", "4", "3", "2", "1"),
                                       labels = c("90%-100%", "80%-90%", "70%-80%", "60%-70%", "50%-60%"))

# Plot using ggplot2
ggplot(sc_counties) + 
   geom_sf(aes(fill = Accuracy_grouped), color = 'gray0') + 
   scale_fill_manual(values = c('#016c59', '#1c9099','#d9ef8b', '#ffffcc', '#fdd49e'), 
                     name = "Accuracy", 
                     na.translate = TRUE, 
                     na.value = 'white', 
                     labels = c("90%-100%", "80%-90%", "70%-80%", "60%-70%", "50%-60%", "No Data")) + 
   ggtitle("Percent agreement accuracy of hospitalization estimates in all 46 counties") + 
   theme_void() + 
   theme(
      plot.title = element_text(hjust = 0.5, size = 30),
      legend.justification = c(0.1, 0.1), 
      legend.position = c(0.1, 0.1),
      legend.title = element_text(size = 28),
      legend.key.size = unit(1, 'cm'),
      legend.text = element_text(size = 26)
   )










# Load required libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Assume total_hospitalizations is your data frame
# Create a data frame with the required variables
data <- data.frame(county = total_hospitalizations$county,
                   Observed = total_hospitalizations$total_observed_hospitalizations,
                   Estimated = total_hospitalizations$total_predicted_hospitalizations3)

# Calculate the difference between Observed and Estimated
data <- data %>%
   mutate(Difference = abs(Observed - Estimated))

# Reorder the county codes based on the Difference
data <- data %>%
   arrange(desc(Difference)) %>%
   mutate(county = factor(county, levels = county))

# Convert data from wide to long format
data_long <- pivot_longer(data, cols = c(Observed, Estimated), names_to = "Type", values_to = "Hospitalizations")

# Ensure 'Observed' is first
data_long$Type <- factor(data_long$Type, levels = c("Observed", "Estimated"))

# Plot using ggplot
p <- ggplot(data_long, aes(x = county, y = Hospitalizations, fill = Type)) +
   geom_bar(stat = "identity", position = "dodge") +
   labs(x = "County", y = "COVID-19 Hospitalizations",
        title = "Observed and Estimated COVID-19 Hospitalizations by County") +
   scale_fill_manual(values = c("blueviolet", "azure4")) +
   theme_minimal() +
   theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 10),   # Increase x-axis tick label size
         axis.text.y = element_text(size = 12),                         # Increase y-axis tick label size
         axis.title.x = element_text(size = 18, margin = margin(t = 15)), # Increase x-axis label size with a margin
         axis.title.y = element_text(size = 18, margin = margin(r = 15)), # Increase y-axis label size with a margin
         legend.text = element_text(size = 16),                         # Increase legend text size
         legend.title = element_text(size = 16),                        # Increase legend title size
         plot.title = element_text(size = 18))                          # Increase plot title size

# Display the plot
print(p)




# Create a data frame with the required variables
data <- data.frame(county = total_hospitalizations$county,
                   Observed = total_hospitalizations$total_observed_hospitalizations,
                   Imputed = total_hospitalizations$total_predicted_hospitalizations3)

# Calculate the difference between Observed and Imputed
data <- data %>%
   mutate(Difference = abs(Observed - Imputed))

# Reorder the county codes based on the Difference
data <- data %>%
   arrange(desc(Difference)) %>%
   mutate(county = factor(county, levels = county))

# Convert data from wide to long format
data_long <- pivot_longer(data, cols = c(Observed, Imputed), names_to = "Type", values_to = "Hospitalizations")

# Ensure 'Observed' is first
data_long$Type <- factor(data_long$Type, levels = c("Observed", "Imputed"))

# Plot using ggplot
p <- ggplot(data_long, aes(x = county, y = Hospitalizations, fill = Type)) +
   geom_bar(stat = "identity", position = "dodge") +
   labs(x = "County", y = "COVID-19 Hospitalizations",
        title = "Observed and Imputed COVID-19 Hospitalizations by County") +
   scale_fill_manual(values = c("blueviolet", "azure4")) +
   theme_minimal() +
   theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 10),   # Increase x-axis tick label size
         axis.text.y = element_text(size = 12),                         # Increase y-axis tick label size
         axis.title.x = element_text(size = 18, margin = margin(t = 15)), # Increase x-axis label size with a margin
         axis.title.y = element_text(size = 18, margin = margin(r = 15)), # Increase y-axis label size with a margin
         legend.text = element_text(size = 16),                         # Increase legend text size
         legend.title = element_text(size = 16),                        # Increase legend title size
         plot.title = element_text(size = 18))                          # Increase plot title size

# Display the plot
print(p)
