
library(dplyr)
library(tidyr)
library(stringr)
library(readxl)
library(openxlsx)
library(lme4)
library(ggplot2)




## adding demographic variables
# Load the county dataset--------
dataset1 <- read_excel("/Users/tanvirahammed/Library/CloudStorage/Box-Box/BoxSecure-DPHS-Opiod/COVID registry/Tanvir/Data/12.28.21 SC Rural Healthcare data by Zip.xlsx")

# Rename the variable from "zip_code" to "ZIP"
dataset1 <- dataset1 %>%
  rename(ZIP = zip_code)

# Select only the desired variables
selected_dataset <- as.data.frame(dataset1 %>%
                                    dplyr::select(ZIP, county))



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

# Left join with training_data to fill missing combinations
census_data <- left_join(census_data, selected_dataset, by = "ZIP")

# Select only the desired variables
selected_dataset <- as.data.frame(census_data %>%
                                     dplyr::select(ZIP, county, Population, `Percent female`, Percent_white, `Percent age 0-17`, `Percent age 18-44`, `Percent age 45-64`, `Percent age 65 years and over`))

selected_dataset <- selected_dataset %>%
   mutate_at(vars(`Percent_white`, `Percent age 0-17`, `Percent age 18-44`, `Percent age 45-64`, `Percent age 65 years and over`), as.numeric)

#creating Percent_male & Percent_white
selected_dataset <- selected_dataset %>%
   mutate(
      Percent_male = 1 - `Percent female`,
      `Percent Non-white` = 1 - `Percent_white`
   )

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



#Prisma Data---------
# dataset:merged_dataset_Table1
prisma <- readRDS("/Users/tanvirahammed/Library/CloudStorage/Box-Box/BoxSecure-DPHS-Opiod/COVID registry/Tanvir/Data/merged_dataset_Table1.rds")

(cross_tab <- table(prisma$SEX, prisma$`Data Source`))
(percentage_tab <- round(prop.table(cross_tab, margin = 2) * 100, 2))


# remove the specified columns
prisma <- prisma[, !(names(prisma) %in% c("Population", 
                                          "Percent female", 
                                          "Percent Non-white", 
                                          "Percent age 0-19", 
                                          "Percent age 20-44", 
                                          "Percent age 45-64", 
                                          "Percent age 65 years and over", 
                                          "BMI", 
                                          "PAYOR1", 
                                          "Benefit Plan"))]





## Merge the datasets based on the ZIP column 
prisma <- prisma  %>%
   left_join(selected_dataset, by = "ZIP")

prisma <- prisma %>%
   mutate_at(vars(Percent_male, `Percent female`, Percent_white, `Percent Non-white`, `Percent age 0-17`, `Percent age 18-44`, `Percent age 45-64`, `Percent age 65 years and over`), as.numeric)


# Subpopulation size 
prisma <- prisma %>%
   mutate(
      subpopulation_size = case_when(
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

prisma$subpopulation_size<-round(prisma$subpopulation_size, 0)




# Remove "county.y" and rename "county.x" to "county"
prisma <- prisma %>%
   dplyr::select(-county.y) %>%  # Remove the "county.y" column
   rename(county = county.x)  # Rename "county.x" to "county"



# Convert 'Year_Month' to Date format
prisma$Date <- as.Date(paste0(prisma$Year_Month, "-01"), format = "%Y-%m-%d")


#Table 2-----------------
# Filter data for training 
prisma <- prisma %>% 
   filter(Date >= as.Date("2023-01-01") & Date <= as.Date("2023-12-31"))


(cross_tab <- table(prisma$SEX, prisma$`Data Source`))
(percentage_tab <- round(prop.table(cross_tab, margin = 2) * 100, 2))

(cross_tab <- table(prisma$age_group, prisma$`Data Source`))
(percentage_tab <- round(prop.table(cross_tab, margin = 2) * 100, 2))

(cross_tab <- table(prisma$race_group, prisma$`Data Source`))
(percentage_tab <- round(prop.table(cross_tab, margin = 2) * 100, 2))



# Filter data for training (Dec 1, 2020, through June 30, 2021)
training_data <- prisma %>%
  filter(Date >= as.Date("2023-01-01") & Date <= as.Date("2023-06-30"))

# Filter data for the test set (July 1 and Dec 31, 2021)
test_data <- prisma %>%
  filter(Date >= as.Date("2023-07-01") & Date <= as.Date("2023-12-31"))



training_grouped_data <- training_data %>%
  group_by(county, SEX, age_group, race_group) %>%
  summarize(
    occurrences = n(),
    subpopulation_size = sum(subpopulation_size, na.rm = TRUE)
  )


# Define all possible combinations
all_combinations <- expand.grid(
  county = unique(training_grouped_data$county),
  SEX = unique(training_grouped_data$SEX),
  age_group = unique(training_grouped_data$age_group),
  race_group = unique(training_grouped_data$race_group)
)


# Left join with training_data to fill missing combinations
training_grouped_data_filled <- left_join(all_combinations, training_grouped_data, by = c("county", "SEX", "age_group", "race_group"))


## Merge the datasets based on the county column 
training_grouped_data_filled<- training_grouped_data_filled  %>%
  left_join(summary_data, by = "county")


# Subpopulation size 
training_grouped_data_filled <- training_grouped_data_filled %>%
   mutate(
      new_subpopulation_size = case_when(
         SEX == "Male" & age_group == "<18" & race_group == "White" ~ Total_Population * (mean_Percent_male ) * (mean_Percent_age_0_17 ) * (mean_Percent_white ),
         SEX == "Male" & age_group == "18-44" & race_group == "White" ~ Total_Population * (mean_Percent_male ) * (mean_Percent_age_18_44 ) * (mean_Percent_white ),
         SEX == "Male" & age_group == "45-64" & race_group == "White" ~ Total_Population * (mean_Percent_male ) * (mean_Percent_age_45_64 ) * (mean_Percent_white ),
         SEX == "Male" & age_group == "65+" & race_group == "White" ~ Total_Population * (mean_Percent_male ) * (mean_Percent_age_65_over ) * (mean_Percent_white ),
         SEX == "Female" & age_group == "<18" & race_group == "White" ~ Total_Population * (mean_Percent_female ) * (mean_Percent_age_0_17 ) * (mean_Percent_white ),
         SEX == "Female" & age_group == "18-44" & race_group == "White" ~ Total_Population * (mean_Percent_female ) * (mean_Percent_age_18_44 ) * (mean_Percent_white ),
         SEX == "Female" & age_group == "45-64" & race_group == "White" ~ Total_Population * (mean_Percent_female ) * (mean_Percent_age_45_64 ) * (mean_Percent_white ),
         SEX == "Female" & age_group == "65+" & race_group == "White" ~ Total_Population * (mean_Percent_female ) * (mean_Percent_age_65_over ) * (mean_Percent_white ),
         SEX == "Male" & age_group == "<18" & race_group == "Non-white" ~ Total_Population * (mean_Percent_male ) * (mean_Percent_age_0_17 ) * (mean_Percent_Non_white ),
         SEX == "Male" & age_group == "18-44" & race_group == "Non-white" ~ Total_Population * (mean_Percent_male ) * (mean_Percent_age_18_44 ) * (mean_Percent_Non_white ),
         SEX == "Male" & age_group == "45-64" & race_group == "Non-white" ~ Total_Population * (mean_Percent_male ) * (mean_Percent_age_45_64 ) * (mean_Percent_Non_white ),
         SEX == "Male" & age_group == "65+" & race_group == "Non-white" ~ Total_Population * (mean_Percent_male ) * (mean_Percent_age_65_over ) * (mean_Percent_Non_white ),
         SEX == "Female" & age_group == "<18" & race_group == "Non-white" ~ Total_Population * (mean_Percent_female ) * (mean_Percent_age_0_17 ) * (mean_Percent_Non_white ),
         SEX == "Female" & age_group == "18-44" & race_group == "Non-white" ~ Total_Population * (mean_Percent_female ) * (mean_Percent_age_18_44 ) * (mean_Percent_Non_white ),
         SEX == "Female" & age_group == "45-64" & race_group == "Non-white" ~ Total_Population * (mean_Percent_female ) * (mean_Percent_age_45_64 ) * (mean_Percent_Non_white ),
         SEX == "Female" & age_group == "65+" & race_group == "Non-white" ~ Total_Population * (mean_Percent_female ) * (mean_Percent_age_65_over ) * (mean_Percent_Non_white ),
         TRUE ~ NA_real_  # Assign NA for other cases
      )
   )


training_grouped_data_filled$new_subpopulation_size<-round(training_grouped_data_filled$new_subpopulation_size, 0)

# Replace NA values in occurrences with 0
training_grouped_data_filled <- training_grouped_data_filled %>%
  mutate(occurrences = replace_na(occurrences, 0))



# test_data-----------

# test_grouped_data <- test_data %>%
#   group_by(city, SEX, age_group, race_group) %>%
#   summarize(
#     occurrences = n(),
#     county = paste(county, collapse = ", ")
#   )
# 
# test_grouped_data <- test_grouped_data %>%
#   mutate(county = str_extract(county, "^[^,]+"))
# 
# 
# test_grouped_data$subpopulation_size <- prisma$`subpopulation_size`[
#   match(paste(test_grouped_data$city, test_grouped_data$SEX, test_grouped_data$age_group, test_grouped_data$race_group), 
#         paste(prisma$city, prisma$SEX, prisma$age_group, prisma$race_group))
# ]




test_grouped_data <- test_data %>%
  group_by(county, SEX, age_group, race_group) %>%
  summarize(
    occurrences = n(),
    subpopulation_size = sum(subpopulation_size, na.rm = TRUE)
  )


# Define all possible combinations
all_combinations <- expand.grid(
  county = unique(test_grouped_data$county),
  SEX = unique(test_grouped_data$SEX),
  age_group = unique(test_grouped_data$age_group),
  race_group = unique(test_grouped_data$race_group)
)


# Left join with test_data to fill missing combinations
test_grouped_data_filled <- left_join(all_combinations, test_grouped_data, by = c("county", "SEX", "age_group", "race_group"))


## Merge the datasets based on the county column 
test_grouped_data_filled<- test_grouped_data_filled  %>%
  left_join(summary_data, by = "county")


# Subpopulation size 
test_grouped_data_filled <- test_grouped_data_filled %>%
   mutate(
      new_subpopulation_size = case_when(
         SEX == "Male" & age_group == "<18" & race_group == "White" ~ Total_Population * (mean_Percent_male ) * (mean_Percent_age_0_17 ) * (mean_Percent_white ),
         SEX == "Male" & age_group == "18-44" & race_group == "White" ~ Total_Population * (mean_Percent_male ) * (mean_Percent_age_18_44 ) * (mean_Percent_white ),
         SEX == "Male" & age_group == "45-64" & race_group == "White" ~ Total_Population * (mean_Percent_male ) * (mean_Percent_age_45_64 ) * (mean_Percent_white ),
         SEX == "Male" & age_group == "65+" & race_group == "White" ~ Total_Population * (mean_Percent_male ) * (mean_Percent_age_65_over ) * (mean_Percent_white ),
         SEX == "Female" & age_group == "<18" & race_group == "White" ~ Total_Population * (mean_Percent_female ) * (mean_Percent_age_0_17 ) * (mean_Percent_white ),
         SEX == "Female" & age_group == "18-44" & race_group == "White" ~ Total_Population * (mean_Percent_female ) * (mean_Percent_age_18_44 ) * (mean_Percent_white ),
         SEX == "Female" & age_group == "45-64" & race_group == "White" ~ Total_Population * (mean_Percent_female ) * (mean_Percent_age_45_64 ) * (mean_Percent_white ),
         SEX == "Female" & age_group == "65+" & race_group == "White" ~ Total_Population * (mean_Percent_female ) * (mean_Percent_age_65_over ) * (mean_Percent_white ),
         SEX == "Male" & age_group == "<18" & race_group == "Non-white" ~ Total_Population * (mean_Percent_male ) * (mean_Percent_age_0_17 ) * (mean_Percent_Non_white ),
         SEX == "Male" & age_group == "18-44" & race_group == "Non-white" ~ Total_Population * (mean_Percent_male ) * (mean_Percent_age_18_44 ) * (mean_Percent_Non_white ),
         SEX == "Male" & age_group == "45-64" & race_group == "Non-white" ~ Total_Population * (mean_Percent_male ) * (mean_Percent_age_45_64 ) * (mean_Percent_Non_white ),
         SEX == "Male" & age_group == "65+" & race_group == "Non-white" ~ Total_Population * (mean_Percent_male ) * (mean_Percent_age_65_over ) * (mean_Percent_Non_white ),
         SEX == "Female" & age_group == "<18" & race_group == "Non-white" ~ Total_Population * (mean_Percent_female ) * (mean_Percent_age_0_17 ) * (mean_Percent_Non_white ),
         SEX == "Female" & age_group == "18-44" & race_group == "Non-white" ~ Total_Population * (mean_Percent_female ) * (mean_Percent_age_18_44 ) * (mean_Percent_Non_white ),
         SEX == "Female" & age_group == "45-64" & race_group == "Non-white" ~ Total_Population * (mean_Percent_female ) * (mean_Percent_age_45_64 ) * (mean_Percent_Non_white ),
         SEX == "Female" & age_group == "65+" & race_group == "Non-white" ~ Total_Population * (mean_Percent_female ) * (mean_Percent_age_65_over ) * (mean_Percent_Non_white ),
         TRUE ~ NA_real_  # Assign NA for other cases
      )
   )



test_grouped_data_filled$new_subpopulation_size<-round(test_grouped_data_filled$new_subpopulation_size, 0)

# Replace NA values in occurrences with 0
test_grouped_data_filled <- test_grouped_data_filled %>%
  mutate(occurrences = replace_na(occurrences, 0))








#RFA data-----------
# Data set: filtered_data_rfa_Table1
RFA <- readRDS("/Users/tanvirahammed/Library/CloudStorage/Box-Box/BoxSecure-DPHS-Opiod/COVID registry/Tanvir/Data/filtered_data_rfa_Table1.rds")

# Create age_group variable
RFA <- RFA %>%
   mutate(age_group = case_when(
      AGRP %in% c("AGE 00-04", "AGE 05-11", "AGE 12-15", "AGE 16-17") ~ "<18",
      AGRP %in% c("AGE 18-24", "AGE 25-29", "AGE 30-34", "AGE 35-39", "AGE 40-44") ~ "18-44",
      AGRP %in% c("AGE 45-49", "AGE 50-54", "AGE 55-59", "AGE 60-64") ~ "45-64",
      AGRP %in% c("AGE 65-69", "AGE 70-74", "AGE 75-79", "AGE 80-84", "AGE 85+") ~ "65+",
      TRUE ~ NA_character_  # For any unexpected values
   ))




# Create race_group variable
RFA <- RFA %>%
  filter(!is.na(RACE)) %>%
  mutate(race_group = case_when(
    RACE == "White" ~ "White",
    RACE != "White" ~ "Non-white"
  ))
summary(as.factor(RFA$race_group))




# Convert 'Year_Month' to Date format
RFA$Date <- as.Date(paste0(RFA$Year_Month, "-01"), format = "%Y-%m-%d")

##Table 2----------------
RFA <- RFA %>%
   filter(Date >= as.Date("2023-01-01") & Date <= as.Date("2023-12-31"))
(cross_tab <- table(RFA$SEX, RFA$`Data Source`))
(percentage_tab <- round(prop.table(cross_tab, margin = 2) * 100, 2))

(cross_tab <- table(RFA$age_group, RFA$`Data Source`))
(percentage_tab <- round(prop.table(cross_tab, margin = 2) * 100, 2))

(cross_tab <- table(RFA$race_group, RFA$`Data Source`))
(percentage_tab <- round(prop.table(cross_tab, margin = 2) * 100, 2))



# Filter data for training (Dec 1, 2020, through June 30, 2021)
RFA_training_data <- RFA %>%
  filter(Date >= as.Date("2023-01-01") & Date <= as.Date("2023-06-30"))

# Filter data for the test set (July 1 and Dec 31, 2021)
RFA_test_data <- RFA %>%
  filter(Date >= as.Date("2023-07-01") & Date <= as.Date("2023-12-31"))


# RFA_training_data-----------
RFA_training_grouped_data <- RFA_training_data %>%
  group_by(county, SEX, age_group, race_group) %>%
  summarize(
    occurrences = n(),
  
  )



# RFA_test_data-----------
RFA_test_grouped_data <- RFA_test_data %>%
  group_by(county, SEX, age_group, race_group) %>%
  summarize(
    occurrences = n(),
    
  )



# Final test and training data (Merge the Prisma & RFA datasets based on the "zip code" column) ---------

## Final training data----------
Final_training_data <- left_join(training_grouped_data_filled, RFA_training_grouped_data, by = c("county", "SEX", "age_group", "race_group"))

Final_training_data <- Final_training_data %>%
  rename(hospitalisation_RFA = occurrences.y, hospitalisation_prisma = occurrences.x) %>%
  dplyr::select(county, SEX, age_group, race_group, hospitalisation_prisma, new_subpopulation_size, hospitalisation_RFA)

# Create new variables log_subpopulation_size & diff
Final_training_data$log_subpopulation_size <- log(Final_training_data$new_subpopulation_size)
Final_training_data$new_subpopulation_size <- NULL

Final_training_data$diff <- Final_training_data$hospitalisation_RFA - Final_training_data$hospitalisation_prisma



## Final test data----------
Final_test_data <- left_join(test_grouped_data_filled, RFA_test_grouped_data, by = c("county", "SEX", "age_group", "race_group"))

Final_test_data <- Final_test_data %>%
  rename(hospitalisation_RFA = occurrences.y, hospitalisation_prisma = occurrences.x) %>%
  dplyr::select(county, SEX, age_group, race_group, hospitalisation_prisma, new_subpopulation_size, hospitalisation_RFA)

# Create new variables log_subpopulation_size & diff
Final_test_data$log_subpopulation_size <- log(Final_test_data$new_subpopulation_size)
Final_test_data$new_subpopulation_size <- NULL

Final_test_data$diff <- Final_test_data$hospitalisation_RFA - Final_test_data$hospitalisation_prisma








# Map of observed hospitalization based on Prisma data ------------------------

Final_test_data1 <- Final_test_data %>%
   group_by(county) %>%
   summarize(
      hospitalisation_prisma = sum(hospitalisation_prisma),
   )
#install.packages("tigris")
library(tigris)
# Use the tigris package to get county shapefile for South Carolina
sc_counties <- tigris::counties(state = "SC", year = 2021, class = "sf")


# Create a data frame with the required variables
hosp_county_esti_impu <- data.frame(county = as.character(Final_test_data1$county),
                                 Observed = Final_test_data1$hospitalisation_prisma)


#Transform county codes to ZCTAs first and merge your data with sc_counties by the ZCTA. 
sc_counties = left_join(sc_counties, hosp_county_esti_impu 
                       %>% dplyr::select(Observed, NAMELSAD = county), by = 'NAMELSAD')


###observed------------
sc_counties = mutate(sc_counties, 
                    Observed_grouped = case_when(Observed < 50 ~ '1',
                                                 Observed >= 50 & Observed <100 ~ '2',
                                                 Observed >= 100 & Observed <150 ~ '3',
                                                 Observed >= 150 & Observed <200 ~ '4'))

# Convert 'Observed_grouped' to a factor with levels ordered from largest to smallest
sc_counties$Observed_grouped <- factor(sc_counties$Observed_grouped, 
                                      levels = c( "4", "3", "2", "1"),
                                      labels = c( "200 - 150", "100 - 150", "50 - 100", "0 - 50"))

# Plot using ggplot2
ggplot(sc_counties) + 
   geom_sf(aes(fill = Observed_grouped), color = 'gray20') + 
   scale_fill_manual(values = c( '#810f7c', '#8c96c6',  '#bfd3e6', '#d0d1e6'), #  '#2b8cbe', '#7bccc4', '#bae4bc', '#f0f9e8'
                     name = "Hospitalizations", 
                     na.translate = TRUE, 
                     na.value = 'white', 
                     labels = c("200 - 150", "100 - 150", "50 - 100", "0 - 50", "No Data")) + 
   ggtitle("Total observed hospitalization counts in 8 counties based on Prisma Health data") + 
   theme_void() + 
   theme(
      plot.title = element_text(hjust = 0.5, size = 30),
      legend.justification = c(0.1, 0.1), 
      legend.position = c(0.1, 0.1),
      legend.title = element_text(size = 28),
      legend.key.size = unit(1, 'cm'),
      legend.text = element_text(size = 28)
   )









#Model-----------

# Fit a negative binomial mixed-effects model
model1 <- glmer.nb(hospitalisation_RFA ~ hospitalisation_prisma + log_subpopulation_size + (1|county), 
                   data = Final_training_data,nAGQ=0)

model2 <- glmer.nb(hospitalisation_RFA ~ SEX + age_group + race_group + hospitalisation_prisma + log_subpopulation_size + (1|county), 
                   data = Final_training_data,nAGQ=0)
                   #control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

model3 <- glmer.nb(hospitalisation_RFA ~ SEX + age_group + race_group + hospitalisation_prisma 
                   +SEX*hospitalisation_prisma  + age_group*hospitalisation_prisma + race_group*hospitalisation_prisma
                   + log_subpopulation_size + (1|county), data = Final_training_data,nAGQ=0)

# Without random intercept
library(MASS)
model4 <- glm.nb(hospitalisation_RFA ~ SEX + age_group + race_group + hospitalisation_prisma + log_subpopulation_size, 
                 data = Final_training_data)

#Difference model
model5 <- glmer.nb(diff ~ SEX + age_group + race_group + log_subpopulation_size + (1|county), 
                   data = Final_training_data,nAGQ=0)
                   #control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

summary(model5)





#RF------------
library(caret)
# Define the control
trControl <- trainControl(method = "cv", number = 10, search = "grid")
set.seed(1234)
rf_default <- train(hospitalisation_RFA ~ hospitalisation_prisma + log_subpopulation_size, 
                    data = Final_training_data, 
                    method = "rf", trControl = trControl)

# Print the results
print(rf_default)


#finding the best mtry value
set.seed(1234)
tuneGrid <- expand.grid(.mtry = c(2: 4))
rf_mtry <- train(hospitalisation_RFA ~ hospitalisation_prisma + log_subpopulation_size, 
                 data = Final_training_data, 
                 method = "rf", tuneGrid = tuneGrid, trControl = trControl)
print(rf_mtry)
#storing the best mtry value
best_mtry <- rf_mtry$bestTune$mtry
tuneGrid <- expand.grid(.mtry = best_mtry)


#final model 1------------
set.seed(1234)
fit_rf <- train(hospitalisation_RFA ~ hospitalisation_prisma + log_subpopulation_size,
                data = Final_training_data, 
                method = "rf", tuneGrid = tuneGrid, trControl = trControl)
                #, importance = TRUE, nodesize = 14, maxnodes = 5, ntree = 250 )



####### model2-------------
trControl <- trainControl(method = "cv", number = 5, search = "grid")
set.seed(1234)
rf_default <- train(hospitalisation_RFA ~ SEX + age_group + race_group + hospitalisation_prisma
                    + log_subpopulation_size, data = Final_training_data,
                    method = "rf", trControl = trControl, ntree = 300)


# Print the results
print(rf_default)

set.seed(1234)
#finding the best mtry value
tuneGrid <- expand.grid(.mtry = c(1:15))
rf_mtry <- train(hospitalisation_RFA ~ SEX + age_group + race_group + hospitalisation_prisma
                 + log_subpopulation_size, data = Final_training_data,
                 method = "rf", tuneGrid = tuneGrid, trControl = trControl, ntree = 300)
print(rf_mtry)
#storing the best mtry value
best_mtry <- rf_mtry$bestTune$mtry
tuneGrid <- expand.grid(.mtry = best_mtry)





#final model2-----------
set.seed(1234)
fit_rf1 <- train(hospitalisation_RFA ~ SEX + age_group + race_group + hospitalisation_prisma
                + log_subpopulation_size, data = Final_training_data, method = "rf",
                tuneGrid = tuneGrid, trControl = trControl, ntree = 300)







#accuracy
# Predict on the count scale
predictions_counts1 <- predict(model1, newdata = Final_test_data, type = "response")
predictions_counts2 <- predict(model2, newdata = Final_test_data, type = "response")
predictions_counts3 <- predict(model3, newdata = Final_test_data, type = "response")
predictions_counts4 <- predict(model4, newdata = Final_test_data, type = "response")
predictions_counts5 <- predict(model5, newdata = Final_test_data, type = "response")
predictions_counts6 <-predict(fit_rf, Final_test_data)
predictions_counts7 <-predict(fit_rf1, Final_test_data)










# Attach the predictions to Final_test_data
Final_test_data$predicted_hospitalisation1 <- predictions_counts1
Final_test_data$predicted_hospitalisation2 <- predictions_counts2
Final_test_data$predicted_hospitalisation3 <- predictions_counts3
Final_test_data$predicted_hospitalisation4 <- predictions_counts4
Final_test_data$predicted_hospitalisation5 <- predictions_counts5
Final_test_data$predicted_hospitalisation6 <- predictions_counts6
Final_test_data$predicted_hospitalisation7 <- predictions_counts7

###Final_test_data---------
#write.xlsx(Final_test_data, "/Users/tanvirahammed/Library/CloudStorage/Box-Box/BoxSecure-DPHS-Opiod/COVID registry/Tanvir/Results/Final_test_data_county_June.xlsx", rowNames = FALSE)



# Group by county and calculate the total hospitalizations for observed and predicted values
total_hospitalizations <- Final_test_data %>%
  group_by(county) %>%
  summarise(
    total_observed_hospitalizations = sum(hospitalisation_RFA, na.rm = TRUE),
    total_predicted_hospitalizations1 = sum(predicted_hospitalisation1, na.rm = TRUE),
    total_predicted_hospitalizations2 = sum(predicted_hospitalisation2, na.rm = TRUE),
    total_predicted_hospitalizations3 = sum(predicted_hospitalisation3, na.rm = TRUE),
    total_predicted_hospitalizations4 = sum(predicted_hospitalisation4, na.rm = TRUE),
    total_predicted_hospitalizations5 = sum(predicted_hospitalisation5, na.rm = TRUE),
    total_predicted_hospitalizations6 = sum(predicted_hospitalisation6, na.rm = TRUE),
    total_predicted_hospitalizations7 = sum(predicted_hospitalisation7, na.rm = TRUE)
  )


#write.xlsx(total_hospitalizations, "/Users/tanvirahammed/Library/CloudStorage/Box-Box/BoxPHI-PHMR Projects/Tanvir/Data/Final Data/total_hospitalizations_county_June.xlsx", rowNames = FALSE)


# Ai
predicted_variables <- c("total_predicted_hospitalizations1", "total_predicted_hospitalizations2", 
                         "total_predicted_hospitalizations3", "total_predicted_hospitalizations4", 
                         "total_predicted_hospitalizations5", "total_predicted_hospitalizations6",
                         "total_predicted_hospitalizations7")

for (i in seq_along(predicted_variables)) {
  col_name <- paste0("A_i_", i)
  total_hospitalizations[[col_name]] <- with(total_hospitalizations, pmin(total_observed_hospitalizations, get(predicted_variables[i])) /
                                               pmax(total_observed_hospitalizations, get(predicted_variables[i])))
}



A_i_columns <- paste0("A_i_", 1:7)
# Subset the data frame to include only the A_i columns
A_i_data <- total_hospitalizations[A_i_columns]
# Use the summary function to get the five-number summary for each A_i column
(summary_A_i <- apply(A_i_data, 2, summary))






#Figure 3----------------
library(ggplot2)
library(tidyr)
library(dplyr)

# Create a data frame with the required variables
data <- data.frame(county = total_hospitalizations$county,
                   Observed = total_hospitalizations$total_observed_hospitalizations,
                   Estimated = total_hospitalizations$total_predicted_hospitalizations7)

data = left_join(data, summary_data 
                        %>% dplyr::select(county, Total_Population), by = 'county')


# # Calculate the difference between Observed and Predicted
# data <- data %>%
#   mutate(Difference = abs(Observed - Estimated))
# 
# # Reorder the ZIP codes based on the Difference
# data <- data %>%
#   arrange(desc(Difference)) %>%
#   mutate(ZIP = factor(ZIP, levels = ZIP))

# Sort the data based on Total_Population in ascending order
data <- data %>%
   arrange(Total_Population) %>%
   mutate(county = factor(county, levels = county[order(Total_Population)]))  # Order counties based on ascending population

# Convert data from wide to long format
data_long <- pivot_longer(data, cols = c(Observed, Estimated), names_to = "Type", values_to = "Hospitalizations")

# Reorder Type so that Observed is plotted first
data_long <- data_long %>%
   mutate(Type = factor(Type, levels = c("Observed", "Estimated")))

# Plot using ggplot for counties
p <- ggplot(data_long, aes(x = county, y = Hospitalizations, fill = Type)) +
   geom_bar(stat = "identity", position = "dodge") +
   labs(x = "Counties", y = "COVID-19 Hospitalizations",
        title = "Observed and Estimated COVID-19 Hospitalizations by Counties") +
   scale_fill_manual(values = c("blueviolet", "azure4")) +
   theme_minimal() +
   theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 15),   # Increase x-axis tick label size
         axis.text.y = element_text(size = 12),                         # Increase y-axis tick label size
         axis.title.x = element_text(size = 18, margin = margin(t = 15)), # Increase x-axis label size with a margin
         axis.title.y = element_text(size = 18, margin = margin(r = 15)), # Increase y-axis label size with a margin
         legend.text = element_text(size = 16),                         # Increase legend text size
         legend.title = element_text(size = 16),                        # Increase legend title size
         plot.title = element_text(size = 20))                          # Increase plot title size

print(p)







# 
# 
# # Attach the predictions to Final_test_data
# Final_test_data$predicted_hospitalisation1 <- predictions_counts1
# Final_test_data$predicted_hospitalisation2 <- predictions_counts2
# Final_test_data$predicted_hospitalisation3 <- predictions_counts3
# Final_test_data$predicted_hospitalisation4 <- predictions_counts4
# Final_test_data$predicted_hospitalisation5 <- predictions_counts5
# 
# 
# # Group by county and calculate the total hospitalizations for observed and predicted values
# total_hospitalizations <- Final_test_data %>%
#   group_by(county) %>%
#   summarise(
#     total_observed_hospitalizations = sum(hospitalisation_RFA, na.rm = TRUE),
#     total_predicted_hospitalizations1 = sum(predicted_hospitalisation1, na.rm = TRUE),
#     total_predicted_hospitalizations2 = sum(predicted_hospitalisation2, na.rm = TRUE),
#     total_predicted_hospitalizations3 = sum(predicted_hospitalisation3, na.rm = TRUE),
#     total_predicted_hospitalizations4 = sum(predicted_hospitalisation4, na.rm = TRUE),
#     total_predicted_hospitalizations5 = sum(predicted_hospitalisation5, na.rm = TRUE)
#   )
# 
# # View the resulting data frame
# print(total_hospitalizations)
# 
# 
# 
# 
# # Ai
# predicted_variables <- c("total_predicted_hospitalizations1", "total_predicted_hospitalizations2", 
#                          "total_predicted_hospitalizations3", "total_predicted_hospitalizations4", 
#                          "total_predicted_hospitalizations5")
# 
# for (i in seq_along(predicted_variables)) {
#   col_name <- paste0("A_i_", i)
#   total_hospitalizations[[col_name]] <- with(total_hospitalizations, pmin(total_observed_hospitalizations, get(predicted_variables[i])) /
#                                                pmax(total_observed_hospitalizations, get(predicted_variables[i])))
# }
# 
# 
# 
# A_i_columns <- paste0("A_i_", 1:5)
# # Subset the data frame to include only the A_i columns
# A_i_data <- total_hospitalizations[A_i_columns]
# # Use the summary function to get the five-number summary for each A_i column
# summary_A_i <- apply(A_i_data, 2, summary)
# # Print the result
# print(summary_A_i)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# # validation for sub population category
# # Creating sub population category variable
# Final_test_data$subpopulation_category <- case_when(
#   Final_test_data$SEX == "Male" & Final_test_data$age_group == "<20" & Final_test_data$race_group == "White" ~ 1,
#   Final_test_data$SEX == "Male" & Final_test_data$age_group == "20-44" & Final_test_data$race_group == "White" ~ 2,
#   Final_test_data$SEX == "Male" & Final_test_data$age_group == "45-64" & Final_test_data$race_group == "White" ~ 3,
#   Final_test_data$SEX == "Male" & Final_test_data$age_group == "65+" & Final_test_data$race_group == "White" ~ 4,
#   Final_test_data$SEX == "Female" & Final_test_data$age_group == "<20" & Final_test_data$race_group == "White" ~ 5,
#   Final_test_data$SEX == "Female" & Final_test_data$age_group == "20-44" & Final_test_data$race_group == "White" ~ 6,
#   Final_test_data$SEX == "Female" & Final_test_data$age_group == "45-64" & Final_test_data$race_group == "White" ~ 7,
#   Final_test_data$SEX == "Female" & Final_test_data$age_group == "65+" & Final_test_data$race_group == "White" ~ 8,
#   Final_test_data$SEX == "Male" & Final_test_data$age_group == "<20" & Final_test_data$race_group == "Non-white" ~ 9,
#   Final_test_data$SEX == "Male" & Final_test_data$age_group == "20-44" & Final_test_data$race_group == "Non-white" ~ 10,
#   Final_test_data$SEX == "Male" & Final_test_data$age_group == "45-64" & Final_test_data$race_group == "Non-white" ~ 11,
#   Final_test_data$SEX == "Male" & Final_test_data$age_group == "65+" & Final_test_data$race_group == "Non-white" ~ 12,
#   Final_test_data$SEX == "Female" & Final_test_data$age_group == "<20" & Final_test_data$race_group == "Non-white" ~ 13,
#   Final_test_data$SEX == "Female" & Final_test_data$age_group == "20-44" & Final_test_data$race_group == "Non-white" ~ 14,
#   Final_test_data$SEX == "Female" & Final_test_data$age_group == "45-64" & Final_test_data$race_group == "Non-white" ~ 15,
#   Final_test_data$SEX == "Female" & Final_test_data$age_group == "65+" & Final_test_data$race_group == "Non-white" ~ 16,
#   TRUE ~ NA_integer_  # Assign NA for other cases
# )
# 
# # Convert the new variable to factor 
# Final_test_data$subpopulation_category <- as.factor(Final_test_data$subpopulation_category)
# 
# # Adding labels to subpopulation category
# levels(Final_test_data$subpopulation_category) <- c(
#   "Male <20 White", "Male 20-44 White", "Male 45-64 White", "Male 65+ White",
#   "Female <20 White", "Female 20-44 White", "Female 45-64 White", "Female 65+ White",
#   "Male <20 Non-white", "Male 20-44 Non-white", "Male 45-64 Non-white", "Male 65+ Non-white",
#   "Female <20 Non-white", "Female 20-44 Non-white", "Female 45-64 Non-white", "Female 65+ Non-white"
# )
# 
# # Group by sub population category and calculate the total hospitalizations for observed and predicted values
# total_hospitalizations2 <- Final_test_data %>%
#   group_by(subpopulation_category) %>%
#   summarise(
#     total_observed_hospitalizations = sum(hospitalisation_RFA, na.rm = TRUE),
#     total_predicted_hospitalizations1 = sum(predicted_hospitalisation1, na.rm = TRUE),
#     total_predicted_hospitalizations2 = sum(predicted_hospitalisation2, na.rm = TRUE),
#     total_predicted_hospitalizations3 = sum(predicted_hospitalisation3, na.rm = TRUE),
#     total_predicted_hospitalizations4 = sum(predicted_hospitalisation4, na.rm = TRUE),
#     total_predicted_hospitalizations5 = sum(predicted_hospitalisation5, na.rm = TRUE),
#     total_predicted_hospitalizations6 = sum(predicted_hospitalisation6, na.rm = TRUE)
#   )
# 
# # View the resulting data frame
# print(total_hospitalizations2)
# 
# 
# # Ai
# predicted_variables <- c("total_predicted_hospitalizations1", "total_predicted_hospitalizations2", 
#                          "total_predicted_hospitalizations3", "total_predicted_hospitalizations4", 
#                          "total_predicted_hospitalizations5", "total_predicted_hospitalizations6")
# 
# for (i in seq_along(predicted_variables)) {
#   col_name <- paste0("A_i_", i)
#   total_hospitalizations2[[col_name]] <- with(total_hospitalizations2, pmin(total_observed_hospitalizations, get(predicted_variables[i])) /
#                                                 pmax(total_observed_hospitalizations, get(predicted_variables[i])))
# }
# 
# A_i_columns <- paste0("A_i_", 1:6)
# # Subset the data frame to include only the A_i columns
# A_i_data <- total_hospitalizations2[A_i_columns]
# # Use the summary function to get the five-number summary for each A_i column
# summary_A_i <- apply(A_i_data, 2, summary)
# # Print the result
# print(summary_A_i)
# 
# 
# 
# 
# write.xlsx(total_hospitalizations, "/Users/tanvirahammed/Library/CloudStorage/Box-Box/BoxSecure-DPHS-Opiod/COVID registry/Tanvir/Results/total_hospitalizations_new.xlsx", rowNames = FALSE)
# write.xlsx(total_hospitalizations2, "/Users/tanvirahammed/Library/CloudStorage/Box-Box/BoxSecure-DPHS-Opiod/COVID registry/Tanvir/Results/total_hospitalizations2_new.xlsx", rowNames = FALSE)
# 
# 















# Create a line plot for each county--------------
#preparing data
RFA_selected <- RFA %>% dplyr::select(county, Year_Month)
RFA_selected <- RFA_selected %>%
  group_by(county, Year_Month) %>%
  summarise(Count = n())

#View(RFA_selected)

prisma_selected <- prisma %>% dplyr::select(county, Year_Month)
prisma_selected <- prisma_selected %>%
  group_by(county, Year_Month) %>%
  summarise(Count = n())

#View(prisma_selected)

RFA_selected <- RFA_selected %>% mutate(`Data Source` = 1)  # Source = 1 for rfa
prisma_selected <- prisma_selected %>% mutate(`Data Source` = 0)  # Source = 0 for prisma

# Combine the datasets by appending the rows
combined_data1 <- bind_rows(RFA_selected, prisma_selected)
View(combined_data1)
# Filter data for (Dec 1, 2020, through Dec 31, 2021)
combined_data1$Date <- as.Date(paste0(combined_data1$Year_Month, "-01"), format = "%Y-%m-%d")
combined_data1 <- combined_data1 %>%
  filter(Date >= as.Date("2020-03-01") & Date <= as.Date("2023-12-31"))

# Convert the "source" column to a factor with specific levels
combined_data1$`Data Source` <- factor(
  combined_data1$`Data Source`,
  levels = c(1, 0),
  labels = c("RFA", "Prisma")
)

#Reorder the data frame to ensure the correct layout
combined_data1 <- combined_data1 %>%
  arrange(county, Date)


selected_counties <- c("Greenville County", "Richland County", "Sumter County", "Pickens County")
combined_data1 <- combined_data1 %>% filter(county %in% selected_counties)

library(lubridate)

# Convert Year_Month to Date format (assuming the format is "YYYY-MM")
combined_data1$Year_Month <- as.Date(paste0(combined_data1$Year_Month, "-01"), format = "%Y-%m-%d")

ggplot(combined_data1, aes(x = Year_Month, y = Count, group = `Data Source`, color = `Data Source`)) +
   geom_line() +
   labs(title = "Hospitalization Count by County Over Time",
        x = "Time",
        y = "No. of Hospitalized Individuals") +
   theme_minimal() +
   theme(
      axis.text.x = element_text(angle = 40, hjust = 1, size = 16),
      axis.text.y = element_text(size = 15, margin = margin(r = 5)),
      axis.title.x = element_text(size = 20, margin = margin(t = 15)),
      axis.title.y = element_text(size = 20, margin = margin(r = 20)),
      plot.title = element_text(size = 25, hjust = 0.5, margin = margin(b = 15)),
      legend.text = element_text(size = 15),
      legend.title = element_text(size = 17),
      legend.position = c(1, 0.55),
      legend.justification = c(1, 1),
      strip.text = element_text(size = 18)
   ) +
   scale_y_continuous(breaks = pretty_breaks(n = 5)) +
   scale_x_date(date_labels = "%Y-%m", date_breaks = "3 months") +
   facet_wrap(~ county, scales = "fixed", ncol = 2)  # Use 'scales = "fixed"' to keep y-axis scales the same


