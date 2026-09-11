library(dplyr)
library(tidyr)
library(stringr)
library(readxl)
library(openxlsx)
library(lme4)
library(readr)
library(ggplot2)





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

#write.xlsx(summary_data, "/Users/tanvirahammed/Library/CloudStorage/Box-Box/BoxSecure-DPHS-Opiod/COVID registry/Tanvir/Results/SC_demographic_summary_data.xlsx", rowNames = FALSE)


#Prisma Data---------
# dataset:merged_dataset_Table1
prisma <- readRDS("/Users/tanvirahammed/Library/CloudStorage/Box-Box/BoxSecure-DPHS-Opiod/COVID registry/Tanvir/Data/merged_dataset_Table1_ZIP.rds")

prisma <- prisma %>%
   filter(SEX != "Unknown")

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


# Create age_group variable
prisma <- prisma %>%
   mutate(age_group = case_when(
      AGE < 18 ~ "<18",
      AGE >= 18 & AGE <= 44 ~ "18-44",
      AGE >= 45 & AGE <= 64 ~ "45-64",
      AGE >= 65 ~ "65+"
   ))

# Create race_group variable
prisma <- prisma %>%
   filter(!is.na(RACE)) %>%
   mutate(race_group = case_when(
      RACE == "White" ~ "White",
      RACE != "White" ~ "Non-white"
   ))


# Filter data for training 
prisma <- prisma %>% 
   filter(EncounterDate >= as.Date("2023-01-01") & EncounterDate <= as.Date("2023-12-31"))


(cross_tab <- table(prisma$SEX, prisma$`Data Source`))
(percentage_tab <- round(prop.table(cross_tab, margin = 2) * 100, 2))

(cross_tab <- table(prisma$age_group, prisma$`Data Source`))
(percentage_tab <- round(prop.table(cross_tab, margin = 2) * 100, 2))

(cross_tab <- table(prisma$race_group, prisma$`Data Source`))
(percentage_tab <- round(prop.table(cross_tab, margin = 2) * 100, 2))


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



# Filter data for training 
training_data <- prisma %>% 
   filter(EncounterDate >= as.Date("2023-01-01") & EncounterDate <= as.Date("2023-06-30"))

# Filter data for the test set (July 1 and Dec 31, 2023)
test_data <- prisma %>%
   filter(EncounterDate >= as.Date("2023-07-01") & EncounterDate<= as.Date("2023-12-31"))



# training_data-----------

training_grouped_data <- training_data %>%
   group_by(ZIP, SEX, age_group, race_group) %>%
   summarize(
      occurrences = n(),
   )


training_grouped_data$subpopulation_size <- prisma$`subpopulation_size`[
   match(paste(training_grouped_data$ZIP, training_grouped_data$SEX, training_grouped_data$age_group, training_grouped_data$race_group), 
         paste(prisma$ZIP, prisma$SEX, prisma$age_group, prisma$race_group))
]




# Define all possible combinations
all_combinations <- expand.grid(
   ZIP = unique(training_grouped_data$ZIP),
   SEX = unique(training_grouped_data$SEX),
   age_group = unique(training_grouped_data$age_group),
   race_group = unique(training_grouped_data$race_group)
)


# Left join with training_data to fill missing combinations
training_grouped_data_filled <- left_join(all_combinations, training_grouped_data, by = c("ZIP", "SEX", "age_group", "race_group"))


## Merge the datasets based on the ZIP column 
training_grouped_data_filled<- training_grouped_data_filled  %>%
   left_join(selected_dataset, by = "ZIP")


# Subpopulation size 
training_grouped_data_filled <- training_grouped_data_filled %>%
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


training_grouped_data_filled$new_subpopulation_size<-round(training_grouped_data_filled$new_subpopulation_size, 0)

# Replace NA values in occurrences with 0
training_grouped_data_filled <- training_grouped_data_filled %>%
   mutate(occurrences = replace_na(occurrences, 0))





# test_data-----------

test_grouped_data <- test_data %>%
   group_by(ZIP, SEX, age_group, race_group) %>%
   summarize(
      occurrences = n(),
      # ZIP = paste(ZIP, collapse = ", ")
   )



test_grouped_data$subpopulation_size <- prisma$`subpopulation_size`[
   match(paste(test_grouped_data$ZIP, test_grouped_data$SEX, test_grouped_data$age_group, test_grouped_data$race_group), 
         paste(prisma$ZIP, prisma$SEX, prisma$age_group, prisma$race_group))
]





# Define all possible combinations
all_combinations1 <- expand.grid(
   ZIP = unique(test_grouped_data$ZIP),
   SEX = unique(test_grouped_data$SEX),
   age_group = unique(test_grouped_data$age_group),
   race_group = unique(test_grouped_data$race_group)
)


# Left join with test_data to fill missing combinations
test_grouped_data_filled <- left_join(all_combinations1, test_grouped_data, by = c("ZIP", "SEX", "age_group", "race_group"))


## Merge the datasets based on the ZIP column 
test_grouped_data_filled<- test_grouped_data_filled  %>%
   left_join(selected_dataset, by = "ZIP")


# Subpopulation size 
test_grouped_data_filled <- test_grouped_data_filled %>%
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

test_grouped_data_filled$new_subpopulation_size<-round(test_grouped_data_filled$new_subpopulation_size, 0)

# Replace NA values in occurrences with 0
test_grouped_data_filled <- test_grouped_data_filled %>%
   mutate(occurrences = replace_na(occurrences, 0))








#RFA data-----------
# Data set: filtered_data_rfa_Table1
RFA <- readRDS("/Users/tanvirahammed/Library/CloudStorage/Box-Box/BoxSecure-DPHS-Opiod/COVID registry/Tanvir/Data/filtered_data_rfa_Table1_ZIP.rds")

#RFA <- read_csv("/Users/tanvirahammed/Library/CloudStorage/Box-Box/BoxPHI-PHMR Projects/Data/SC RFA Hospitalization Data/Years 2019-2023/Clean Data/data_covid.csv")



# Creating new age groups from AGRP
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


RFA <- RFA %>%
   filter(ADMD_new >= as.Date("2023-01-01") & ADMD_new <= as.Date("2023-12-31"))

(cross_tab <- table(RFA$SEX))
(percentage_tab <- round(prop.table(cross_tab) * 100, 2))

(cross_tab <- table(RFA$age_group))
(percentage_tab <- round(prop.table(cross_tab) * 100, 2))

(cross_tab <- table(RFA$race_group))
(percentage_tab <- round(prop.table(cross_tab) * 100, 2))



# Filter data for training (Dec 1, 2020, through June 30, 2021)
RFA_training_data <- RFA %>%
   filter(ADMD_new >= as.Date("2023-01-01") & ADMD_new <= as.Date("2023-06-30"))

# Filter data for the test set (July 1 and Dec 31, 2021)
RFA_test_data <- RFA %>%
   filter(ADMD_new >= as.Date("2023-07-01") & ADMD_new <= as.Date("2023-12-31"))






# RFA_training_data-----------
RFA_training_grouped_data <- RFA_training_data %>%
   group_by(ZIP, SEX, age_group, race_group) %>%
   summarize(
      occurrences = n(),
      # ZIP = paste(ZIP, collapse = ", ")
   )



# Define all possible combinations
all_combinations2 <- expand.grid(
   ZIP = unique(RFA_training_grouped_data$ZIP),
   SEX = unique(RFA_training_grouped_data$SEX),
   age_group = unique(RFA_training_grouped_data$age_group),
   race_group = unique(RFA_training_grouped_data$race_group)
)


# Left join with test_data to fill missing combinations
RFA_training_grouped_data <- left_join(all_combinations2, RFA_training_grouped_data, by = c("ZIP", "SEX", "age_group", "race_group"))


# Replace NA values in occurrences with 0
RFA_training_grouped_data <- RFA_training_grouped_data %>%
   mutate(occurrences = replace_na(occurrences, 0))


# RFA_test_data-----------
RFA_test_grouped_data <- RFA_test_data %>%
   group_by(ZIP, SEX, age_group, race_group) %>%
   summarize(
      occurrences = n(),
      # ZIP = paste(ZIP, collapse = ", ")
   )

# RFA_test_grouped_data <- RFA_test_grouped_data %>%
#   mutate(ZIP = str_extract(ZIP, "^[^,]+"))


# Define all possible combinations
all_combinations3 <- expand.grid(
   ZIP = unique(RFA_test_grouped_data$ZIP),
   SEX = unique(RFA_test_grouped_data$SEX),
   age_group = unique(RFA_test_grouped_data$age_group),
   race_group = unique(RFA_test_grouped_data$race_group)
)


# Left join with test_data to fill missing combinations
RFA_test_grouped_data <- left_join(all_combinations3, RFA_test_grouped_data, by = c("ZIP", "SEX", "age_group", "race_group"))


# Replace NA values in occurrences with 0
RFA_test_grouped_data <- RFA_test_grouped_data %>%
   mutate(occurrences = replace_na(occurrences, 0))


# Final test and training data (Merge the Prisma & RFA datasets based on the "zip code" column) ---------

## Final training data----------
Final_training_data <- left_join(training_grouped_data_filled, RFA_training_grouped_data, by = c("ZIP", "SEX", "age_group", "race_group"))

Final_training_data <- Final_training_data %>%
   rename(hospitalisation_RFA = occurrences.y, hospitalisation_prisma = occurrences.x) %>%
   dplyr::select(ZIP, SEX, age_group, race_group, hospitalisation_prisma, new_subpopulation_size, hospitalisation_RFA)












# Create new variables log_subpopulation_size & diff
Final_training_data$log_subpopulation_size <- log(Final_training_data$new_subpopulation_size)
Final_training_data$new_subpopulation_size <- NULL

Final_training_data$diff <- Final_training_data$hospitalisation_RFA - Final_training_data$hospitalisation_prisma
Final_training_data <- Final_training_data[Final_training_data$diff >= 0, ]

# Removing rows where SEX has missing values
Final_training_data <- Final_training_data %>%
   filter(!is.na(SEX))





## Final test data----------
Final_test_data <- left_join(test_grouped_data_filled, RFA_test_grouped_data, by = c("ZIP", "SEX", "age_group", "race_group"))

Final_test_data <- Final_test_data %>%
   rename(hospitalisation_RFA = occurrences.y, hospitalisation_prisma = occurrences.x) %>%
   dplyr::select(ZIP, SEX, age_group, race_group, hospitalisation_prisma, new_subpopulation_size, hospitalisation_RFA)

# Create new variables log_subpopulation_size & diff
Final_test_data$log_subpopulation_size <- log(Final_test_data$new_subpopulation_size)
Final_test_data$new_subpopulation_size <- NULL

Final_test_data$diff <- Final_test_data$hospitalisation_RFA - Final_test_data$hospitalisation_prisma
Final_test_data <- Final_test_data[Final_test_data$diff >= 0, ]

# Removing rows where SEX has missing values
Final_test_data <- Final_test_data %>%
   filter(!is.na(SEX))

# Remove rows where ZIP is 29340
Final_test_data <- Final_test_data[Final_test_data$ZIP != 29340, ]



# Map of observed hospitalization based on Prisma data ------------------------

Final_test_data1 <- Final_test_data %>%
   group_by(ZIP) %>%
   summarize(
      hospitalisation_prisma = sum(hospitalisation_prisma),
   )

#download the shapefile for zip codes of a state using tigris package. 
sc_zcta_sf = tigris::zctas(state = "SC", class = "sf", year = '2010')

#This this shapefile has a column for ZCTAs. 
# Create a data frame with the required variables
hosp_ZIP_esti_impu <- data.frame(ZIP = as.character(Final_test_data1$ZIP),
                                 Observed = Final_test_data1$hospitalisation_prisma)


#Transform zip codes to ZCTAs first and merge your data with sc_zcta_sf by the ZCTA. 
sc_zcta_sf = left_join(sc_zcta_sf, hosp_ZIP_esti_impu 
                       %>% dplyr::select(Observed, ZCTA5CE10 = ZIP), by = 'ZCTA5CE10')


###observed------------
sc_zcta_sf = mutate(sc_zcta_sf, 
                    Observed_grouped = case_when(Observed < 100 ~ '1',
                                                 Observed >= 100 & Observed <200 ~ '2',
                                                 Observed >= 200 & Observed <350 ~ '3',
                                                 Observed >= 350 & Observed <500 ~ '4',
                                                 Observed >= 500 & Observed <650 ~ '5'))

# Convert 'Observed_grouped' to a factor with levels ordered from largest to smallest
sc_zcta_sf$Observed_grouped <- factor(sc_zcta_sf$Observed_grouped, 
                                      levels = c("5", "4", "3", "2", "1"),
                                      labels = c("500 - 650", "350 - 500", "200 - 350", "100 - 200", "0 - 100"))

# Plot using ggplot2
ggplot(sc_zcta_sf) + 
   geom_sf(aes(fill = Observed_grouped), color = 'gray20') + 
   scale_fill_manual(values = c('#4d004b', '#810f7c', '#8c96c6',  '#bfd3e6', '#d0d1e6'),# '#810f7c', '#8c96c6', '#9ebcda', '#bfd3e6 
                     name = "Hospitalizations", 
                     na.translate = TRUE, 
                     na.value = 'white', 
                     labels = c("500 - 650", "350 - 500", "200 - 350", "100 - 200", "0 - 100", "No Data")) + 
   ggtitle("Total observed hospitalization counts in 91 ZIP codes based on Prisma Health data") + 
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
model1 <- glmer.nb(hospitalisation_RFA ~ hospitalisation_prisma + log_subpopulation_size + (1|ZIP), 
                   data = Final_training_data,nAGQ=0
                   #,control=glmerControl(optimizer="optimx.nlminb",optCtrl=list(maxfun=2e6))
)



model2 <- glmer.nb(hospitalisation_RFA ~ SEX + age_group + race_group + hospitalisation_prisma + log_subpopulation_size + (1|ZIP), 
                   data = Final_training_data,nAGQ=0)
#control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))


model3 <- glmer.nb(hospitalisation_RFA ~ SEX + age_group + race_group + hospitalisation_prisma 
                   +SEX*hospitalisation_prisma  + age_group*hospitalisation_prisma + race_group*hospitalisation_prisma
                   + log_subpopulation_size + (1|ZIP), data = Final_training_data,nAGQ=0)

# Without random intercept
library(MASS)
model4 <- glm.nb(hospitalisation_RFA ~ SEX + age_group + race_group + hospitalisation_prisma + log_subpopulation_size, 
                 data = Final_training_data)

#Difference model

model5 <- glmer.nb(diff ~ SEX + age_group + race_group + log_subpopulation_size + (1|ZIP), 
                   data = Final_training_data,nAGQ=0)
#control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

summary(model5)


#RF (model 1)------------
library(caret)

# Step 1: Set up parallel processing
cl <- makeCluster(detectCores() - 1) # Use all but one core
registerDoParallel(cl)

# Define the control
set.seed(1234)
trControl <- trainControl(method = "cv", number = 10, search = "grid")
# When performing cross-validation, we tend to go with the common 10 folds (k=10). 
# A higher k (number of folds) means that each model is trained on a larger training set and tested on a smaller test fold. In theory, this should lead to a lower prediction error as the models see more of the available data.
#A lower k means that the model is trained on a smaller training set and tested on a larger test fold. Here, the potential for the data distribution in the test fold to differ from the training set is bigger, and we should thus expect a higher prediction error on average.
set.seed(1234)
rf_default <- train(hospitalisation_RFA ~ SEX + age_group + race_group + hospitalisation_prisma + log_subpopulation_size, 
                    data = Final_training_data, 
                    method = "rf", trControl = trControl, ntree = 300)

# Print the results
print(rf_default)


#finding the best mtry value
tuneGrid <- expand.grid(.mtry = c(1: 9))

rf_mtry <- train(hospitalisation_RFA ~ SEX + age_group + race_group + hospitalisation_prisma + log_subpopulation_size, 
                 data = Final_training_data, method = "rf", tuneGrid = tuneGrid, trControl = trControl, 
                 ntree = 300)
print(rf_mtry)
# mtry is how many variables will be included in the first split.
# mtry depends on the number of columns and the model mode. The default in randomForest::randomForest() is floor(sqrt(ncol(x))) for classification and floor(ncol(x)/3) for regression.

#storing the best mtry value
best_mtry <- rf_mtry$bestTune$mtry
tuneGrid <- expand.grid(.mtry = best_mtry)


fit_rf <- train(hospitalisation_RFA ~ SEX + age_group + race_group + hospitalisation_prisma + log_subpopulation_size, 
                data = Final_training_data, method = "rf",
                tuneGrid = tuneGrid, 
                trControl = trControl, ntree = 300)







#RF (model 3)------------
set.seed(1234)
trControl <- trainControl(method = "cv", number = 10, search = "grid")
rf_default <- train(hospitalisation_RFA ~ hospitalisation_prisma 
                    + log_subpopulation_size, 
                    data = Final_training_data, 
                    method = "rf", trControl = trControl, ntree = 300)


# Print the results
print(rf_default)


#finding the best mtry value
set.seed(1234)
tuneGrid <- expand.grid(.mtry = c(1: 3))

set.seed(1234)
rf_mtry <- train(hospitalisation_RFA ~ hospitalisation_prisma 
                 + log_subpopulation_size, 
                 data = Final_training_data, 
                 method = "rf", tuneGrid = tuneGrid, trControl = trControl, ntree = 300)




print(rf_mtry)
# mtry is how many variables will be included in the first split.
# mtry depends on the number of columns and the model mode. The default in randomForest::randomForest() is floor(sqrt(ncol(x))) for classification and floor(ncol(x)/3) for regression.

#storing the best mtry value
best_mtry <- rf_mtry$bestTune$mtry
tuneGrid <- expand.grid(.mtry = best_mtry)




set.seed(1234)
fit_rf2 <- train(hospitalisation_RFA ~ hospitalisation_prisma 
                 + log_subpopulation_size, 
                 data = Final_training_data, 
                 method = "rf", tuneGrid = tuneGrid, trControl = trControl, ntree = 300)






# Predict on the count scale--------------
predictions_counts1 <- predict(model1, newdata = Final_test_data, type = "response")
predictions_counts2 <- predict(model2, newdata = Final_test_data, type = "response")
predictions_counts3 <- predict(model3, newdata = Final_test_data, type = "response")
predictions_counts4 <- predict(model4, newdata = Final_test_data, type = "response")
predictions_counts5 <- predict(model5, newdata = Final_test_data, type = "response")
predictions_counts6 <-predict(fit_rf2, Final_test_data)
predictions_counts7 <-predict(fit_rf, Final_test_data)


# Attach the predictions to Final_test_data
Final_test_data$predicted_hospitalisation1 <- predictions_counts1
Final_test_data$predicted_hospitalisation2 <- predictions_counts2
Final_test_data$predicted_hospitalisation3 <- predictions_counts3
Final_test_data$predicted_hospitalisation4 <- predictions_counts4
Final_test_data$predicted_hospitalisation5 <- predictions_counts5
Final_test_data$predicted_hospitalisation6 <- predictions_counts6
Final_test_data$predicted_hospitalisation7 <- predictions_counts7


##Final_test_data---------
#write.xlsx(Final_test_data, "/Users/tanvirahammed/Library/CloudStorage/Box-Box/BoxSecure-DPHS-Opiod/COVID registry/Tanvir/Results/Final_test_data_ZIP_June.xlsx", rowNames = FALSE)


# Group by ZIP and calculate the total hospitalizations for observed and predicted values
total_hospitalizations <- Final_test_data %>%
   group_by(ZIP) %>%
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


#write.xlsx(total_hospitalizations, "/Users/tanvirahammed/Library/CloudStorage/Box-Box/BoxPHI-PHMR Projects/Tanvir/Data/Final Data/total_hospitalizations_ZIP_June.xlsx", rowNames = FALSE)






predicted_variables <- c("total_predicted_hospitalizations1", "total_predicted_hospitalizations2", 
                         "total_predicted_hospitalizations3", "total_predicted_hospitalizations4", 
                         "total_predicted_hospitalizations5", "total_predicted_hospitalizations6",
                         "total_predicted_hospitalizations7"
)


# Ai------------
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
# MAPE_columns <- paste0("MAPE_", 1:7)
# MAPE_data <- total_hospitalizations[MAPE_columns]
# 
# # Compute the mean MAPE for each prediction column
# (mean_MAPE <- apply(MAPE_data, 2, mean, na.rm = TRUE))


stopCluster(cl)
registerDoSEQ() # Return to sequential processing



#Figure 2------------------
library(ggplot2)
library(tidyr)
library(dplyr)

# Create a data frame with the required variables
data <- data.frame(ZIP = total_hospitalizations$ZIP,
                   Observed = total_hospitalizations$total_observed_hospitalizations,
                   Estimated = total_hospitalizations$total_predicted_hospitalizations7)



#Transform zip codes to ZCTAs first and merge your data with sc_zcta_sf by the ZCTA. 
data <- left_join(data, selected_dataset 
                       %>% dplyr::select(ZIP, Population), by = 'ZIP')





# Calculate the difference between Observed and Predicted
data <- data %>%
   mutate(Difference = abs(Observed - Estimated))

# Reorder the ZIP codes based on the Difference
data <- data %>%
   arrange(desc(Difference)) %>%
   mutate(ZIP = factor(ZIP, levels = ZIP))

# Assuming your data is stored in a data frame called 'data'
# Sort data based on Population
data_sorted <- data %>%
   arrange(Population)

# Convert data from wide to long format
data_long <- pivot_longer(data_sorted, cols = c(Observed, Estimated), 
                          names_to = "Type", values_to = "Hospitalizations")

# Reorder Type so that Observed is plotted first
data_long <- data_long %>% 
   mutate(Type = factor(Type, levels = c("Observed", "Estimated")))

# Plot using ggplot
p <- ggplot(data_long, aes(x = factor(ZIP, levels = data_sorted$ZIP), 
                           y = Hospitalizations, fill = Type)) +
   geom_bar(stat = "identity", position = "dodge") +
   labs(x = "ZIP Codes", y = "COVID-19 Hospitalizations",
        title = "Observed and Estimated COVID-19 Hospitalizations by ZIP Codes") +
   scale_fill_manual(values = c("blue", "red")) +
   theme_minimal() +
   theme(axis.text.x = element_text(angle = 70, hjust = 1, size = 10),   # Increase x-axis tick label size
         axis.text.y = element_text(size = 12),                         # Increase y-axis tick label size
         axis.title.x = element_text(size = 22),                        # Increase x-axis label size
         axis.title.y = element_text(size = 22),                        # Increase y-axis label size
         legend.text = element_text(size = 16),                         # Increase legend text size
         legend.title = element_text(size = 16),                        # Increase legend title size
         plot.title = element_text(size = 22))                          # Increase plot title size

# Print the plot
print(p)

# # Add extra text annotations
# p + annotate("text", x = 71, y = 3450, label = "For training: Dec 1, 2020 - Feb 28, 2021", size = 6.5, color = "black") +
#   annotate("text", x = 71, y = 3250, label = "For testing: Aug 1, 2021 - Nov 30, 2021", size = 6.5, color = "black")
# 
# 
































# Plotting observed hospitalizations
plot(total_hospitalizations$ZIP, total_hospitalizations$total_observed_hospitalizations,
     type = "l", col = "blue", lwd = 1,
     xlab = "ZIP Code", ylab = "COVID-19 Hospitalisations",
     main = "Comparison of Observed and Predicted COVID-19 Hospitalisations by ZIP Code")

# Adding lines for predicted hospitalizations
lines(total_hospitalizations$ZIP, total_hospitalizations$total_predicted_hospitalizations7, col = "red")

# Adding legend
legend("topright", legend = c("Observed", "Predicted"),
       col = c("blue", "red"), lty = 1, lwd = 1, bty = "n", x.intersp = 0.09, y.intersp = 0.2)




































library(ggplot2)

# Convert ZIP column to character to avoid it being treated as numerical
total_hospitalizations$ZIP <- as.character(total_hospitalizations$ZIP)


# Plotting observed hospitalizations
plot(total_hospitalizations$ZIP, total_hospitalizations$total_observed_hospitalizations,
     type = "l", col = "blue", lwd = 1,
     xlab = "ZIP Code", ylab = "COVID-19 Hospitalisations",
     main = "Comparison of Observed and Predicted COVID-19 Hospitalisations by ZIP Code")

# Adding lines for predicted hospitalizations
lines(total_hospitalizations$ZIP, total_hospitalizations$total_predicted_hospitalizations1, col = "black")
lines(total_hospitalizations$ZIP, total_hospitalizations$total_predicted_hospitalizations2, col = "green")
lines(total_hospitalizations$ZIP, total_hospitalizations$total_predicted_hospitalizations3, col = "orange")
lines(total_hospitalizations$ZIP, total_hospitalizations$total_predicted_hospitalizations4, col = "purple")
lines(total_hospitalizations$ZIP, total_hospitalizations$total_predicted_hospitalizations5, col = "brown")
lines(total_hospitalizations$ZIP, total_hospitalizations$total_predicted_hospitalizations6, col = "pink")
lines(total_hospitalizations$ZIP, total_hospitalizations$total_predicted_hospitalizations7, col = "red")


# Adding legend
legend("topleft", legend = c("Actual Hospitalisations", "Predicted1", "Predicted2", "Predicted3", "Predicted4", "Predicted5", "Predicted6", "Predicted7"),
       col = c("blue", "red", "green", "orange", "purple", "brown", "pink", "black"), lwd = 2, cex = .3)

# 
# 
# #Graph for best model------------
# # Load the ggplot2 library
# library(ggplot2)
# 
# # Create a data frame with the required variables
# data <- data.frame(ZIP = total_hospitalizations$ZIP,
#                    Observed = total_hospitalizations$total_observed_hospitalizations,
#                    Predicted = total_hospitalizations$total_predicted_hospitalizations7)
# 
# # Convert data from wide to long format
# data_long <- tidyr::pivot_longer(data, cols = c(Observed, Predicted), names_to = "Type", values_to = "Hospitalizations")
# 
# # Plot using ggplot
# p<-ggplot(data_long, aes(x = ZIP, y = Hospitalizations, color = Type, group = Type)) +
#   geom_line() +
#   labs(x = "ZIP Code", y = "COVID-19 Hospitalizations",
#        title = "Observed and Predicted COVID-19 Hospitalisations by ZIP Code") +
#   scale_color_manual(values = c("blue", "red")) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8), # Increase x-axis tick label size
#         axis.text.y = element_text(size = 12),                         # Increase y-axis tick label size
#         axis.title.x = element_text(size = 22, margin = margin(t = 15)), # Increase x-axis label size with a margin
#         axis.title.y = element_text(size = 22, margin = margin(r = 15)), # Increase y-axis label size with a margin
#         legend.text = element_text(size = 22),                         # Increase legend text size
#         legend.title = element_text(size = 16),                        # Increase legend title size
#         plot.title = element_text(size = 22))                          # Increase plot title size
# 
# 
# # Add extra text annotations
# p + annotate("text", x = 71, y = 3450, label = "For training: Dec 1, 2020 - Feb 28, 2021", size = 6.5, color = "black") +
#   annotate("text", x = 71, y = 3250, label = "For testing: Aug 1, 2021 - Nov 30, 2021", size = 6.5, color = "black") 























### validation for sub population category------------
# Creating sub population category variable
Final_test_data$subpopulation_category <- case_when(
   Final_test_data$SEX == "Male" & Final_test_data$age_group == "<20" & Final_test_data$race_group == "White" ~ 1,
   Final_test_data$SEX == "Male" & Final_test_data$age_group == "20-44" & Final_test_data$race_group == "White" ~ 2,
   Final_test_data$SEX == "Male" & Final_test_data$age_group == "45-64" & Final_test_data$race_group == "White" ~ 3,
   Final_test_data$SEX == "Male" & Final_test_data$age_group == "65+" & Final_test_data$race_group == "White" ~ 4,
   Final_test_data$SEX == "Female" & Final_test_data$age_group == "<20" & Final_test_data$race_group == "White" ~ 5,
   Final_test_data$SEX == "Female" & Final_test_data$age_group == "20-44" & Final_test_data$race_group == "White" ~ 6,
   Final_test_data$SEX == "Female" & Final_test_data$age_group == "45-64" & Final_test_data$race_group == "White" ~ 7,
   Final_test_data$SEX == "Female" & Final_test_data$age_group == "65+" & Final_test_data$race_group == "White" ~ 8,
   Final_test_data$SEX == "Male" & Final_test_data$age_group == "<20" & Final_test_data$race_group == "Non-white" ~ 9,
   Final_test_data$SEX == "Male" & Final_test_data$age_group == "20-44" & Final_test_data$race_group == "Non-white" ~ 10,
   Final_test_data$SEX == "Male" & Final_test_data$age_group == "45-64" & Final_test_data$race_group == "Non-white" ~ 11,
   Final_test_data$SEX == "Male" & Final_test_data$age_group == "65+" & Final_test_data$race_group == "Non-white" ~ 12,
   Final_test_data$SEX == "Female" & Final_test_data$age_group == "<20" & Final_test_data$race_group == "Non-white" ~ 13,
   Final_test_data$SEX == "Female" & Final_test_data$age_group == "20-44" & Final_test_data$race_group == "Non-white" ~ 14,
   Final_test_data$SEX == "Female" & Final_test_data$age_group == "45-64" & Final_test_data$race_group == "Non-white" ~ 15,
   Final_test_data$SEX == "Female" & Final_test_data$age_group == "65+" & Final_test_data$race_group == "Non-white" ~ 16,
   TRUE ~ NA_integer_  # Assign NA for other cases
)

# Convert the new variable to factor 
Final_test_data$subpopulation_category <- as.factor(Final_test_data$subpopulation_category)

# Adding labels to subpopulation category
levels(Final_test_data$subpopulation_category) <- c(
   "Male <20 White", "Male 20-44 White", "Male 45-64 White", "Male 65+ White",
   "Female <20 White", "Female 20-44 White", "Female 45-64 White", "Female 65+ White",
   "Male <20 Non-white", "Male 20-44 Non-white", "Male 45-64 Non-white", "Male 65+ Non-white",
   "Female <20 Non-white", "Female 20-44 Non-white", "Female 45-64 Non-white", "Female 65+ Non-white"
)

# Group by sub population category and calculate the total hospitalizations for observed and predicted values
total_hospitalizations2 <- Final_test_data %>%
   group_by(subpopulation_category) %>%
   summarise(
      total_observed_hospitalizations = sum(hospitalisation_RFA, na.rm = TRUE),
      total_predicted_hospitalizations1 = sum(predicted_hospitalisation1, na.rm = TRUE),
      total_predicted_hospitalizations2 = sum(predicted_hospitalisation2, na.rm = TRUE),
      total_predicted_hospitalizations3 = sum(predicted_hospitalisation3, na.rm = TRUE),
      total_predicted_hospitalizations4 = sum(predicted_hospitalisation4, na.rm = TRUE),
      total_predicted_hospitalizations5 = sum(predicted_hospitalisation5, na.rm = TRUE),
      total_predicted_hospitalizations6 = sum(predicted_hospitalisation6, na.rm = TRUE)
   )

# View the resulting data frame
print(total_hospitalizations2)


# Ai
predicted_variables <- c("total_predicted_hospitalizations1", "total_predicted_hospitalizations2", 
                         "total_predicted_hospitalizations3", "total_predicted_hospitalizations4", 
                         "total_predicted_hospitalizations5", "total_predicted_hospitalizations6"
)

for (i in seq_along(predicted_variables)) {
   col_name <- paste0("A_i_", i)
   total_hospitalizations2[[col_name]] <- with(total_hospitalizations2, pmin(total_observed_hospitalizations, get(predicted_variables[i])) /
                                                  pmax(total_observed_hospitalizations, get(predicted_variables[i])))
}

A_i_columns <- paste0("A_i_", 1:6)
# Subset the data frame to include only the A_i columns
A_i_data <- total_hospitalizations2[A_i_columns]
# Use the summary function to get the five-number summary for each A_i column
summary_A_i <- apply(A_i_data, 2, summary)
# Print the result
print(summary_A_i)







#3 months retro------------------





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

#write.xlsx(summary_data, "/Users/tanvirahammed/Library/CloudStorage/Box-Box/BoxSecure-DPHS-Opiod/COVID registry/Tanvir/Results/SC_demographic_summary_data.xlsx", rowNames = FALSE)


#Prisma Data---------
# dataset:merged_dataset_Table1
prisma <- readRDS("/Users/tanvirahammed/Library/CloudStorage/Box-Box/BoxSecure-DPHS-Opiod/COVID registry/Tanvir/Data/merged_dataset_Table1_ZIP.rds")

prisma <- prisma %>%
   filter(SEX != "Unknown")

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


# Create age_group variable
prisma <- prisma %>%
   mutate(age_group = case_when(
      AGE < 18 ~ "<18",
      AGE >= 18 & AGE <= 44 ~ "18-44",
      AGE >= 45 & AGE <= 64 ~ "45-64",
      AGE >= 65 ~ "65+"
   ))

# Create race_group variable
prisma <- prisma %>%
   filter(!is.na(RACE)) %>%
   mutate(race_group = case_when(
      RACE == "White" ~ "White",
      RACE != "White" ~ "Non-white"
   ))


# Filter data for training 
prisma <- prisma %>% 
   filter(EncounterDate >= as.Date("2023-01-01") & EncounterDate <= as.Date("2023-12-31"))


(cross_tab <- table(prisma$SEX, prisma$`Data Source`))
(percentage_tab <- round(prop.table(cross_tab, margin = 2) * 100, 2))

(cross_tab <- table(prisma$age_group, prisma$`Data Source`))
(percentage_tab <- round(prop.table(cross_tab, margin = 2) * 100, 2))

(cross_tab <- table(prisma$race_group, prisma$`Data Source`))
(percentage_tab <- round(prop.table(cross_tab, margin = 2) * 100, 2))


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



# Filter data for training 
training_data <- prisma %>% 
   filter(EncounterDate >= as.Date("2023-06-01") & EncounterDate <= as.Date("2023-09-30"))

# Filter data for the test set (Oct 1 and Dec 31, 2023)
test_data <- prisma %>%
   filter(EncounterDate >= as.Date("2023-10-01") & EncounterDate<= as.Date("2023-12-23"))



# training_data-----------

training_grouped_data <- training_data %>%
   group_by(ZIP, SEX, age_group, race_group) %>%
   summarize(
      occurrences = n(),
   )


training_grouped_data$subpopulation_size <- prisma$`subpopulation_size`[
   match(paste(training_grouped_data$ZIP, training_grouped_data$SEX, training_grouped_data$age_group, training_grouped_data$race_group), 
         paste(prisma$ZIP, prisma$SEX, prisma$age_group, prisma$race_group))
]




# Define all possible combinations
all_combinations <- expand.grid(
   ZIP = unique(training_grouped_data$ZIP),
   SEX = unique(training_grouped_data$SEX),
   age_group = unique(training_grouped_data$age_group),
   race_group = unique(training_grouped_data$race_group)
)


# Left join with training_data to fill missing combinations
training_grouped_data_filled <- left_join(all_combinations, training_grouped_data, by = c("ZIP", "SEX", "age_group", "race_group"))


## Merge the datasets based on the ZIP column 
training_grouped_data_filled<- training_grouped_data_filled  %>%
   left_join(selected_dataset, by = "ZIP")


# Subpopulation size 
training_grouped_data_filled <- training_grouped_data_filled %>%
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


training_grouped_data_filled$new_subpopulation_size<-round(training_grouped_data_filled$new_subpopulation_size, 0)

# Replace NA values in occurrences with 0
training_grouped_data_filled <- training_grouped_data_filled %>%
   mutate(occurrences = replace_na(occurrences, 0))





# test_data-----------

test_grouped_data <- test_data %>%
   group_by(ZIP, SEX, age_group, race_group) %>%
   summarize(
      occurrences = n(),
      # ZIP = paste(ZIP, collapse = ", ")
   )



test_grouped_data$subpopulation_size <- prisma$`subpopulation_size`[
   match(paste(test_grouped_data$ZIP, test_grouped_data$SEX, test_grouped_data$age_group, test_grouped_data$race_group), 
         paste(prisma$ZIP, prisma$SEX, prisma$age_group, prisma$race_group))
]





# Define all possible combinations
all_combinations1 <- expand.grid(
   ZIP = unique(test_grouped_data$ZIP),
   SEX = unique(test_grouped_data$SEX),
   age_group = unique(test_grouped_data$age_group),
   race_group = unique(test_grouped_data$race_group)
)


# Left join with test_data to fill missing combinations
test_grouped_data_filled <- left_join(all_combinations1, test_grouped_data, by = c("ZIP", "SEX", "age_group", "race_group"))


## Merge the datasets based on the ZIP column 
test_grouped_data_filled<- test_grouped_data_filled  %>%
   left_join(selected_dataset, by = "ZIP")


# Subpopulation size 
test_grouped_data_filled <- test_grouped_data_filled %>%
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

test_grouped_data_filled$new_subpopulation_size<-round(test_grouped_data_filled$new_subpopulation_size, 0)

# Replace NA values in occurrences with 0
test_grouped_data_filled <- test_grouped_data_filled %>%
   mutate(occurrences = replace_na(occurrences, 0))








#RFA data-----------
# Data set: filtered_data_rfa_Table1
RFA <- readRDS("/Users/tanvirahammed/Library/CloudStorage/Box-Box/BoxSecure-DPHS-Opiod/COVID registry/Tanvir/Data/filtered_data_rfa_Table1_ZIP.rds")

#RFA <- read_csv("/Users/tanvirahammed/Library/CloudStorage/Box-Box/BoxPHI-PHMR Projects/Data/SC RFA Hospitalization Data/Years 2019-2023/Clean Data/data_covid.csv")



# Creating new age groups from AGRP
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


RFA <- RFA %>%
   filter(ADMD_new >= as.Date("2023-01-01") & ADMD_new <= as.Date("2023-12-31"))


# Filter data for training (Dec 1, 2020, through June 30, 2021)
RFA_training_data <- RFA %>%
   filter(ADMD_new >= as.Date("2023-06-01") & ADMD_new <= as.Date("2023-09-30"))

# Filter data for the test set (July 1 and Dec 31, 2021)
RFA_test_data <- RFA %>%
   filter(ADMD_new >= as.Date("2023-10-01") & ADMD_new <= as.Date("2023-12-23"))






# RFA_training_data-----------
RFA_training_grouped_data <- RFA_training_data %>%
   group_by(ZIP, SEX, age_group, race_group) %>%
   summarize(
      occurrences = n(),
      # ZIP = paste(ZIP, collapse = ", ")
   )



# Define all possible combinations
all_combinations2 <- expand.grid(
   ZIP = unique(RFA_training_grouped_data$ZIP),
   SEX = unique(RFA_training_grouped_data$SEX),
   age_group = unique(RFA_training_grouped_data$age_group),
   race_group = unique(RFA_training_grouped_data$race_group)
)


# Left join with test_data to fill missing combinations
RFA_training_grouped_data <- left_join(all_combinations2, RFA_training_grouped_data, by = c("ZIP", "SEX", "age_group", "race_group"))


# Replace NA values in occurrences with 0
RFA_training_grouped_data <- RFA_training_grouped_data %>%
   mutate(occurrences = replace_na(occurrences, 0))


# RFA_test_data-----------
RFA_test_grouped_data <- RFA_test_data %>%
   group_by(ZIP, SEX, age_group, race_group) %>%
   summarize(
      occurrences = n(),
      # ZIP = paste(ZIP, collapse = ", ")
   )

# RFA_test_grouped_data <- RFA_test_grouped_data %>%
#   mutate(ZIP = str_extract(ZIP, "^[^,]+"))


# Define all possible combinations
all_combinations3 <- expand.grid(
   ZIP = unique(RFA_test_grouped_data$ZIP),
   SEX = unique(RFA_test_grouped_data$SEX),
   age_group = unique(RFA_test_grouped_data$age_group),
   race_group = unique(RFA_test_grouped_data$race_group)
)


# Left join with test_data to fill missing combinations
RFA_test_grouped_data <- left_join(all_combinations3, RFA_test_grouped_data, by = c("ZIP", "SEX", "age_group", "race_group"))


# Replace NA values in occurrences with 0
RFA_test_grouped_data <- RFA_test_grouped_data %>%
   mutate(occurrences = replace_na(occurrences, 0))


# Final test and training data (Merge the Prisma & RFA datasets based on the "zip code" column) ---------

## Final training data----------
Final_training_data <- left_join(training_grouped_data_filled, RFA_training_grouped_data, by = c("ZIP", "SEX", "age_group", "race_group"))

Final_training_data <- Final_training_data %>%
   rename(hospitalisation_RFA = occurrences.y, hospitalisation_prisma = occurrences.x) %>%
   dplyr::select(ZIP, SEX, age_group, race_group, hospitalisation_prisma, new_subpopulation_size, hospitalisation_RFA)












# Create new variables log_subpopulation_size & diff
Final_training_data$log_subpopulation_size <- log(Final_training_data$new_subpopulation_size)
Final_training_data$new_subpopulation_size <- NULL

Final_training_data$diff <- Final_training_data$hospitalisation_RFA - Final_training_data$hospitalisation_prisma
Final_training_data <- Final_training_data[Final_training_data$diff >= 0, ]

# Removing rows where SEX has missing values
Final_training_data <- Final_training_data %>%
   filter(!is.na(SEX))
Final_training_data$ZIP <- as.factor(Final_training_data$ZIP)





## Final test data----------
Final_test_data <- left_join(test_grouped_data_filled, RFA_test_grouped_data, by = c("ZIP", "SEX", "age_group", "race_group"))

Final_test_data <- Final_test_data %>%
   rename(hospitalisation_RFA = occurrences.y, hospitalisation_prisma = occurrences.x) %>%
   dplyr::select(ZIP, SEX, age_group, race_group, hospitalisation_prisma, new_subpopulation_size, hospitalisation_RFA)

# Create new variables log_subpopulation_size & diff
Final_test_data$log_subpopulation_size <- log(Final_test_data$new_subpopulation_size)
Final_test_data$new_subpopulation_size <- NULL

Final_test_data$diff <- Final_test_data$hospitalisation_RFA - Final_test_data$hospitalisation_prisma
Final_test_data <- Final_test_data[Final_test_data$diff >= 0, ]

# Removing rows where SEX has missing values
Final_test_data <- Final_test_data %>%
   filter(!is.na(SEX))

# Remove rows where ZIP is 29340
Final_test_data <- Final_test_data[Final_test_data$ZIP != 29340, ]
Final_test_data$ZIP <- factor(Final_test_data$ZIP, levels = levels(Final_training_data$ZIP))






#Model-----------
# Fit a negative binomial mixed-effects model
model1 <- glmer.nb(hospitalisation_RFA ~ hospitalisation_prisma + log_subpopulation_size + (1|ZIP), 
                   data = Final_training_data,nAGQ=0
                   #,control=glmerControl(optimizer="optimx.nlminb",optCtrl=list(maxfun=2e6))
)



model2 <- glmer.nb(hospitalisation_RFA ~ SEX + age_group + race_group + hospitalisation_prisma + log_subpopulation_size + (1|ZIP), 
                   data = Final_training_data,nAGQ=0)
#control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))


model3 <- glmer.nb(hospitalisation_RFA ~ SEX + age_group + race_group + hospitalisation_prisma 
                   +SEX*hospitalisation_prisma  + age_group*hospitalisation_prisma + race_group*hospitalisation_prisma
                   + log_subpopulation_size + (1|ZIP), data = Final_training_data,nAGQ=0)

# Without random intercept
library(MASS)
model4 <- glm.nb(hospitalisation_RFA ~ SEX + age_group + race_group + hospitalisation_prisma + log_subpopulation_size, 
                 data = Final_training_data)

#Difference model

model5 <- glmer.nb(diff ~ SEX + age_group + race_group + log_subpopulation_size + (1|ZIP), 
                   data = Final_training_data,nAGQ=0)
#control=glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

summary(model5)


#RF (model 1)------------
library(caret)

# Step 1: Set up parallel processing
cl <- makeCluster(detectCores() - 1) # Use all but one core
registerDoParallel(cl)

# Define the control
set.seed(1234)
trControl <- trainControl(method = "cv", number = 15, search = "grid")
# When performing cross-validation, we tend to go with the common 10 folds (k=10). 
# A higher k (number of folds) means that each model is trained on a larger training set and tested on a smaller test fold. In theory, this should lead to a lower prediction error as the models see more of the available data.
#A lower k means that the model is trained on a smaller training set and tested on a larger test fold. Here, the potential for the data distribution in the test fold to differ from the training set is bigger, and we should thus expect a higher prediction error on average.
set.seed(1234)
rf_default <- train(hospitalisation_RFA ~ SEX + age_group + race_group + hospitalisation_prisma + log_subpopulation_size, 
                    data = Final_training_data, 
                    method = "rf", trControl = trControl, ntree = 300)

# Print the results
print(rf_default)


#finding the best mtry value
tuneGrid <- expand.grid(.mtry = c(1: 9))

rf_mtry <- train(hospitalisation_RFA ~ SEX + age_group + race_group + hospitalisation_prisma + log_subpopulation_size, 
                 data = Final_training_data, method = "rf", tuneGrid = tuneGrid, trControl = trControl, 
                 ntree = 300)
print(rf_mtry)
# mtry is how many variables will be included in the first split.
# mtry depends on the number of columns and the model mode. The default in randomForest::randomForest() is floor(sqrt(ncol(x))) for classification and floor(ncol(x)/3) for regression.

#storing the best mtry value
best_mtry <- rf_mtry$bestTune$mtry
tuneGrid <- expand.grid(.mtry = best_mtry)


fit_rf <- train(hospitalisation_RFA ~ SEX + age_group + race_group + hospitalisation_prisma + log_subpopulation_size, 
                data = Final_training_data, method = "rf",
                tuneGrid = tuneGrid, 
                trControl = trControl, ntree = 300)







#RF (model 3)------------
set.seed(1234)
trControl <- trainControl(method = "cv", number = 15, search = "grid")
rf_default <- train(hospitalisation_RFA ~ hospitalisation_prisma 
                    + log_subpopulation_size, 
                    data = Final_training_data, 
                    method = "rf", trControl = trControl, ntree = 300)


# Print the results
print(rf_default)


#finding the best mtry value
set.seed(1234)
tuneGrid <- expand.grid(.mtry = c(1: 3))

set.seed(1234)
rf_mtry <- train(hospitalisation_RFA ~ hospitalisation_prisma 
                 + log_subpopulation_size, 
                 data = Final_training_data, 
                 method = "rf", tuneGrid = tuneGrid, trControl = trControl, ntree = 300)




print(rf_mtry)
# mtry is how many variables will be included in the first split.
# mtry depends on the number of columns and the model mode. The default in randomForest::randomForest() is floor(sqrt(ncol(x))) for classification and floor(ncol(x)/3) for regression.

#storing the best mtry value
best_mtry <- rf_mtry$bestTune$mtry
tuneGrid <- expand.grid(.mtry = best_mtry)




set.seed(1234)
fit_rf2 <- train(hospitalisation_RFA ~ hospitalisation_prisma 
                 + log_subpopulation_size, 
                 data = Final_training_data, 
                 method = "rf", tuneGrid = tuneGrid, trControl = trControl, ntree = 300)




Final_test_data$ZIP <- factor(Final_test_data$ZIP, levels = levels(Final_training_data$ZIP))

# Predict on the count scale--------------
predictions_counts1 <- predict(model1, newdata = Final_test_data, type = "response", allow.new.levels = TRUE)
predictions_counts2 <- predict(model2, newdata = Final_test_data, type = "response", allow.new.levels = TRUE)
predictions_counts3 <- predict(model3, newdata = Final_test_data, type = "response", allow.new.levels = TRUE)
predictions_counts4 <- predict(model4, newdata = Final_test_data, type = "response", allow.new.levels = TRUE)
predictions_counts5 <- predict(model5, newdata = Final_test_data, type = "response", allow.new.levels = TRUE)
predictions_counts6 <-predict(fit_rf2, Final_test_data)
predictions_counts7 <-predict(fit_rf, Final_test_data)


# Attach the predictions to Final_test_data
Final_test_data$predicted_hospitalisation1 <- predictions_counts1
Final_test_data$predicted_hospitalisation2 <- predictions_counts2
Final_test_data$predicted_hospitalisation3 <- predictions_counts3
Final_test_data$predicted_hospitalisation4 <- predictions_counts4
Final_test_data$predicted_hospitalisation5 <- predictions_counts5
Final_test_data$predicted_hospitalisation6 <- predictions_counts6
Final_test_data$predicted_hospitalisation7 <- predictions_counts7


##Final_test_data---------
#write.xlsx(Final_test_data, "/Users/tanvirahammed/Library/CloudStorage/Box-Box/BoxSecure-DPHS-Opiod/COVID registry/Tanvir/Results/Final_test_data_ZIP_June.xlsx", rowNames = FALSE)


# Group by ZIP and calculate the total hospitalizations for observed and predicted values
total_hospitalizations <- Final_test_data %>%
   group_by(ZIP) %>%
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


#write.xlsx(total_hospitalizations, "/Users/tanvirahammed/Library/CloudStorage/Box-Box/BoxPHI-PHMR Projects/Tanvir/Data/Final Data/total_hospitalizations_ZIP_June.xlsx", rowNames = FALSE)






predicted_variables <- c("total_predicted_hospitalizations1", "total_predicted_hospitalizations2", 
                         "total_predicted_hospitalizations3", "total_predicted_hospitalizations4", 
                         "total_predicted_hospitalizations5", "total_predicted_hospitalizations6",
                         "total_predicted_hospitalizations7"
)


# Ai------------
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
# MAPE_columns <- paste0("MAPE_", 1:7)
# MAPE_data <- total_hospitalizations[MAPE_columns]
# 
# # Compute the mean MAPE for each prediction column
# (mean_MAPE <- apply(MAPE_data, 2, mean, na.rm = TRUE))
# 
# 
stopCluster(cl)
registerDoSEQ() # Return to sequential processing




# 
# write.xlsx(total_hospitalizations, "/Users/tanvirahammed/Library/CloudStorage/Box-Box/BoxSecure-DPHS-Opiod/COVID registry/Tanvir/Results/total_hospitalizations_new_ZIP.xlsx", rowNames = FALSE)
# write.xlsx(total_hospitalizations2, "/Users/tanvirahammed/Library/CloudStorage/Box-Box/BoxSecure-DPHS-Opiod/COVID registry/Tanvir/Results/total_hospitalizations2_new_ZIP.xlsx", rowNames = FALSE)

