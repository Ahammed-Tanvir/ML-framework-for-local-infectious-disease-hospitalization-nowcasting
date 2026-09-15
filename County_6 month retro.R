################################################################################
# County level, Table 2, Table 3,Figure 1, Figure 3.
################################################################################

library(readxl)
library(tidyverse)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
library(lme4)
library(MASS)
library(caret)
library(tigris)
library(lubridate)
library(scales)

## =============================================================================
## PART A -- DATA CONSTRUCTION 
## =============================================================================

# ---- Prisma extract ----------------------------------------------------------
hosp_data_prisma <- read_csv("/Users/tanvirahammed/Library/CloudStorage/Box-Box/BoxPHI-PHMR Projects/Iromi/EHR Data construction/PRISMA HEALTH/Prisma_Health_COVID_Patient_Data/Prisma_Health_COVID_Patient_Data_9_15.csv",
                             col_select = c("PAT_ID", "EncounterDate", "ZIP",
                                            "Week", "Is.Abnormal.",
                                            "Hospitalization", "ED.Visit", "Outpatient",
                                            "SEX" , "ETHNICITY", "DOB", "Race"))

hosp_data_prisma <- hosp_data_prisma[!is.na(hosp_data_prisma$Is.Abnormal.) & hosp_data_prisma$Is.Abnormal. != 0, ]

prisma <- hosp_data_prisma[format(hosp_data_prisma$EncounterDate, "%Y") %in% c("2020", "2021", "2022", "2023"), ]

prisma <- prisma %>%
   mutate(
      DOB = as.Date(DOB, format = "%m/%d/%Y"),
      AGE = as.integer(difftime(EncounterDate, DOB, units = "weeks") / 52.25)
   ) %>%
   filter(EncounterDate >= DOB)

prisma$Year_Month <- format(prisma$EncounterDate, "%Y-%m")

# ---- ZIP -> county crosswalk --------------------------------------------------
dataset1 <- read_excel("/Users/tanvirahammed/Library/CloudStorage/Box-Box/BoxSecure-DPHS-Opiod/COVID registry/Tanvir/Data/12.28.21 SC Rural Healthcare data by Zip.xlsx")

dataset1 <- dataset1 %>%
   rename(ZIP = zip_code)

selected_dataset <- dataset1 %>%
   dplyr::select(ZIP, city, county, Population, `Percent female`, `Percent Non-white`, `Percent age 0-19`, `Percent age 20-44`, `Percent age 45-64`, `Percent age 65 years and over`)

merged_dataset <- prisma  %>%
   left_join(selected_dataset, by = "ZIP")

summary_table_prisma <- merged_dataset %>%
   group_by(county, Year_Month, EncounterDate) %>%
   summarise(Count = n())

county_totals <- summary_table_prisma %>%
   group_by(county) %>%
   summarise(Total_Count = sum(Count))


selected_counties_19 <- c(
   "Aiken", "Anderson", "Cherokee", "Clarendon", "Fairfield", "Greenville",
   "Greenwood",
   "Kershaw", "Laurens", "Lee", "Lexington", "Newberry",
   "Oconee", "Orangeburg", "Pickens", "Richland", "Spartanburg", "Sumter",
   "Union"
)
selected_counties <- county_totals %>%
   filter(!is.na(county) & county %in% paste(selected_counties_19, "County"))

filtered_summary_table_prisma <- summary_table_prisma %>%
   filter(county %in% selected_counties$county)

filtered_merged_dataset <- merged_dataset %>%
   filter(county %in% selected_counties$county)

# ---- RFA extract ---------------------------------------------------------------
hosp_data_rfa <- read_csv("/Users/tanvirahammed/Library/CloudStorage/Box-Box/BoxPHI-PHMR Projects/Data/SC Health Records/RFA/Years 2019-2023/Clean Data/data_covid.csv")

non_unique_RFA_ID <- hosp_data_rfa %>%
    group_by(RFA_ID) %>%
    summarise(Num_Occurrences = n()) %>%
    filter(Num_Occurrences > 1)

non_unique_records <- hosp_data_rfa %>%
  filter(RFA_ID %in% non_unique_RFA_ID$RFA_ID)

## Filter data for unique hospitalization case
filtered_data_rfa <- hosp_data_rfa %>%
  distinct(RFA_ID, ADMYEAR, ADMMTH, .keep_all = TRUE)

# Find rows where RFA_ID are similar, and the difference in ADMMTH is 1
result <- filtered_data_rfa %>%
  group_by(RFA_ID) %>%
  filter(n() > 1 & all(diff(ADMMTH) == 1))

result <- result %>%
  arrange(RFA_ID, ADMMTH)

result_filtered <- result %>%
  filter(ADMMTH != DISMTH)

selected_rfa_ids <- result_filtered$RFA_ID

selected_rows <- result %>%
  filter(RFA_ID %in% selected_rfa_ids)

df_filtered <- selected_rows %>%
  group_by(RFA_ID) %>%
  filter(ADMMTH != lag(DISMTH) | is.na(lag(DISMTH)))

## removing and adding these rows in filtered_data_rfa
selected_ids <- df_filtered$RFA_ID

filtered_data_rfa <- filtered_data_rfa %>%
  filter(!RFA_ID %in% selected_ids)

filtered_data_rfa <- bind_rows(filtered_data_rfa, df_filtered)

# Create a new variable named "Year_Month" using ADMYEAR and ADMMTH
filtered_data_rfa$Year_Month <- paste(filtered_data_rfa$ADMYEAR, filtered_data_rfa$ADMMTH, sep = "-")

filtered_data_rfa <- filtered_data_rfa %>%
  filter(Year_Month != "NA-NA")

filtered_data_rfa$Year_Month <- str_replace(
  filtered_data_rfa$Year_Month,
  "(\\d{4})-(\\d{1}$)",
  "\\1-0\\2"
)

county_mapping <- c(
  "1" = "Abbeville", "2" = "Aiken", "3" = "Allendale", "4" = "Anderson", "5" = "Bamberg",
  "6" = "Barnwell", "7" = "Beaufort", "8" = "Berkeley", "9" = "Calhoun", "10" = "Charleston",
  "11" = "Cherokee", "12" = "Chester", "13" = "Chesterfield", "14" = "Clarendon", "15" = "Colleton",
  "16" = "Darlington", "17" = "Dillon", "18" = "Dorchester", "19" = "Edgefield", "20" = "Fairfield",
  "21" = "Florence", "22" = "Georgetown", "23" = "Greenville", "24" = "Greenwood", "25" = "Hampton",
  "26" = "Horry", "27" = "Jasper", "28" = "Kershaw", "29" = "Lancaster", "30" = "Laurens",
  "31" = "Lee", "32" = "Lexington", "33" = "McCormick", "34" = "Marion", "35" = "Marlboro",
  "36" = "Newberry", "37" = "Oconee", "38" = "Orangeburg", "39" = "Pickens", "40" = "Richland",
  "41" = "Saluda", "42" = "Spartanburg", "43" = "Sumter", "44" = "Union", "45" = "Williamsburg", "46" = "York"
)

filtered_data_rfa$county <- county_mapping[as.character(filtered_data_rfa$COUNTY)]

filtered_data_rfa <- filtered_data_rfa %>%
  dplyr::select(RFA_ID, ZIP, Year_Month, SEX, RACE, AGRP, county)

unique_county_data_before <- unique(filtered_data_rfa$county)

selected_counties_rfa <- c(
   "Aiken", "Anderson", "Cherokee", "Clarendon", "Fairfield", "Greenville",
   "Greenwood",
   "Kershaw", "Laurens", "Lee", "Lexington", "Newberry",
   "Oconee", "Orangeburg", "Pickens", "Richland", "Spartanburg", "Sumter",
   "Union"
)

filtered_data_rfa1 <- filtered_data_rfa %>%
  filter(county %in% selected_counties_rfa)

unique_county_data_after <- unique(filtered_data_rfa1$county)

removed_county <- setdiff(unique_county_data_before, unique_county_data_after)
removed_county <- data.frame(county = removed_county)

removed_county_data <- filtered_data_rfa %>%
  filter(county %in% removed_county$county)

filtered_data_rfa <- filtered_data_rfa %>%
  filter(county %in% selected_counties_rfa)

# Add "County" suffix if missing
add_County_if_missing <- function(county_name) {
  if (!grepl(" County$", county_name)) {
    return(paste(county_name, "County"))
  }
  return(county_name)
}
filtered_data_rfa$county <- sapply(filtered_data_rfa$county, add_County_if_missing)

# Cleaning: SEX
filtered_data_rfa_Table1 <- filtered_data_rfa

filtered_data_rfa_Table1$SEX <- ifelse(is.na(filtered_data_rfa_Table1$SEX), NA,
                                       ifelse(filtered_data_rfa_Table1$SEX == "M", "Male", "Female"))

filtered_data_rfa_Table1  <- filtered_data_rfa_Table1 %>%
  mutate(RACE = case_when(
    RACE == 1 ~ "White",
    RACE == 2 ~ "African-American",
    RACE == 3 ~ "Asian",
    RACE == 4 ~ "American Indian",
    RACE == 5 ~ "Other",
    RACE == 6 ~ "Hispanic",
    RACE == 7 ~ NA_character_,
    TRUE ~ as.character(RACE)
  ))

# ---- Table 1 objects: age_group / race_group + Data Source --------------------
merged_dataset_Table1 <- filtered_merged_dataset %>%
   mutate(age_group = case_when(
      AGE < 18 ~ "<18",
      AGE >= 18 & AGE <= 44 ~ "18-44",
      AGE >= 45 & AGE <= 64 ~ "45-64",
      AGE >= 65 ~ "65+"
   ))

merged_dataset_Table1 <- merged_dataset_Table1 %>%
   mutate(Race = ifelse(Race %in% c("Unknown", "Patient Refused"), NA, Race)) %>%
   filter(!is.na(Race)) %>%
   mutate(race_group = case_when(
      Race == "White or Caucasian" ~ "White",
      TRUE ~ "Non-white"
   ))

filtered_data_rfa_Table1 <- filtered_data_rfa_Table1 %>%
  mutate(`Data Source` = 1)  # Source = 1 for rfa

merged_dataset_Table1 <- merged_dataset_Table1 %>%
  mutate(`Data Source` = 0)  # Source = 0 for prisma

## =============================================================================
## PART B -- ANALYSIS: Table 2, Table 3 (county half), Figure 1, Figure 3
## =============================================================================

## -----------------------------------------------------------------------------
## 1. ZIP -> county crosswalk + county-level demographic predictors
## -----------------------------------------------------------------------------
census_data <- read_csv("/Users/tanvirahammed/Library/CloudStorage/Box-Box/BoxPHI-PHMR Projects/Tanvir/Project 2 (Prediction)/Data/Age and Sex by ZCTA.csv")
census_data_race <- read_csv("/Users/tanvirahammed/Library/CloudStorage/Box-Box/BoxPHI-PHMR Projects/Tanvir/Project 2 (Prediction)/Data/Race by ZCTA.csv")
census_data <- left_join(census_data, census_data_race, by = "ZCTA")

census_data <- mutate(census_data,
                      age.18.under = under5 + `5-9 years` + `10-14 years` + `15-19 years` * 0.6,
                      age.18.44 = `15-19 years` * 0.4 + `20-24 years` + `25-29 years` + `30-34 years` + `35-39 years` + `40-44 years`,
                      age.45.64 = `45-49 years` + `50-54 years` + `55-59 years` + `60-64 years`,
                      age.65.over = `65-69 years` + `70-74 years` + `75-79 years` + `80-84 years` + over85,
                      total.population.study = age.18.under+age.18.44 + age.45.64 + age.65.over)

census_data <- mutate(census_data,
                      `Percent age 0-17` = age.18.under / Total_population,
                      `Percent age 18-44` = age.18.44 / Total_population,
                      `Percent age 45-64` = age.45.64 / Total_population,
                      `Percent age 65 years and over` = age.65.over / Total_population,
                      Percent_white = Race_white/ Total_population,
                      `Percent female` = Total_Female/Total_population)

census_data <- census_data %>% rename(ZIP = ZCTA, Population=Total_population)

county_crosswalk_only <- as.data.frame(dataset1 %>% dplyr::select(ZIP, county))

# Left join with training_data to fill missing combinations
census_data <- left_join(census_data, county_crosswalk_only, by = "ZIP")

selected_dataset <- as.data.frame(census_data %>%
                                     dplyr::select(ZIP, county, Population, `Percent female`, Percent_white, `Percent age 0-17`, `Percent age 18-44`, `Percent age 45-64`, `Percent age 65 years and over`))

selected_dataset <- selected_dataset %>%
   mutate_at(vars(`Percent_white`, `Percent age 0-17`, `Percent age 18-44`, `Percent age 45-64`, `Percent age 65 years and over`), as.numeric)

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

## -----------------------------------------------------------------------------
## 2. prisma <- merged_dataset_Table1 (built fresh in Part A above)
## -----------------------------------------------------------------------------
prisma <- merged_dataset_Table1


prisma <- prisma %>%
   filter(SEX != "Unknown")

(cross_tab <- table(prisma$SEX, prisma$`Data Source`))
(percentage_tab <- round(prop.table(cross_tab, margin = 2) * 100, 2))

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

prisma <- prisma  %>%
   left_join(selected_dataset, by = "ZIP")

prisma <- prisma %>%
   mutate_at(vars(Percent_male, `Percent female`, Percent_white, `Percent Non-white`, `Percent age 0-17`, `Percent age 18-44`, `Percent age 45-64`, `Percent age 65 years and over`), as.numeric)

prisma <- prisma %>%
   filter(!is.na(Population))

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
         TRUE ~ NA_real_
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
prisma <- prisma %>%
   filter(Date >= as.Date("2023-01-01") & Date <= as.Date("2023-12-31"))

(cross_tab <- table(prisma$SEX, prisma$`Data Source`))
(percentage_tab <- round(prop.table(cross_tab, margin = 2) * 100, 2))

(cross_tab <- table(prisma$age_group, prisma$`Data Source`))
(percentage_tab <- round(prop.table(cross_tab, margin = 2) * 100, 2))

(cross_tab <- table(prisma$race_group, prisma$`Data Source`))
(percentage_tab <- round(prop.table(cross_tab, margin = 2) * 100, 2))

## -----------------------------------------------------------------------------
## 3. Train / test split + grouping to (county, SEX, age_group, race_group) counts
## -----------------------------------------------------------------------------
training_data <- prisma %>%
  filter(Date >= as.Date("2023-01-01") & Date <= as.Date("2023-06-30"))

test_data <- prisma %>%
  filter(Date >= as.Date("2023-07-01") & Date <= as.Date("2023-12-31"))

training_grouped_data <- training_data %>%
  group_by(county, SEX, age_group, race_group) %>%
  summarize(
    occurrences = n(),
    subpopulation_size = sum(subpopulation_size, na.rm = TRUE)
  )

all_combinations <- expand.grid(
  county = unique(training_grouped_data$county),
  SEX = unique(training_grouped_data$SEX),
  age_group = unique(training_grouped_data$age_group),
  race_group = unique(training_grouped_data$race_group)
)

training_grouped_data_filled <- left_join(all_combinations, training_grouped_data, by = c("county", "SEX", "age_group", "race_group"))

training_grouped_data_filled<- training_grouped_data_filled  %>%
  left_join(summary_data, by = "county")

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
         TRUE ~ NA_real_
      )
   )

training_grouped_data_filled$new_subpopulation_size<-round(training_grouped_data_filled$new_subpopulation_size, 0)

training_grouped_data_filled <- training_grouped_data_filled %>%
  mutate(occurrences = replace_na(occurrences, 0))

test_grouped_data <- test_data %>%
  group_by(county, SEX, age_group, race_group) %>%
  summarize(
    occurrences = n(),
    subpopulation_size = sum(subpopulation_size, na.rm = TRUE)
  )

all_combinations <- expand.grid(
  county = unique(test_grouped_data$county),
  SEX = unique(test_grouped_data$SEX),
  age_group = unique(test_grouped_data$age_group),
  race_group = unique(test_grouped_data$race_group)
)

test_grouped_data_filled <- left_join(all_combinations, test_grouped_data, by = c("county", "SEX", "age_group", "race_group"))

test_grouped_data_filled<- test_grouped_data_filled  %>%
  left_join(summary_data, by = "county")

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
         TRUE ~ NA_real_
      )
   )

test_grouped_data_filled$new_subpopulation_size<-round(test_grouped_data_filled$new_subpopulation_size, 0)

test_grouped_data_filled <- test_grouped_data_filled %>%
  mutate(occurrences = replace_na(occurrences, 0))

## -----------------------------------------------------------------------------
## 4. RFA <- filtered_data_rfa_Table1 (built fresh in Part A above)
## -----------------------------------------------------------------------------
RFA <- filtered_data_rfa_Table1

RFA <- RFA %>%
   mutate(age_group = case_when(
      AGRP %in% c("AGE 00-04", "AGE 05-11", "AGE 12-15", "AGE 16-17") ~ "<18",
      AGRP %in% c("AGE 18-24", "AGE 25-29", "AGE 30-34", "AGE 35-39", "AGE 40-44") ~ "18-44",
      AGRP %in% c("AGE 45-49", "AGE 50-54", "AGE 55-59", "AGE 60-64") ~ "45-64",
      AGRP %in% c("AGE 65-69", "AGE 70-74", "AGE 75-79", "AGE 80-84", "AGE 85+") ~ "65+",
      TRUE ~ NA_character_
   ))

RFA <- RFA %>%
  filter(!is.na(RACE)) %>%
  mutate(race_group = case_when(
    RACE == "White" ~ "White",
    RACE != "White" ~ "Non-white"
  ))
summary(as.factor(RFA$race_group))

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

RFA_training_data <- RFA %>%
  filter(Date >= as.Date("2023-01-01") & Date <= as.Date("2023-06-30"))

RFA_test_data <- RFA %>%
  filter(Date >= as.Date("2023-07-01") & Date <= as.Date("2023-12-31"))

RFA_training_grouped_data <- RFA_training_data %>%
  group_by(county, SEX, age_group, race_group) %>%
  summarize(
    occurrences = n(),

  )


all_combinations_rfa_train <- expand.grid(
  county = unique(RFA_training_grouped_data$county),
  SEX = unique(RFA_training_grouped_data$SEX),
  age_group = unique(RFA_training_grouped_data$age_group),
  race_group = unique(RFA_training_grouped_data$race_group)
)
RFA_training_grouped_data <- left_join(all_combinations_rfa_train, RFA_training_grouped_data,
                                        by = c("county", "SEX", "age_group", "race_group")) %>%
  mutate(occurrences = replace_na(occurrences, 0))

RFA_test_grouped_data <- RFA_test_data %>%
  group_by(county, SEX, age_group, race_group) %>%
  summarize(
    occurrences = n(),

  )

all_combinations_rfa_test <- expand.grid(
  county = unique(RFA_test_grouped_data$county),
  SEX = unique(RFA_test_grouped_data$SEX),
  age_group = unique(RFA_test_grouped_data$age_group),
  race_group = unique(RFA_test_grouped_data$race_group)
)
RFA_test_grouped_data <- left_join(all_combinations_rfa_test, RFA_test_grouped_data,
                                    by = c("county", "SEX", "age_group", "race_group")) %>%
  mutate(occurrences = replace_na(occurrences, 0))

## -----------------------------------------------------------------------------
## 5. Merge Prisma + RFA -> Final_training_data / Final_test_data
## -----------------------------------------------------------------------------
Final_training_data <- left_join(training_grouped_data_filled, RFA_training_grouped_data, by = c("county", "SEX", "age_group", "race_group"))

Final_training_data <- Final_training_data %>%
  rename(hospitalisation_RFA = occurrences.y, hospitalisation_prisma = occurrences.x) %>%
  dplyr::select(county, SEX, age_group, race_group, hospitalisation_prisma, new_subpopulation_size, hospitalisation_RFA)

Final_training_data$log_subpopulation_size <- log(Final_training_data$new_subpopulation_size)
Final_training_data$new_subpopulation_size <- NULL

Final_training_data$diff <- Final_training_data$hospitalisation_RFA - Final_training_data$hospitalisation_prisma

Final_test_data <- left_join(test_grouped_data_filled, RFA_test_grouped_data, by = c("county", "SEX", "age_group", "race_group"))

Final_test_data <- Final_test_data %>%
  rename(hospitalisation_RFA = occurrences.y, hospitalisation_prisma = occurrences.x) %>%
  dplyr::select(county, SEX, age_group, race_group, hospitalisation_prisma, new_subpopulation_size, hospitalisation_RFA)

Final_test_data$log_subpopulation_size <- log(Final_test_data$new_subpopulation_size)
Final_test_data$new_subpopulation_size <- NULL

Final_test_data$diff <- Final_test_data$hospitalisation_RFA - Final_test_data$hospitalisation_prisma

## -----------------------------------------------------------------------------
## 6. Map of observed Prisma hospitalizations (8-county snapshot)
## -----------------------------------------------------------------------------
Final_test_data1 <- Final_test_data %>%
   group_by(county) %>%
   summarize(
      hospitalisation_prisma = sum(hospitalisation_prisma),
   )

sc_counties <- tigris::counties(state = "SC", year = 2021, class = "sf")

hosp_county_esti_impu <- data.frame(county = as.character(Final_test_data1$county),
                                 Observed = Final_test_data1$hospitalisation_prisma)

sc_counties = left_join(sc_counties, hosp_county_esti_impu
                       %>% dplyr::select(Observed, NAMELSAD = county), by = 'NAMELSAD')

sc_counties = mutate(sc_counties,
                    Observed_grouped = case_when(Observed < 50 ~ '1',
                                                 Observed >= 50 & Observed <100 ~ '2',
                                                 Observed >= 100 & Observed <150 ~ '3',
                                                 Observed >= 150 & Observed <200 ~ '4'))

sc_counties$Observed_grouped <- factor(sc_counties$Observed_grouped,
                                      levels = c( "4", "3", "2", "1"),
                                      labels = c( "200 - 150", "100 - 150", "50 - 100", "0 - 50"))

ggplot(sc_counties) +
   geom_sf(aes(fill = Observed_grouped), color = 'gray20') +
   scale_fill_manual(values = c( '#810f7c', '#8c96c6',  '#bfd3e6', '#d0d1e6'),
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

## -----------------------------------------------------------------------------
## 7. Models: 5 negative-binomial + 2 Random Forest  ->  TABLE 3 (county half)
## -----------------------------------------------------------------------------
model1 <- glmer.nb(hospitalisation_RFA ~ hospitalisation_prisma + log_subpopulation_size + (1|county),
                   data = Final_training_data,nAGQ=0)

model2 <- glmer.nb(hospitalisation_RFA ~ SEX + age_group + race_group + hospitalisation_prisma + log_subpopulation_size + (1|county),
                   data = Final_training_data,nAGQ=0)

model3 <- glmer.nb(hospitalisation_RFA ~ SEX + age_group + race_group + hospitalisation_prisma
                   +SEX*hospitalisation_prisma  + age_group*hospitalisation_prisma + race_group*hospitalisation_prisma
                   + log_subpopulation_size + (1|county), data = Final_training_data,nAGQ=0)

model4 <- glm.nb(hospitalisation_RFA ~ SEX + age_group + race_group + hospitalisation_prisma + log_subpopulation_size,
                 data = Final_training_data)

model5 <- glmer.nb(diff ~ SEX + age_group + race_group + log_subpopulation_size + (1|county),
                   data = Final_training_data,nAGQ=0)

summary(model5)

#RF------------
trControl <- trainControl(method = "cv", number = 10, search = "grid")
set.seed(1234)
rf_default <- train(hospitalisation_RFA ~ hospitalisation_prisma + log_subpopulation_size,
                    data = Final_training_data,
                    method = "rf", trControl = trControl)

print(rf_default)

set.seed(1234)
tuneGrid <- expand.grid(.mtry = c(2: 4))
rf_mtry <- train(hospitalisation_RFA ~ hospitalisation_prisma + log_subpopulation_size,
                 data = Final_training_data,
                 method = "rf", tuneGrid = tuneGrid, trControl = trControl)
print(rf_mtry)
best_mtry <- rf_mtry$bestTune$mtry
tuneGrid <- expand.grid(.mtry = best_mtry)

set.seed(1234)
fit_rf <- train(hospitalisation_RFA ~ hospitalisation_prisma + log_subpopulation_size,
                data = Final_training_data,
                method = "rf", tuneGrid = tuneGrid, trControl = trControl)

####### model2-------------
trControl <- trainControl(method = "cv", number = 5, search = "grid")
set.seed(1234)
rf_default <- train(hospitalisation_RFA ~ SEX + age_group + race_group + hospitalisation_prisma
                    + log_subpopulation_size, data = Final_training_data,
                    method = "rf", trControl = trControl, ntree = 300)

print(rf_default)

set.seed(1234)
tuneGrid <- expand.grid(.mtry = c(1:15))
rf_mtry <- train(hospitalisation_RFA ~ SEX + age_group + race_group + hospitalisation_prisma
                 + log_subpopulation_size, data = Final_training_data,
                 method = "rf", tuneGrid = tuneGrid, trControl = trControl, ntree = 300)
print(rf_mtry)
best_mtry <- rf_mtry$bestTune$mtry
tuneGrid <- expand.grid(.mtry = best_mtry)

set.seed(1234)
fit_rf1 <- train(hospitalisation_RFA ~ SEX + age_group + race_group + hospitalisation_prisma
                + log_subpopulation_size, data = Final_training_data, method = "rf",
                tuneGrid = tuneGrid, trControl = trControl, ntree = 300)

#accuracy
predictions_counts1 <- predict(model1, newdata = Final_test_data, type = "response")
predictions_counts2 <- predict(model2, newdata = Final_test_data, type = "response")
predictions_counts3 <- predict(model3, newdata = Final_test_data, type = "response")
predictions_counts4 <- predict(model4, newdata = Final_test_data, type = "response")
predictions_counts5 <- predict(model5, newdata = Final_test_data, type = "response")
predictions_counts6 <-predict(fit_rf, Final_test_data)
predictions_counts7 <-predict(fit_rf1, Final_test_data)

Final_test_data$predicted_hospitalisation1 <- predictions_counts1
Final_test_data$predicted_hospitalisation2 <- predictions_counts2
Final_test_data$predicted_hospitalisation3 <- predictions_counts3
Final_test_data$predicted_hospitalisation4 <- predictions_counts4
Final_test_data$predicted_hospitalisation5 <- predictions_counts5
Final_test_data$predicted_hospitalisation6 <- predictions_counts6
Final_test_data$predicted_hospitalisation7 <- predictions_counts7

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
A_i_data <- total_hospitalizations[A_i_columns]
# ---- TABLE 3 (county half) ---------------------------------------------------
(summary_A_i <- apply(A_i_data, 2, summary))
# ------------------------------------------------------------------------------

total_hospitalizations_table3 <- total_hospitalizations

## -----------------------------------------------------------------------------
## 8. FIGURE 3 (observed vs. estimated, RF Model 2, 19 counties)
## -----------------------------------------------------------------------------
data <- data.frame(county = total_hospitalizations$county,
                   Observed = total_hospitalizations$total_observed_hospitalizations,
                   Estimated = total_hospitalizations$total_predicted_hospitalizations7)

data = left_join(data, summary_data
                        %>% dplyr::select(county, Total_Population), by = 'county')

data <- data %>%
   arrange(Total_Population) %>%
   mutate(county = factor(county, levels = county[order(Total_Population)]))

data_long <- pivot_longer(data, cols = c(Observed, Estimated), names_to = "Type", values_to = "Hospitalizations")

data_long <- data_long %>%
   mutate(Type = factor(Type, levels = c("Observed", "Estimated")))

p <- ggplot(data_long, aes(x = county, y = Hospitalizations, fill = Type)) +
   geom_bar(stat = "identity", position = "dodge") +
   labs(x = "Counties", y = "COVID-19 Hospitalizations",
        title = "Observed and Estimated COVID-19 Hospitalizations by Counties") +
   scale_fill_manual(values = c("blueviolet", "azure4")) +
   theme_minimal() +
   theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 15),
         axis.text.y = element_text(size = 12),
         axis.title.x = element_text(size = 18, margin = margin(t = 15)),
         axis.title.y = element_text(size = 18, margin = margin(r = 15)),
         legend.text = element_text(size = 16),
         legend.title = element_text(size = 16),
         plot.title = element_text(size = 20))

print(p)

## -----------------------------------------------------------------------------
## 9. FIGURE 1 (Greenville/Richland/Sumter/Pickens time series, Prisma vs RFA)
## -----------------------------------------------------------------------------
RFA_selected <- RFA %>% dplyr::select(county, Year_Month)
RFA_selected <- RFA_selected %>%
  group_by(county, Year_Month) %>%
  summarise(Count = n())

prisma_selected <- prisma %>% dplyr::select(county, Year_Month)
prisma_selected <- prisma_selected %>%
  group_by(county, Year_Month) %>%
  summarise(Count = n())

RFA_selected <- RFA_selected %>% mutate(`Data Source` = 1)
prisma_selected <- prisma_selected %>% mutate(`Data Source` = 0)

combined_data1 <- bind_rows(RFA_selected, prisma_selected)
combined_data1$Date <- as.Date(paste0(combined_data1$Year_Month, "-01"), format = "%Y-%m-%d")
combined_data1 <- combined_data1 %>%
  filter(Date >= as.Date("2020-03-01") & Date <= as.Date("2023-12-31"))

combined_data1$`Data Source` <- factor(
  combined_data1$`Data Source`,
  levels = c(1, 0),
  labels = c("RFA", "Prisma")
)

combined_data1 <- combined_data1 %>%
  arrange(county, Date)

selected_counties_plot <- c("Greenville County", "Richland County", "Sumter County", "Pickens County")
combined_data1 <- combined_data1 %>% filter(county %in% selected_counties_plot)

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
   facet_wrap(~ county, scales = "fixed", ncol = 2)












################################################################################
# County-level imputation, RANDOM FOREST MODEL 1 basis (Prisma count + subpop only)
# Produces the "Random Forest Model 1" rows of TABLE 4 and TABLE 5 (county half).
################################################################################

library(dplyr)
library(readxl)
library(openxlsx)
library(ggplot2)
library(missForest)
library(mice)

## =============================================================================
## 1. This script's OWN selected_dataset / summary_data (incl. Berkeley patch)
## =============================================================================
census_data <- read_csv("/Users/tanvirahammed/Library/CloudStorage/Box-Box/BoxPHI-PHMR Projects/Tanvir/Project 2 (Prediction)/Data/Age and Sex by ZCTA.csv")
census_data_race <- read_csv("/Users/tanvirahammed/Library/CloudStorage/Box-Box/BoxPHI-PHMR Projects/Tanvir/Project 2 (Prediction)/Data/Race by ZCTA.csv")
census_data <- left_join(census_data, census_data_race, by = "ZCTA")

census_data <- mutate(census_data,
                      age.18.under = under5 + `5-9 years` + `10-14 years` + `15-19 years` * 0.6,
                      age.18.44 = `15-19 years` * 0.4 + `20-24 years` + `25-29 years` + `30-34 years` + `35-39 years` + `40-44 years`,
                      age.45.64 = `45-49 years` + `50-54 years` + `55-59 years` + `60-64 years`,
                      age.65.over = `65-69 years` + `70-74 years` + `75-79 years` + `80-84 years` + over85,
                      total.population.study = age.18.under+age.18.44 + age.45.64 + age.65.over)

census_data <- mutate(census_data,
                      `Percent age 0-17` = age.18.under / Total_population,
                      `Percent age 18-44` = age.18.44 / Total_population,
                      `Percent age 45-64` = age.45.64 / Total_population,
                      `Percent age 65 years and over` = age.65.over / Total_population,
                      Percent_white = Race_white/ Total_population,
                      `Percent female` = Total_Female/Total_population)

census_data <- census_data %>% rename(ZIP = ZCTA, Population=Total_population)

selected_dataset <- as.data.frame(census_data %>%
                                     dplyr::select(ZIP, Population, `Percent female`, Percent_white, `Percent age 0-17`, `Percent age 18-44`, `Percent age 45-64`, `Percent age 65 years and over`))

selected_dataset <- selected_dataset %>%
   mutate_at(vars(`Percent_white`, `Percent age 0-17`, `Percent age 18-44`, `Percent age 45-64`, `Percent age 65 years and over`), as.numeric)

selected_dataset <- selected_dataset %>%
   mutate(
      Percent_male = 1 - `Percent female`,
      `Percent Non-white` = 1 - `Percent_white`
   )

## adding county dataset (dataset1 already loaded in step3/Part A, but this
## script originally re-reads it, so keeping that as-is for fidelity)
dataset1 <- read_excel("/Users/tanvirahammed/Library/CloudStorage/Box-Box/BoxSecure-DPHS-Opiod/COVID registry/Tanvir/Data/12.28.21 SC Rural Healthcare data by Zip.xlsx")

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

## =============================================================================
## 2. Final_test_data (from step3's session, RF Model 1 basis) + removed_county_data
## =============================================================================
Final_test_data <- Final_test_data %>%
   dplyr::select(county, SEX, age_group, race_group, hospitalisation_RFA, log_subpopulation_size,
                 predicted_hospitalisation6)

Final_test_data <- left_join(Final_test_data, summary_data, by = "county")

# removed_county_data: reuse the raw capture from step3/Part A (RFA_ID, ZIP,
# Year_Month, SEX, RACE, AGRP, county -- pre "County"-suffix, pre-recoding)
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
      TRUE ~ as.character(RACE)
   ))

removed_county_data <- removed_county_data %>%
   mutate(age_group = case_when(
      AGRP %in% c("AGE 00-04", "AGE 05-11", "AGE 12-15", "AGE 16-17") ~ "<18",
      AGRP %in% c("AGE 18-24", "AGE 25-29", "AGE 30-34", "AGE 35-39", "AGE 40-44") ~ "18-44",
      AGRP %in% c("AGE 45-49", "AGE 50-54", "AGE 55-59", "AGE 60-64") ~ "45-64",
      AGRP %in% c("AGE 65-69", "AGE 70-74", "AGE 75-79", "AGE 80-84", "AGE 85+") ~ "65+",
      TRUE ~ NA_character_
   ))

removed_county_data <- removed_county_data %>%
   filter(!is.na(RACE)) %>%
   mutate(race_group = case_when(
      RACE == "White" ~ "White",
      RACE != "White" ~ "Non-white"
   ))

removed_county_data$Date <- as.Date(paste0(removed_county_data$Year_Month, "-01"), format = "%Y-%m-%d")

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
         
         TRUE ~ NA_real_
      )
   )

removed_county_data$new_subpopulation_size<-round(removed_county_data$new_subpopulation_size, 0)
removed_county_data$log_subpopulation_size<-log(removed_county_data$new_subpopulation_size)
removed_county_data$new_subpopulation_size<- NULL

## =============================================================================
## 3. Merge + impute
## =============================================================================
colnames(removed_county_data) <- make.names(colnames(removed_county_data))
colnames(Final_test_data) <- make.names(colnames(Final_test_data))

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

# Check for missing values in the dataset
summary(imputation_data)
cols_with_na <- colnames(imputation_data)[colSums(is.na(imputation_data)) > 0]
cols_with_na <- cols_with_na[cols_with_na != "predicted_hospitalisation6"]

print(cols_with_na)

imputation_data_clean <- imputation_data[complete.cases(imputation_data[, cols_with_na]), ]

summary(imputation_data_clean)
imputation_data_clean<- as.data.frame(imputation_data_clean)
imputation_data_clean1 <- imputation_data_clean

imputation_data_clean$hospitalisation_RFA <- NULL

set.seed(1234)
missForest_result <- missForest(imputation_data_clean)

set.seed(1234)
mice_imputed <- data.frame(
   county = imputation_data_clean$county,
   SEX = imputation_data_clean$SEX,
   age_group = imputation_data_clean$age_group,
   race_group = imputation_data_clean$race_group,
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

## =============================================================================
## TABLE 5 (county half, "Random Forest Model 1" row): all counties
## =============================================================================
total_hospitalizations_table5 <- total_hospitalizations

predicted_variables <- c("total_predicted_hospitalizations1", "total_predicted_hospitalizations2",
                         "total_predicted_hospitalizations3", "total_predicted_hospitalizations4"
)

for (i in seq_along(predicted_variables)) {
   col_name <- paste0("A_i_", i)
   total_hospitalizations_table5[[col_name]] <- with(total_hospitalizations_table5, pmin(total_observed_hospitalizations, get(predicted_variables[i])) /
                                                        pmax(total_observed_hospitalizations, get(predicted_variables[i])))
}

A_i_columns <- paste0("A_i_", 1:4)
A_i_data <- total_hospitalizations_table5[A_i_columns]
summary_A_i <- apply(A_i_data, 2, summary)
print(summary_A_i)
n_distinct(total_hospitalizations_table5$county)

## =============================================================================
## TABLE 4 (county half, "Random Forest Model 1" row): insufficient-coverage only
## (before-imputation prediction == 0, meaning no Prisma signal at all)
## =============================================================================
total_hospitalizations_table4 <- total_hospitalizations[total_hospitalizations$total_predicted_hospitalizations_before_imputation == 0, ]

for (i in seq_along(predicted_variables)) {
   col_name <- paste0("A_i_", i)
   total_hospitalizations_table4[[col_name]] <- with(total_hospitalizations_table4, pmin(total_observed_hospitalizations, get(predicted_variables[i])) /
                                                        pmax(total_observed_hospitalizations, get(predicted_variables[i])))
}

A_i_columns <- paste0("A_i_", 1:4)
A_i_data <- total_hospitalizations_table4[A_i_columns]
summary_A_i <- apply(A_i_data, 2, summary)
print(summary_A_i)
n_distinct(total_hospitalizations_table4$county)
