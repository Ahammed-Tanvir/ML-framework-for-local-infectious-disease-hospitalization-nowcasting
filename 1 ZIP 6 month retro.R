################################################################################
# ZIP-code level: 6-month retrospective estimation (Table 1, Table 3)
#                 + imputation for all 305 ZIP codes (Table 5)
# Training: 2023-01-01 to 2023-06-30 | Test: 2023-07-01 to 2023-12-31
################################################################################

library(dplyr)
library(tidyr)
library(stringr)
library(readxl)
library(openxlsx)
library(lme4)
library(MASS)
library(readr)
library(ggplot2)
library(caret)
library(missForest)
library(mice)
library(sf)

## =============================================================================
## 1. Census / demographic predictors by ZCTA
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

## =============================================================================
## 2. Prisma Health EHR extract -> TABLE 1 (Prisma half) + subpop size
## =============================================================================
prisma <- readRDS("/Users/tanvirahammed/Library/CloudStorage/Box-Box/BoxSecure-DPHS-Opiod/COVID registry/Tanvir/Data/merged_dataset_Table1_ZIP.rds")

prisma <- prisma %>%
   filter(SEX != "Unknown")

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

prisma <- prisma %>%
   mutate(age_group = case_when(
      AGE < 18 ~ "<18",
      AGE >= 18 & AGE <= 44 ~ "18-44",
      AGE >= 45 & AGE <= 64 ~ "45-64",
      AGE >= 65 ~ "65+"
   ))

prisma <- prisma %>%
   filter(!is.na(RACE)) %>%
   mutate(race_group = case_when(
      RACE == "White" ~ "White",
      RACE != "White" ~ "Non-white"
   ))

prisma <- prisma %>%
   filter(EncounterDate >= as.Date("2023-01-01") & EncounterDate <= as.Date("2023-12-31"))

# ---- TABLE 1 (Prisma) --------------------------------------------------------
(cross_tab <- table(prisma$SEX, prisma$`Data Source`))
(percentage_tab <- round(prop.table(cross_tab, margin = 2) * 100, 2))

(cross_tab <- table(prisma$age_group, prisma$`Data Source`))
(percentage_tab <- round(prop.table(cross_tab, margin = 2) * 100, 2))

(cross_tab <- table(prisma$race_group, prisma$`Data Source`))
(percentage_tab <- round(prop.table(cross_tab, margin = 2) * 100, 2))
# ------------------------------------------------------------------------------

prisma <- prisma  %>%
   left_join(selected_dataset, by = "ZIP")

prisma <- prisma %>%
   mutate_at(vars(Percent_male, `Percent female`, Percent_white, `Percent Non-white`, `Percent age 0-17`, `Percent age 18-44`, `Percent age 45-64`, `Percent age 65 years and over`), as.numeric)

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

## =============================================================================
## 3. Train / test split + grouping to (ZIP, SEX, age_group, race_group) counts
## =============================================================================
training_data <- prisma %>%
   filter(EncounterDate >= as.Date("2023-01-01") & EncounterDate <= as.Date("2023-06-30"))

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

all_combinations <- expand.grid(
   ZIP = unique(training_grouped_data$ZIP),
   SEX = unique(training_grouped_data$SEX),
   age_group = unique(training_grouped_data$age_group),
   race_group = unique(training_grouped_data$race_group)
)

training_grouped_data_filled <- left_join(all_combinations, training_grouped_data, by = c("ZIP", "SEX", "age_group", "race_group"))

training_grouped_data_filled<- training_grouped_data_filled  %>%
   left_join(selected_dataset, by = "ZIP")

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
         TRUE ~ NA_real_
      )
   )

training_grouped_data_filled$new_subpopulation_size<-round(training_grouped_data_filled$new_subpopulation_size, 0)

training_grouped_data_filled <- training_grouped_data_filled %>%
   mutate(occurrences = replace_na(occurrences, 0))

# test_data-----------
test_grouped_data <- test_data %>%
   group_by(ZIP, SEX, age_group, race_group) %>%
   summarize(
      occurrences = n(),
   )

test_grouped_data$subpopulation_size <- prisma$`subpopulation_size`[
   match(paste(test_grouped_data$ZIP, test_grouped_data$SEX, test_grouped_data$age_group, test_grouped_data$race_group),
         paste(prisma$ZIP, prisma$SEX, prisma$age_group, prisma$race_group))
]

all_combinations1 <- expand.grid(
   ZIP = unique(test_grouped_data$ZIP),
   SEX = unique(test_grouped_data$SEX),
   age_group = unique(test_grouped_data$age_group),
   race_group = unique(test_grouped_data$race_group)
)

test_grouped_data_filled <- left_join(all_combinations1, test_grouped_data, by = c("ZIP", "SEX", "age_group", "race_group"))

test_grouped_data_filled<- test_grouped_data_filled  %>%
   left_join(selected_dataset, by = "ZIP")

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
         TRUE ~ NA_real_
      )
   )

test_grouped_data_filled$new_subpopulation_size<-round(test_grouped_data_filled$new_subpopulation_size, 0)

test_grouped_data_filled <- test_grouped_data_filled %>%
   mutate(occurrences = replace_na(occurrences, 0))

## =============================================================================
## 4. SC RFA data -> TABLE 1 (RFA half)
## =============================================================================
RFA <- readRDS("/Users/tanvirahammed/Library/CloudStorage/Box-Box/BoxSecure-DPHS-Opiod/COVID registry/Tanvir/Data/filtered_data_rfa_Table1_ZIP.rds")

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

RFA <- RFA %>%
   filter(ADMD_new >= as.Date("2023-01-01") & ADMD_new <= as.Date("2023-12-31"))

# ---- TABLE 1 (RFA) -----------------------------------------------------------
(cross_tab <- table(RFA$SEX))
(percentage_tab <- round(prop.table(cross_tab) * 100, 2))

(cross_tab <- table(RFA$age_group))
(percentage_tab <- round(prop.table(cross_tab) * 100, 2))

(cross_tab <- table(RFA$race_group))
(percentage_tab <- round(prop.table(cross_tab) * 100, 2))
# ------------------------------------------------------------------------------

RFA_training_data <- RFA %>%
   filter(ADMD_new >= as.Date("2023-01-01") & ADMD_new <= as.Date("2023-06-30"))

RFA_test_data <- RFA %>%
   filter(ADMD_new >= as.Date("2023-07-01") & ADMD_new <= as.Date("2023-12-31"))

RFA_training_grouped_data <- RFA_training_data %>%
   group_by(ZIP, SEX, age_group, race_group) %>%
   summarize(
      occurrences = n(),
   )

all_combinations2 <- expand.grid(
   ZIP = unique(RFA_training_grouped_data$ZIP),
   SEX = unique(RFA_training_grouped_data$SEX),
   age_group = unique(RFA_training_grouped_data$age_group),
   race_group = unique(RFA_training_grouped_data$race_group)
)

RFA_training_grouped_data <- left_join(all_combinations2, RFA_training_grouped_data, by = c("ZIP", "SEX", "age_group", "race_group"))

RFA_training_grouped_data <- RFA_training_grouped_data %>%
   mutate(occurrences = replace_na(occurrences, 0))

RFA_test_grouped_data <- RFA_test_data %>%
   group_by(ZIP, SEX, age_group, race_group) %>%
   summarize(
      occurrences = n(),
   )

all_combinations3 <- expand.grid(
   ZIP = unique(RFA_test_grouped_data$ZIP),
   SEX = unique(RFA_test_grouped_data$SEX),
   age_group = unique(RFA_test_grouped_data$age_group),
   race_group = unique(RFA_test_grouped_data$race_group)
)

RFA_test_grouped_data <- left_join(all_combinations3, RFA_test_grouped_data, by = c("ZIP", "SEX", "age_group", "race_group"))

RFA_test_grouped_data <- RFA_test_grouped_data %>%
   mutate(occurrences = replace_na(occurrences, 0))

## =============================================================================
## 5. Merge Prisma + RFA -> Final_training_data / Final_test_data
## =============================================================================
Final_training_data <- left_join(training_grouped_data_filled, RFA_training_grouped_data, by = c("ZIP", "SEX", "age_group", "race_group"))

Final_training_data <- Final_training_data %>%
   rename(hospitalisation_RFA = occurrences.y, hospitalisation_prisma = occurrences.x) %>%
   dplyr::select(ZIP, SEX, age_group, race_group, hospitalisation_prisma, new_subpopulation_size, hospitalisation_RFA)

Final_training_data$log_subpopulation_size <- log(Final_training_data$new_subpopulation_size)
Final_training_data$new_subpopulation_size <- NULL

Final_training_data$diff <- Final_training_data$hospitalisation_RFA - Final_training_data$hospitalisation_prisma
Final_training_data <- Final_training_data[Final_training_data$diff >= 0, ]

Final_training_data <- Final_training_data %>%
   filter(!is.na(SEX))

Final_test_data <- left_join(test_grouped_data_filled, RFA_test_grouped_data, by = c("ZIP", "SEX", "age_group", "race_group"))

Final_test_data <- Final_test_data %>%
   rename(hospitalisation_RFA = occurrences.y, hospitalisation_prisma = occurrences.x) %>%
   dplyr::select(ZIP, SEX, age_group, race_group, hospitalisation_prisma, new_subpopulation_size, hospitalisation_RFA)

Final_test_data$log_subpopulation_size <- log(Final_test_data$new_subpopulation_size)
Final_test_data$new_subpopulation_size <- NULL

Final_test_data$diff <- Final_test_data$hospitalisation_RFA - Final_test_data$hospitalisation_prisma
Final_test_data <- Final_test_data[Final_test_data$diff >= 0, ]

Final_test_data <- Final_test_data %>%
   filter(!is.na(SEX))

# Remove rows where ZIP is 29340
Final_test_data <- Final_test_data[Final_test_data$ZIP != 29340, ]

## =============================================================================
## 6. Fig-1-style map of observed Prisma hospitalizations (91 ZIPs w/ Prisma data)
## =============================================================================
Final_test_data1 <- Final_test_data %>%
   group_by(ZIP) %>%
   summarize(
      hospitalisation_prisma = sum(hospitalisation_prisma),
   )

sc_zcta_sf = tigris::zctas(state = "SC", class = "sf", year = '2010')

hosp_ZIP_esti_impu <- data.frame(ZIP = as.character(Final_test_data1$ZIP),
                                 Observed = Final_test_data1$hospitalisation_prisma)

sc_zcta_sf = left_join(sc_zcta_sf, hosp_ZIP_esti_impu
                       %>% dplyr::select(Observed, ZCTA5CE10 = ZIP), by = 'ZCTA5CE10')

sc_zcta_sf = mutate(sc_zcta_sf,
                    Observed_grouped = case_when(Observed < 100 ~ '1',
                                                 Observed >= 100 & Observed <200 ~ '2',
                                                 Observed >= 200 & Observed <350 ~ '3',
                                                 Observed >= 350 & Observed <500 ~ '4',
                                                 Observed >= 500 & Observed <650 ~ '5'))

sc_zcta_sf$Observed_grouped <- factor(sc_zcta_sf$Observed_grouped,
                                      levels = c("5", "4", "3", "2", "1"),
                                      labels = c("500 - 650", "350 - 500", "200 - 350", "100 - 200", "0 - 100"))

ggplot(sc_zcta_sf) +
   geom_sf(aes(fill = Observed_grouped), color = 'gray20') +
   scale_fill_manual(values = c('#4d004b', '#810f7c', '#8c96c6',  '#bfd3e6', '#d0d1e6'),
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

## =============================================================================
## 7. Models: 5 negative-binomial + 2 Random Forest  ->  TABLE 3 (ZIP half)
## =============================================================================
model1 <- glmer.nb(hospitalisation_RFA ~ hospitalisation_prisma + log_subpopulation_size + (1|ZIP),
                   data = Final_training_data,nAGQ=0
)

model2 <- glmer.nb(hospitalisation_RFA ~ SEX + age_group + race_group + hospitalisation_prisma + log_subpopulation_size + (1|ZIP),
                   data = Final_training_data,nAGQ=0)

model3 <- glmer.nb(hospitalisation_RFA ~ SEX + age_group + race_group + hospitalisation_prisma
                   +SEX*hospitalisation_prisma  + age_group*hospitalisation_prisma + race_group*hospitalisation_prisma
                   + log_subpopulation_size + (1|ZIP), data = Final_training_data,nAGQ=0)

model4 <- glm.nb(hospitalisation_RFA ~ SEX + age_group + race_group + hospitalisation_prisma + log_subpopulation_size,
                 data = Final_training_data)

model5 <- glmer.nb(diff ~ SEX + age_group + race_group + log_subpopulation_size + (1|ZIP),
                   data = Final_training_data,nAGQ=0)

summary(model5)

#RF (model 1)------------
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)

set.seed(1234)
trControl <- trainControl(method = "cv", number = 10, search = "grid")

set.seed(1234)
rf_default <- train(hospitalisation_RFA ~ SEX + age_group + race_group + hospitalisation_prisma + log_subpopulation_size,
                    data = Final_training_data,
                    method = "rf", trControl = trControl, ntree = 300)

print(rf_default)

tuneGrid <- expand.grid(.mtry = c(1: 9))

rf_mtry <- train(hospitalisation_RFA ~ SEX + age_group + race_group + hospitalisation_prisma + log_subpopulation_size,
                 data = Final_training_data, method = "rf", tuneGrid = tuneGrid, trControl = trControl,
                 ntree = 300)
print(rf_mtry)

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

print(rf_default)

set.seed(1234)
tuneGrid <- expand.grid(.mtry = c(1: 3))

set.seed(1234)
rf_mtry <- train(hospitalisation_RFA ~ hospitalisation_prisma
                 + log_subpopulation_size,
                 data = Final_training_data,
                 method = "rf", tuneGrid = tuneGrid, trControl = trControl, ntree = 300)

print(rf_mtry)

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

Final_test_data$predicted_hospitalisation1 <- predictions_counts1
Final_test_data$predicted_hospitalisation2 <- predictions_counts2
Final_test_data$predicted_hospitalisation3 <- predictions_counts3
Final_test_data$predicted_hospitalisation4 <- predictions_counts4
Final_test_data$predicted_hospitalisation5 <- predictions_counts5
Final_test_data$predicted_hospitalisation6 <- predictions_counts6
Final_test_data$predicted_hospitalisation7 <- predictions_counts7

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

predicted_variables <- c("total_predicted_hospitalizations1", "total_predicted_hospitalizations2",
                         "total_predicted_hospitalizations3", "total_predicted_hospitalizations4",
                         "total_predicted_hospitalizations5", "total_predicted_hospitalizations6",
                         "total_predicted_hospitalizations7"
)

for (i in seq_along(predicted_variables)) {
   col_name <- paste0("A_i_", i)
   total_hospitalizations[[col_name]] <- with(total_hospitalizations, pmin(total_observed_hospitalizations, get(predicted_variables[i])) /
                                                 pmax(total_observed_hospitalizations, get(predicted_variables[i])))
}

A_i_columns <- paste0("A_i_", 1:7)
A_i_data <- total_hospitalizations[A_i_columns]
# ---- TABLE 3 (ZIP half) -------------------------------------------------------
(summary_A_i <- apply(A_i_data, 2, summary))
# ------------------------------------------------------------------------------

# Preserve this Table-3 version under its own name so Figure 2 (section 9) can
# still read it after section 8 below reassigns `total_hospitalizations` to the
# Table-5 imputation output. (Necessary only because Figure 2 is included in
# this file; the original script this is based on doesn't have that step.)
total_hospitalizations_table3 <- total_hospitalizations

stopCluster(cl)
registerDoSEQ()

## =============================================================================
## 8. Imputation for full 305 ZIP codes -> TABLE 5 (ZIP half)
## =============================================================================
Final_test_data_imp <- Final_test_data %>%
   dplyr::select(ZIP, SEX, age_group, race_group, hospitalisation_RFA,
                 log_subpopulation_size, predicted_hospitalisation7)

Final_test_data_imp$ZIP <- as.numeric(as.character(Final_test_data_imp$ZIP))

Final_test_data_imp<- Final_test_data_imp  %>%
   left_join(selected_dataset, by = "ZIP")

# Finding the ZIPs for imputation-------------------
unique_ZIP_data_before <- unique(RFA_test_grouped_data$ZIP)
unique_ZIP_data_after <- unique(Final_test_data_imp$ZIP)

removed_ZIP <- setdiff(unique_ZIP_data_before, unique_ZIP_data_after)
removed_ZIP <- data.frame(ZIP = removed_ZIP)

removed_ZIP_data <- RFA_test_data %>% filter(ZIP %in% removed_ZIP$ZIP)

removed_ZIP_data <- removed_ZIP_data %>%
   mutate(age_group = case_when(
      AGRP %in% c("AGE 00-04", "AGE 05-11", "AGE 12-15", "AGE 16-17") ~ "<18",
      AGRP %in% c("AGE 18-24", "AGE 25-29", "AGE 30-34", "AGE 35-39", "AGE 40-44") ~ "18-44",
      AGRP %in% c("AGE 45-49", "AGE 50-54", "AGE 55-59", "AGE 60-64") ~ "45-64",
      AGRP %in% c("AGE 65-69", "AGE 70-74", "AGE 75-79", "AGE 80-84", "AGE 85+") ~ "65+",
      TRUE ~ NA_character_
   ))

removed_ZIP_data <- removed_ZIP_data %>%
   group_by(ZIP, SEX, age_group,  race_group) %>%
   summarize(
      hospitalisation_RFA = n()
   )

all_combinationsR <- expand.grid(
   ZIP = unique(removed_ZIP_data$ZIP),
   SEX = unique(removed_ZIP_data$SEX),
   age_group = unique(removed_ZIP_data$age_group),
   race_group = unique(removed_ZIP_data$race_group)
)

removed_ZIP_data <- left_join(all_combinationsR, removed_ZIP_data, by = c("ZIP", "SEX", "age_group", "race_group"))

removed_ZIP_data<- removed_ZIP_data  %>%
   left_join(selected_dataset, by = "ZIP")

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
         TRUE ~ NA_real_
      )
   )

removed_ZIP_data$new_subpopulation_size<-round(removed_ZIP_data$new_subpopulation_size, 0)

removed_ZIP_data <- removed_ZIP_data %>%
   mutate(hospitalisation_RFA = replace_na(hospitalisation_RFA, 0))

removed_ZIP_data$log_subpopulation_size <- log(removed_ZIP_data$new_subpopulation_size)
removed_ZIP_data$new_subpopulation_size <- NULL

Final_test_data_imp <- Final_test_data_imp %>%
   rename(
      `Percent female` = `Percent female`,
      `Percent Non-white` = `Percent Non-white`,
      `Percent age 0-17` = `Percent age 0-17`,
      `Percent age 18-44` = `Percent age 18-44`,
      `Percent age 45-64` = `Percent age 45-64`,
      `Percent age 65 years and over` = `Percent age 65 years and over`
   )

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

# Check for missing values in the dataset
summary(imputation_data)
cols_with_na <- colnames(imputation_data)[colSums(is.na(imputation_data)) > 0]
cols_with_na <- cols_with_na[cols_with_na != "predicted_hospitalisation7"]

print(cols_with_na)

imputation_data_clean <- imputation_data[complete.cases(imputation_data[, cols_with_na]), ]

summary(imputation_data_clean)
imputation_data_clean<- as.data.frame(imputation_data_clean)
imputation_data_clean <- imputation_data_clean[is.finite(imputation_data_clean$log_subpopulation_size), ]
imputation_data_clean1 <- imputation_data_clean
imputation_data_clean$hospitalisation_RFA <- NULL

set.seed(1234)
missForest_result <- missForest(imputation_data_clean)

colnames(imputation_data_clean) <- make.names(colnames(imputation_data_clean))

set.seed(1234)
mice_imputed <- data.frame(
   ZIP = imputation_data_clean$ZIP,
   SEX = imputation_data_clean$SEX,
   age_group = imputation_data_clean$age_group,
   race_group = imputation_data_clean$race_group,
   total_predicted_hospitalizations_before_imputation = imputation_data_clean$predicted_hospitalisation7,
   total_predicted_hospitalizations1 = missForest_result$ximp$predicted_hospitalisation7,
   imputed_pmm = complete(mice(imputation_data_clean, method = "pmm"))$predicted_hospitalisation7,
   imputed_cart = complete(mice(imputation_data_clean, method = "cart"))$predicted_hospitalisation7,
   imputed_lasso = complete(mice(imputation_data_clean, method = "lasso.norm"))$predicted_hospitalisation7
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

predicted_variables <- c("total_predicted_hospitalizations1", "total_predicted_hospitalizations2",
                         "total_predicted_hospitalizations3", "total_predicted_hospitalizations4"
)

for (i in seq_along(predicted_variables)) {
   col_name <- paste0("A_i_", i)
   total_hospitalizations[[col_name]] <- with(total_hospitalizations, pmin(total_observed_hospitalizations, get(predicted_variables[i])) /
                                                 pmax(total_observed_hospitalizations, get(predicted_variables[i])))
}

A_i_columns <- paste0("A_i_", 1:4)
A_i_data <- total_hospitalizations[A_i_columns]
summary_A_i <- apply(A_i_data, 2, summary)
# ---- TABLE 5 (ZIP half, RF Model 2 basis row) --------------------------------
print(summary_A_i)
n_distinct(total_hospitalizations$ZIP)
# ------------------------------------------------------------------------------
# NOTE: this produces only the "Random Forest Model 2" row of Table 5 (ZIP half).
# The "Negative Binomial Model 2" row of Table 5 (and both rows of Table 4) come
# from a second imputation pass using predicted_hospitalisation2 (NB Model 2) as
# the imputation basis instead of predicted_hospitalisation7 (RF Model 2) -- see
# ZIP_Table4_5_NBmodel2_basis_RUN_AFTER_STEP1.R, which must be run in the
# SAME R session right after this script (it reuses Final_test_data, RFA_test_data,
# RFA_test_grouped_data, selected_dataset, census_data still in memory).

## =============================================================================
## 9. FIGURE 2 (observed vs. estimated, RF Model 2, 125 ZIP codes)
## =============================================================================
data <- data.frame(
   ZIP = total_hospitalizations_table3$ZIP,
   Observed  = total_hospitalizations_table3$total_observed_hospitalizations,
   Estimated = total_hospitalizations_table3$total_predicted_hospitalizations7
)

data <- data %>% left_join(selected_dataset %>% dplyr::select(ZIP, Population), by = "ZIP")
data <- data %>% mutate(Difference = abs(Observed - Estimated))
data <- data %>% arrange(desc(Difference)) %>% mutate(ZIP = factor(ZIP, levels = ZIP))

data_sorted <- data %>% arrange(Population)
data_long <- pivot_longer(data_sorted, cols = c(Observed, Estimated),
                          names_to = "Type", values_to = "Hospitalizations") %>%
   mutate(Type = factor(Type, levels = c("Observed", "Estimated")))

p <- ggplot(data_long, aes(x = factor(ZIP, levels = data_sorted$ZIP), y = Hospitalizations, fill = Type)) +
   geom_bar(stat = "identity", position = "dodge") +
   labs(x = "ZIP Codes", y = "COVID-19 Hospitalizations",
        title = "Observed and Estimated COVID-19 Hospitalizations by ZIP Codes") +
   scale_fill_manual(values = c("blue", "red")) +
   theme_minimal() +
   theme(axis.text.x = element_text(angle = 70, hjust = 1, size = 10),
         axis.text.y = element_text(size = 12),
         axis.title.x = element_text(size = 22),
         axis.title.y = element_text(size = 22),
         legend.text = element_text(size = 16),
         legend.title = element_text(size = 16),
         plot.title = element_text(size = 22))
print(p)















################################################################################
# ZIP-code level imputation, NEGATIVE BINOMIAL MODEL 2 basis
# Produces the "Negative Binomial Model 2" rows of TABLE 4 and TABLE 5 (ZIP half).
################################################################################

library(dplyr)
library(readxl)
library(openxlsx)
library(ggplot2)
library(sf)
library(missForest)
library(mice)

# Select only the specified columns
Final_test_data_imp <- Final_test_data %>%
   dplyr::select(ZIP, SEX, age_group, race_group, hospitalisation_RFA,
                 log_subpopulation_size, predicted_hospitalisation2)

Final_test_data_imp$ZIP <- as.numeric(as.character(Final_test_data_imp$ZIP))

## Merge the datasets based on the ZIP column
Final_test_data_imp<- Final_test_data_imp  %>%
   left_join(selected_dataset, by = "ZIP")

# Finding the ZIPs for imputation-------------------
unique_ZIP_data_before <- unique(RFA_test_grouped_data$ZIP)
unique_ZIP_data_after <- unique(Final_test_data_imp$ZIP)

# Find the set difference
removed_ZIP <- setdiff(unique_ZIP_data_before, unique_ZIP_data_after)
removed_ZIP <- data.frame(ZIP = removed_ZIP)

# Filter filtered_data_rfa to keep only rows where ZIP is in removed_ZIP_values
removed_ZIP_data <- RFA_test_data %>% filter(ZIP %in% removed_ZIP$ZIP)

# Create age_group variable
removed_ZIP_data <- removed_ZIP_data %>%
   mutate(age_group = case_when(
      AGRP %in% c("AGE 00-04", "AGE 05-11", "AGE 12-15", "AGE 16-17") ~ "<18",
      AGRP %in% c("AGE 18-24", "AGE 25-29", "AGE 30-34", "AGE 35-39", "AGE 40-44") ~ "18-44",
      AGRP %in% c("AGE 45-49", "AGE 50-54", "AGE 55-59", "AGE 60-64") ~ "45-64",
      AGRP %in% c("AGE 65-69", "AGE 70-74", "AGE 75-79", "AGE 80-84", "AGE 85+") ~ "65+",
      TRUE ~ NA_character_
   ))

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
         TRUE ~ NA_real_
      )
   )

removed_ZIP_data$new_subpopulation_size<-round(removed_ZIP_data$new_subpopulation_size, 0)

# Replace NA values in occurrences with 0
removed_ZIP_data <- removed_ZIP_data %>%
   mutate(hospitalisation_RFA = replace_na(hospitalisation_RFA, 0))

removed_ZIP_data$log_subpopulation_size <- log(removed_ZIP_data$new_subpopulation_size)
removed_ZIP_data$new_subpopulation_size <- NULL

# Merge Final_test_data_imp and removed_ZIP_data
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

# Check for missing values in the dataset
summary(imputation_data)
cols_with_na <- colnames(imputation_data)[colSums(is.na(imputation_data)) > 0]
cols_with_na <- cols_with_na[cols_with_na != "predicted_hospitalisation2"]

print(cols_with_na)

# Step 3: Remove rows with NAs in these columns
imputation_data_clean <- imputation_data[complete.cases(imputation_data[, cols_with_na]), ]

summary(imputation_data_clean)
imputation_data_clean<- as.data.frame(imputation_data_clean)
imputation_data_clean <- imputation_data_clean[is.finite(imputation_data_clean$log_subpopulation_size), ]
imputation_data_clean1 <- imputation_data_clean
imputation_data_clean$hospitalisation_RFA <- NULL

# Run missForest on the cleaned dataset including the column with missing values
set.seed(1234)
missForest_result <- missForest(imputation_data_clean)

# Automatically clean column names to be valid R names
colnames(imputation_data_clean) <- make.names(colnames(imputation_data_clean))

set.seed(1234)
mice_imputed <- data.frame(
   ZIP = imputation_data_clean$ZIP,
   SEX = imputation_data_clean$SEX,
   age_group = imputation_data_clean$age_group,
   race_group = imputation_data_clean$race_group,
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

## =============================================================================
## TABLE 5 (ZIP half, "Negative Binomial Model 2" row): all matched ZIPs
## =============================================================================
total_hospitalizations_table5 <- total_hospitalizations[total_hospitalizations$total_observed_hospitalizations > 50, ]

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
n_distinct(total_hospitalizations_table5$ZIP)

## =============================================================================
## TABLE 4 (ZIP half, "Negative Binomial Model 2" row): insufficient-coverage only
## (i.e. before-imputation prediction == 0, meaning no Prisma signal at all)
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
n_distinct(total_hospitalizations_table4$ZIP)
