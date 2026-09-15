################################################################################
# ZIP-code level: WEEKLY nowcasting
# Produces: TABLE 6 (ZIP half: sufficient / insufficient / overall)
#           FIGURE 4 (weekly observed/estimated/accuracy maps, panels a-d)
# Training: through 2023-07-02 | Test: 2023-07-03 to 2023-10-01
################################################################################

library(dplyr)
library(tidyr)
library(stringr)
library(readr)
library(readxl)
library(openxlsx)
library(lme4)
library(data.table)
library(lubridate)
library(doParallel)
library(caret)
library(mice)
library(tigris)
library(ggplot2)
library(sf)

## =============================================================================
## PART A -- data prep (Estimation_July3toOct1_ZIP_taking_all_variables.R, lines 1-360)
## =============================================================================

#Demographic variables----------------
census_data <- read_csv("/Users/tanvirahammed/Library/CloudStorage/Box-Box/BoxPHI-PHMR Projects/Data/Community Data/SC Census Tract ZIP Code Data/CRPH_ZIP_new.csv")

census_data <- census_data %>%
   dplyr::select(
      zip_code, Population, `% Rural`, `Percent female`, `Percent Non-white`,
      `PCP Placement Score`, `OB Placement Score`, `Percent age 0-19`,
      `Percent age 20-44`, `Percent age 45-64`, `Percent age 65 years and over`,
      `Percent Women aged 15-50`, `Percent < 30 minutes from PCP`,
      `Percent < 30 minutes from OB/GYN provider`, `Percent < 30 minutes from a Hospital`,
      `Percent < 30 minutes from OB Hospital Unit`, `Number of Hospitals`,
      `Number of Home Health Agencies`, `Number of Hospice`,
      `Percent Labor force participation`, `Percent Unemployed`, `Percent Uninsured`,
      `Percent <19 Uninsured`, `Median Income`, `Percent in Poverty`,
      `Inpatient - Total`, `Inpatient - Alzheimer's`, `Inpatient - Asthma`,
      `Inpatient - Congestive Heart Failure`, `Inpatient - COPD`,
      `Inpatient - Depression`, `Inpatient - Type I Diabetes`,
      `Inpatient - Type II Diabetes`, `Inpatient - Hepatitis C`,
      `Inpatient - Hypertension`, `Inpatient - Obstetrics`,
      `Inpatient - Mental Disorders`, `ED - Total`, `ED - Alzheimer’s`,
      `ED - Asthma`, `ED - Congestive Heart Failure`, `ED - COPD`,
      `ED - Depression`, `ED - Type I Diabetes`, `ED - Type II Diabetes`,
      `ED - Hepatitis C`, `ED - Hypertension`, `ED - Obstetrics`,
      `ED - Mental Disorders`, `Infant mortality per 1000 livebirths`,
      `Mortality per 1000 population`
   )

census_data <- census_data %>% rename(ZIP = zip_code)

selected_dataset <- census_data %>%
   mutate_at(vars(Population, `% Rural`, `Percent female`,
                  `Percent Non-white`, `PCP Placement Score`, `OB Placement Score`,
                  `Percent age 0-19`, `Percent age 20-44`, `Percent age 45-64`,
                  `Percent age 65 years and over`, `Percent Women aged 15-50`,
                  `Percent < 30 minutes from PCP`, `Percent < 30 minutes from OB/GYN provider`,
                  `Percent < 30 minutes from a Hospital`, `Percent < 30 minutes from OB Hospital Unit`,
                  `Number of Hospitals`, `Number of Home Health Agencies`, `Number of Hospice`,
                  `Percent Labor force participation`, `Percent Unemployed`, `Percent Uninsured`,
                  `Percent <19 Uninsured`, `Median Income`, `Percent in Poverty`,
                  `Inpatient - Total`,
                  `Inpatient - Alzheimer's`, `Inpatient - Asthma`, `Inpatient - Congestive Heart Failure`,
                  `Inpatient - COPD`, `Inpatient - Depression`, `Inpatient - Type I Diabetes`,
                  `Inpatient - Type II Diabetes`, `Inpatient - Hepatitis C`,
                  `Inpatient - Hypertension`, `Inpatient - Obstetrics`, `Inpatient - Mental Disorders`,
                  `ED - Total`, `ED - Alzheimer’s`, `ED - Asthma`, `ED - Congestive Heart Failure`,
                  `ED - COPD`, `ED - Depression`, `ED - Type I Diabetes`,
                  `ED - Type II Diabetes`, `ED - Hepatitis C`, `ED - Hypertension`,
                  `ED - Obstetrics`, `ED - Mental Disorders`, `Infant mortality per 1000 livebirths`,
                  `Mortality per 1000 population`), as.numeric)

selected_dataset <- selected_dataset %>%
   mutate(
      Percent_male = 1 - `Percent female`,
      `Percent white` = 1 - `Percent Non-white`
   )

# Prisma Hospitalization Data  ----
hosp_data_prisma <- read_csv("/Users/tanvirahammed/Library/CloudStorage/Box-Box/BoxPHI-PHMR Projects/Iromi/EHR Data construction/PRISMA HEALTH/Prisma_Health_COVID_Patient_Data/Prisma_Health_COVID_Patient_Data_9_15.csv",
                             col_select = c("PAT_ID", "EncounterDate", "ZIP",
                                            "Week", "Is.Abnormal.",
                                            "Hospitalization", "ED.Visit", "Outpatient"))

hosp_data_prisma <- hosp_data_prisma[!is.na(hosp_data_prisma$Is.Abnormal.) & hosp_data_prisma$Is.Abnormal. != 0, ]

prisma <- hosp_data_prisma[format(hosp_data_prisma$EncounterDate, "%Y") %in% c("2020", "2021", "2022", "2023"), ]

ZIP_totals <- prisma %>%
   group_by(ZIP) %>%
   summarise(Count = n())

# Filter the data frame to select counties with a total count >= 50 (performs slightly better when it is 50 instead of 100)
selected_ZIPs <- ZIP_totals %>%
   filter(!is.na(ZIP) & Count >= 50)

prisma <- prisma %>%
   filter(ZIP %in% selected_ZIPs$ZIP)
unique(prisma$ZIP)

## Training and Testing data ----------
prisma$Week <- as.Date(prisma$Week)

training_data <- prisma %>% filter(EncounterDate <= "2023-07-02")

test_data <- prisma %>% filter(EncounterDate > "2023-07-02" & EncounterDate <= "2023-10-01")

##training_data ---------
training_grouped_data <- training_data %>%
   group_by(ZIP, Week) %>%
   summarize(
      occurrences = n(),
   )

all_combinations <- expand.grid(
   ZIP = unique(training_grouped_data$ZIP),
   Week = unique(training_grouped_data$Week)
)

training_grouped_data_filled <- left_join(all_combinations, training_grouped_data, by = c("ZIP", "Week"))

training_grouped_data_filled<- training_grouped_data_filled  %>%
   left_join(selected_dataset, by = "ZIP")

training_grouped_data_filled <- training_grouped_data_filled %>%
   mutate(occurrences = replace_na(occurrences, 0))

## test_data-----------
test_grouped_data <- test_data %>%
   group_by(ZIP, Week) %>%
   summarize(
      occurrences = n()
   )

all_combinations1 <- expand.grid(
   ZIP = unique(test_grouped_data$ZIP),
   Week = unique(test_grouped_data$Week)
)

test_grouped_data_filled <- left_join(all_combinations1, test_grouped_data, by = c("ZIP", "Week"))

test_grouped_data_filled<- test_grouped_data_filled  %>%
   left_join(selected_dataset, by = "ZIP")

test_grouped_data_filled <- test_grouped_data_filled %>%
   mutate(occurrences = replace_na(occurrences, 0))

#RFA data-----------
hosp_data_rfa <- read_csv("/Users/tanvirahammed/Library/CloudStorage/Box-Box/BoxPHI-PHMR Projects/Data/SC Health Records/RFA/Years 2019-2023/Clean Data/data_covid.csv",
                          col_select = c(ZIP, ADMS, COUNTY,
                                         DISYEAR, ADMYEAR, DISMTH, DISDAY, ADMMTH,
                                         ADMDAY, RFA_ID, ADMD_new, DISD_new))

##Remove duplicate------
hosp_data_rfa <- hosp_data_rfa %>%
   distinct(RFA_ID, ADMD_new, .keep_all = TRUE)

# Find rows where RFA_ID is similar and difference in ADMD_new is less than 2 weeks
hosp_data_rfa <- as.data.table(hosp_data_rfa)
hosp_data_rfa[, ADMD_new := as.Date(ADMD_new)]
hosp_data_rfa <- hosp_data_rfa[order(RFA_ID, ADMD_new)]
hosp_data_rfa[, admission_diff := ADMD_new - shift(ADMD_new), by = RFA_ID]
close_admissions <- hosp_data_rfa[admission_diff < 14 & !is.na(admission_diff)]

rows_to_remove <- hosp_data_rfa[close_admissions, on = .(RFA_ID, ADMD_new), which = TRUE]
hosp_data_rfa <- hosp_data_rfa[-rows_to_remove]

#Cleaning
RFA <- hosp_data_rfa %>%
   dplyr::select(RFA_ID, ZIP, ADMD_new)

RFA$Week <- floor_date(RFA$ADMD_new, unit = "week", week_start = 1)

## Training and Testing data ----------
RFA_training_data <- RFA %>%
   filter(ADMD_new >= "2020-03-02" & ADMD_new <= "2023-07-02")

RFA_test_data <- RFA %>%
   filter(ADMD_new >= "2023-07-03" & ADMD_new <= "2023-10-01")

## RFA_training_data-----------
RFA_training_grouped_data <- RFA_training_data %>%
   group_by(ZIP, Week) %>%
   summarize(
      occurrences = n()
   )

all_combinations2 <- expand.grid(
   ZIP = unique(RFA_training_grouped_data$ZIP),
   Week = unique(RFA_training_grouped_data$Week)
)

RFA_training_grouped_data <- left_join(all_combinations2, RFA_training_grouped_data, by = c("ZIP", "Week"))

RFA_training_grouped_data <- RFA_training_grouped_data %>%
   mutate(occurrences = replace_na(occurrences, 0))

## RFA_test_data-----------
RFA_test_grouped_data <- RFA_test_data %>%
   group_by(ZIP, Week) %>%
   summarize(
      occurrences = n()
   )

all_combinations3 <- expand.grid(
   ZIP = unique(RFA_test_grouped_data$ZIP),
   Week = unique(RFA_test_grouped_data$Week)
)

RFA_test_grouped_data <- left_join(all_combinations3, RFA_test_grouped_data, by = c("ZIP", "Week"))

RFA_test_grouped_data <- RFA_test_grouped_data %>%
   mutate(occurrences = replace_na(occurrences, 0))

# Final test and training data ---------

## Final training data----------
Final_training_data <- left_join(training_grouped_data_filled, RFA_training_grouped_data, by = c("ZIP", "Week"))

exclude_columns <- c("occurrences.x", "occurrences.y", "ZIP", "Week")

log_columns <- Final_training_data %>%
   dplyr::select(-all_of(exclude_columns)) %>%
   dplyr::select(where(~ is.numeric(.) && any(. >= 10))) %>%
   colnames()

Final_training_data <- Final_training_data %>%
   mutate(across(all_of(log_columns), ~ log(.), .names = "log_{col}")) %>%
   dplyr::select(-all_of(log_columns))

Final_training_data <- Final_training_data %>%
   rename(hospitalisation_RFA = occurrences.y, hospitalisation_prisma = occurrences.x)

#Removing NA
na_zip_codes <- Final_training_data %>%
   filter(is.na(hospitalisation_RFA)) %>%
   dplyr::select(ZIP) %>%
   distinct()

subset_na_zip <- Final_training_data %>%
   filter(ZIP %in% na_zip_codes$ZIP)

Final_training_data <- Final_training_data %>%
   filter(!ZIP %in% na_zip_codes$ZIP)

## Final test data----------
Final_test_data <- left_join(test_grouped_data_filled, RFA_test_grouped_data, by = c("ZIP", "Week"))

log_columns <- Final_test_data %>%
   dplyr::select(-all_of(exclude_columns)) %>%
   dplyr::select(where(~ is.numeric(.) && any(. >= 10))) %>%
   colnames()

Final_test_data <- Final_test_data %>%
   mutate(across(all_of(log_columns), ~ log(.), .names = "log_{col}")) %>%
   dplyr::select(-all_of(log_columns))

Final_test_data <- Final_test_data %>%
   rename(hospitalisation_RFA = occurrences.y, hospitalisation_prisma = occurrences.x)

#Removing NA
na_zip_codes <- Final_test_data %>%
   filter(is.na(hospitalisation_RFA)) %>%
   dplyr::select(ZIP) %>%
   distinct()

subset_na_zip <- Final_test_data %>%
   filter(ZIP %in% na_zip_codes$ZIP)

Final_test_data <- Final_test_data %>%
   filter(!ZIP %in% na_zip_codes$ZIP)

## =============================================================================
## PART B -- Model, TABLE 6 (ZIP half), FIGURE 4 (by_week_ZIP.R, verbatim)
## =============================================================================

#Model--------------------
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)

trControl <- trainControl(method = "cv", number = 5, search = "grid")
set.seed(1234)

sampled_data <- Final_training_data %>% sample_frac(0.9)

tuneGrid <- expand.grid(.mtry = c(1:4))

set.seed(1234)
rf_default <- train(hospitalisation_RFA ~ ZIP + Week + hospitalisation_prisma
                    ,
                    data = sampled_data, method = "rf", trControl = trControl, tuneGrid = tuneGrid, ntree = 300)

importance_rf <- varImp(rf_default)
plot(importance_rf)

best_mtry <- rf_default$bestTune$mtry

tuneGrid_final <- expand.grid(.mtry = best_mtry)

fit_rf1 <- train(hospitalisation_RFA ~ ZIP + Week + hospitalisation_prisma
                 ,
                 data = sampled_data, method = "rf",
                 tuneGrid = tuneGrid_final, trControl = trControl, ntree = 300)

predictions_counts <- predict(fit_rf1, Final_test_data)
Final_test_data$predicted_hospitalisation <- predictions_counts

stopCluster(cl)
registerDoSEQ()

total_hospitalizations <- Final_test_data %>%
   group_by(ZIP, Week) %>%
   summarise(
      total_observed_hospitalizations = sum(hospitalisation_RFA, na.rm = TRUE),
      total_predicted_hospitalizations = sum(predicted_hospitalisation, na.rm = TRUE)
   )

total_hospitalizations$Ai <-  pmin(total_hospitalizations$total_observed_hospitalizations, total_hospitalizations$total_predicted_hospitalizations) /
   pmax(total_hospitalizations$total_observed_hospitalizations, total_hospitalizations$total_predicted_hospitalizations)

total_hospitalizations_avg <- total_hospitalizations %>%
   group_by(ZIP) %>%
   summarise(
      Ai_avg = mean(Ai, na.rm = TRUE),
      median_observed_hospitalizations = mean(total_observed_hospitalizations, na.rm = TRUE),
      median_predicted_hospitalizations = mean(total_predicted_hospitalizations, na.rm = TRUE))

# ---- TABLE 6 (ZIP half, "sufficient Prisma coverage" row) --------------------
summary(total_hospitalizations_avg$Ai_avg)
unique(total_hospitalizations_avg$ZIP)
# ------------------------------------------------------------------------------

# Finding the ZIPs for imputation-------------------
unique_ZIP_data_before <- unique(RFA_test_grouped_data$ZIP)
unique_ZIP_data_after <- unique(Final_test_data$ZIP)

removed_ZIP <- setdiff(unique_ZIP_data_before, unique_ZIP_data_after)
removed_ZIP <- data.frame(ZIP = removed_ZIP)

removed_ZIP_data <- RFA %>% filter(ZIP %in% removed_ZIP$ZIP)

#Imputation----------------
removed_ZIP_data1 <- removed_ZIP_data %>%
   group_by(ZIP, Week) %>%
   summarize(
      total_observed_hospitalizations = n(),
   )

removed_ZIP_data1$total_predicted_hospitalizations<- NA

removed_ZIP_data1 <- removed_ZIP_data1 %>%
   filter(Week >= "2023-07-03" & Week <= "2023-09-25")

all_combinations_removed_ZIP_data1 <- expand.grid(
   ZIP = unique(removed_ZIP_data1$ZIP),
   Week = unique(removed_ZIP_data1$Week)
)

removed_ZIP_data1 <- left_join(all_combinations_removed_ZIP_data1, removed_ZIP_data1, by = c("ZIP", "Week"))

removed_ZIP_data1 <- removed_ZIP_data1 %>%
   mutate(total_observed_hospitalizations = replace_na(total_observed_hospitalizations, 0))

total_hospitalizations1 <- total_hospitalizations %>%
   dplyr::select(-Ai)

imputation_data <- bind_rows(removed_ZIP_data1, total_hospitalizations1)
imputation_data1 <- imputation_data

imputation_data$total_observed_hospitalizations <- NULL

## Merge the datasets based on the ZIP column
names(selected_dataset) <- gsub(" ", "_", names(selected_dataset))
names(selected_dataset) <- gsub("-", "_", names(selected_dataset))

imputation_data <- imputation_data  %>%
   left_join(selected_dataset %>% dplyr::select(ZIP, Population,
                                                Percent_male, Percent_white), by = "ZIP")

# Remove rows where log_population is NA
imputation_data <- imputation_data %>%
   filter(!is.na(Population))

imputation_data$log_population <- log(imputation_data$Population)
imputation_data$Population <- NULL

set.seed(1234)
mice_imputed <- data.frame(
   ZIP = imputation_data$ZIP,
   Week = imputation_data$Week,
   total_predicted_hospitalizations_before_imputation = imputation_data$total_predicted_hospitalizations,
   total_predicted_hospitalizations3 = complete(mice(imputation_data, method = "cart"))$total_predicted_hospitalizations
   )

mice_imputed <- mice_imputed %>%
   left_join(imputation_data1 %>%
                dplyr::select(ZIP, Week, total_observed_hospitalizations), by = c("ZIP", "Week"))

mice_imputed$Ai <-  pmin(mice_imputed$total_observed_hospitalizations, mice_imputed$total_predicted_hospitalizations3) /
   pmax(mice_imputed$total_observed_hospitalizations, mice_imputed$total_predicted_hospitalizations3)

mice_imputed_avg <- mice_imputed %>%
   group_by(ZIP) %>%
   summarise(
      Ai_avg = mean(Ai, na.rm = TRUE),
      median_observed_hospitalizations = mean(total_observed_hospitalizations, na.rm = TRUE),
      median_predicted_hospitalizations = mean(total_predicted_hospitalizations3, na.rm = TRUE))

# ---- TABLE 6 (ZIP half, "overall" row: sufficient + imputed combined) --------
summary(mice_imputed_avg$Ai_avg)
unique(mice_imputed_avg$ZIP)
# ------------------------------------------------------------------------------

######Accuracy for the imputed data--------------
mice_imputed_imp <- mice_imputed[is.na(mice_imputed$total_predicted_hospitalizations_before_imputation), ]

mice_imputed_avg_imp <- mice_imputed_imp %>%
   group_by(ZIP) %>%
   summarise(
      Ai_avg = mean(Ai, na.rm = TRUE),
      median_observed_hospitalizations = mean(total_observed_hospitalizations, na.rm = TRUE),
      median_predicted_hospitalizations = mean(total_predicted_hospitalizations3, na.rm = TRUE))

# ---- TABLE 6 (ZIP half, "insufficient Prisma coverage" row: imputed-only) ----
summary(mice_imputed_avg_imp$Ai_avg)
unique(mice_imputed_avg_imp$ZIP)
# ------------------------------------------------------------------------------

## Map of hospitalization observed/estimates/accuracy in all 352 ZIP codes ------------------------
sc_zcta_sf = tigris::zctas(state = "SC", class = "sf", year = '2010')

hosp_ZIP_esti_impu <- data.frame(ZIP = as.character(mice_imputed_avg$ZIP),
                                 Observed = mice_imputed_avg$median_observed_hospitalizations,
                                 Estimated = mice_imputed_avg$median_predicted_hospitalizations,
                                 Accuracy = mice_imputed_avg$Ai_avg)

sc_zcta_sf = left_join(sc_zcta_sf, hosp_ZIP_esti_impu
                       %>% dplyr::select(Observed, Estimated, Accuracy, ZCTA5CE10 = ZIP), by = 'ZCTA5CE10')

###observed------------
sc_zcta_sf = mutate(sc_zcta_sf,
                    Observed_grouped = case_when(Observed < 5 ~ '1',
                                                 Observed >= 5 & Observed <10 ~ '2',
                                                 Observed >= 10 & Observed <20 ~ '3',
                                                 Observed >= 20 & Observed <30 ~ '4',
                                                 Observed >= 30 & Observed <50 ~ '5',
                                                 Observed >= 50 ~ '6'))

sc_zcta_sf$Observed_grouped <- factor(sc_zcta_sf$Observed_grouped,
                                      levels = c("6", "5", "4", "3", "2", "1"),
                                      labels = c("50 and above", "30 - 50", "20 - 30", "10 - 20", "5 - 10", "0 - 5"))

ggplot(sc_zcta_sf) +
   geom_sf(aes(fill = Observed_grouped), color = 'black') +
   scale_fill_manual(values = c('#360034', '#4d004b', '#810f7c', '#8c96c6',  '#bfd3e6', '#d0d1e6'),
                     name = "Hospitalizations",
                     na.translate = TRUE,
                     na.value = 'white',
                     labels = c("50 and above", "30 - 50", "20 - 30", "10 - 20", "5 - 10", "0 - 5", "No Data")) +
   ggtitle("Average weekly observed hospitalization counts in 317 ZIP codes based on SC RFA data") +
   theme_void() +
   theme(
      plot.title = element_text(hjust = 0.5, size = 25),
      legend.justification = c(0.1, 0.1),
      legend.position = c(0.1, 0.1),
      legend.title = element_text(size = 28),
      legend.key.size = unit(1, 'cm'),
      legend.text = element_text(size = 28)
   )

###estimates-----------
sc_zcta_sf = mutate(sc_zcta_sf,
                    Estimated_grouped = case_when(Estimated < 5 ~ '1',
                                                  Estimated >= 5 & Estimated <10 ~ '2',
                                                  Estimated >= 10 & Estimated <20 ~ '3',
                                                  Estimated >= 20 & Estimated <30 ~ '4',
                                                  Estimated >= 30 & Estimated <50 ~ '5',
                                                  Estimated >= 50 ~ '6'))

sc_zcta_sf$Estimated_grouped <- factor(sc_zcta_sf$Estimated_grouped,
                                       levels = c("6", "5", "4", "3", "2", "1"),
                                       labels = c("50 and above", "30 - 50", "20 - 30", "10 - 20", "5 - 10", "0 - 5"))

ggplot(sc_zcta_sf) +
   geom_sf(aes(fill = Estimated_grouped), color = 'black') +
   scale_fill_manual(values = c('#360034', '#4d004b', '#810f7c',  '#8c96c6',  '#bfd3e6', '#d0d1e6'),
                     name = "Hospitalizations",
                     na.translate = TRUE,
                     na.value = 'white',
                     labels = c("50 and above", "30 - 50", "20 - 30", "10 - 20", "5 - 10", "0 - 5", "No Data")) +
   ggtitle("Average weekly estimated hospitalization counts in 317 ZIP codes") +
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
                    Accuracy_grouped = case_when(Accuracy >= .20 & Accuracy <.40 ~ '1',
                                                 Accuracy >= .40 & Accuracy <.50 ~ '2',
                                                 Accuracy >= .50 & Accuracy <.60 ~ '3',
                                                 Accuracy >= .60 & Accuracy <.70 ~ '4',
                                                 Accuracy >= .70 & Accuracy <.80 ~ '5',
                                                 Accuracy >= .80 & Accuracy <1.00 ~ '6'))

sc_zcta_sf$Accuracy_grouped <- factor(sc_zcta_sf$Accuracy_grouped,
                                      levels = c("6", "5", "4", "3", "2", "1"),
                                      labels = c("80%-100%", "70%-80%", "60%-70%", "50%-60%", "40%-50%", "20%-40%"))

ggplot(sc_zcta_sf) +
   geom_sf(aes(fill = Accuracy_grouped), color = 'gray0') +
   scale_fill_manual(values = c('#016c59', '#1c9099','#d9ef8b', '#ffffcc', '#fdd49e', '#fc9272'),
                     name = "Accuracy",
                     na.translate = TRUE,
                     na.value = 'white',
                     labels = c("80%-100%", "70%-80%", "60%-70%", "50%-60%", "40%-50%", "20%-40%", "No Data")) +
   ggtitle("Weekly median average percent agreement accuracy of hospitalization estimates in 317 ZIP codes") +
   theme_void() +
   theme(
      plot.title = element_text(hjust = 0.5, size = 25),
      legend.justification = c(0.1, 0.1),
      legend.position = c(0.1, 0.1),
      legend.title = element_text(size = 28),
      legend.key.size = unit(1, 'cm'),
      legend.text = element_text(size = 28)
   )

# Map of observed hospitalization based on Prisma data ------------------------
Final_test_data_p <- Final_test_data %>%
   group_by(ZIP) %>%
   summarize(
      hospitalisation_prisma = mean(hospitalisation_prisma),
   )

sc_zcta_sf = tigris::zctas(state = "SC", class = "sf", year = '2010')

hosp_ZIP_esti_impu <- data.frame(ZIP = as.character(Final_test_data_p$ZIP),
                                 Observed = Final_test_data_p$hospitalisation_prisma)

sc_zcta_sf = left_join(sc_zcta_sf, hosp_ZIP_esti_impu
                       %>% dplyr::select(Observed, ZCTA5CE10 = ZIP), by = 'ZCTA5CE10')

###observed------------
sc_zcta_sf = mutate(sc_zcta_sf,
                    Observed_grouped = case_when(Observed < 1 ~ '1',
                                                 Observed >= 1 & Observed <3 ~ '2',
                                                 Observed >= 3 & Observed <5 ~ '3',
                                                 Observed >= 5 & Observed <10 ~ '4',
                                                 Observed >= 10 & Observed <20 ~ '5'))

sc_zcta_sf$Observed_grouped <- factor(sc_zcta_sf$Observed_grouped,
                                      levels = c("5", "4", "3", "2", "1"),
                                      labels = c("10 - 20", "5 - 10", "3 - 5", "1 - 3", "0"))

ggplot(sc_zcta_sf) +
   geom_sf(aes(fill = Observed_grouped), color = 'gray20') +
   scale_fill_manual(values = c('#4d004b', '#810f7c', '#8c96c6',  '#bfd3e6', '#d0d1e6'),
                     name = "Hospitalizations",
                     na.translate = TRUE,
                     na.value = 'white',
                     labels = c("10 - 20", "5 - 10", "3 - 5", "1 - 3", "0", "No Data")) +
   ggtitle("Average weekly observed hospitalization in 123 ZIP codes based on Prisma Health data") +
   theme_void() +
   theme(
      plot.title = element_text(hjust = 0.5, size = 25),
      legend.justification = c(0.1, 0.1),
      legend.position = c(0.1, 0.1),
      legend.title = element_text(size = 28),
      legend.key.size = unit(1, 'cm'),
      legend.text = element_text(size = 28)
   )
