################################################################################
# County level: WEEKLY nowcasting
# Produces: TABLE 6 (county half: sufficient / insufficient / overall)
#           FIGURE 5 (weekly observed/estimated/accuracy maps, panels a-d)
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

#Demographic variables----------------
census_data <- read_csv("/Users/tanvirahammed/Library/CloudStorage/Box-Box/BoxPHI-PHMR Projects/Data/Community Data/SC Census Tract ZIP Code Data/CRPH_ZIP_new.csv")

census_data <- census_data %>%
   dplyr::select(
      zip_code, county,Population, `% Rural`, `Percent female`, `Percent Non-white`,
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

prisma <- prisma  %>%
   left_join(selected_dataset, by = "ZIP")

county_totals <- prisma %>%
   group_by(county) %>%
   summarise(Count = n())

# Filter the data frame to select counties with a total count >= 50 (performs slightly better when it is 50 instead of 100)
selected_countys <- county_totals %>%
   filter(!is.na(county) & Count >= 50)

prisma <- prisma %>%
   filter(county %in% selected_countys$county)
unique(prisma$county)

## Training and Testing data ----------
prisma$Week <- as.Date(prisma$Week)

training_data <- prisma %>% filter(EncounterDate <= "2023-07-02")

test_data <- prisma %>% filter(EncounterDate > "2023-07-02" & EncounterDate <= "2023-10-01")

##training_data ---------
training_grouped_data <- training_data %>%
   group_by(county, Week) %>%
   summarize(
      occurrences = n(),
   )

all_combinations <- expand.grid(
   county = unique(training_grouped_data$county),
   Week = unique(training_grouped_data$Week)
)

training_grouped_data_filled <- left_join(all_combinations, training_grouped_data, by = c("county", "Week"))

training_grouped_data_filled <- training_grouped_data_filled %>%
   mutate(occurrences = replace_na(occurrences, 0))

census_data_county <- read_csv("/Users/tanvirahammed/Library/CloudStorage/Box-Box/BoxPHI-PHMR Projects/Data/Community Data/SC County Data/SC_county_sociodemographic.csv")
census_data_county <- census_data_county %>% rename(county = County)

add_County_if_missing <- function(county_name) {
   if (!grepl(" County$", county_name)) {
      return(paste(county_name, "County"))
   }
   return(county_name)
}
census_data_county$county <- sapply(census_data_county$county, add_County_if_missing)

names(census_data_county) <- gsub(" ", "_", names(census_data_county))

census_data_county <- census_data_county %>%
   mutate(
      Percent_Male = (Male / Total_population),
      Percent_Female = (Female / Total_population) ,
      Percent_White = (White_Not_Hispanic / Total_population),
      Percent_Non_White = ((Total_population - White_Not_Hispanic) / Total_population)
   )

## Merge the datasets based on the county column
training_grouped_data_filled<- training_grouped_data_filled  %>%
   left_join(census_data_county, by = "county")

## test_data-----------
test_grouped_data <- test_data %>%
   group_by(county, Week) %>%
   summarize(
      occurrences = n()
   )

all_combinations1 <- expand.grid(
   county = unique(test_grouped_data$county),
   Week = unique(test_grouped_data$Week)
)

test_grouped_data_filled <- left_join(all_combinations1, test_grouped_data, by = c("county", "Week"))

test_grouped_data_filled <- test_grouped_data_filled %>%
   mutate(occurrences = replace_na(occurrences, 0))

## Merge the datasets based on the county column
test_grouped_data_filled<- test_grouped_data_filled  %>%
   left_join(census_data_county, by = "county")

#RFA data-----------
hosp_data_rfa <- read_csv("/Users/tanvirahammed/Library/CloudStorage/Box-Box/BoxPHI-PHMR Projects/Data/SC Health Records/RFA/Years 2019-2023/Clean Data/data_covid.csv",
                          col_select = c(ZIP, ADMS, COUNTY,
                                         DISYEAR, ADMYEAR, DISMTH, DISDAY, ADMMTH,
                                         ADMDAY, RFA_ID, ADMD_new, DISD_new))

hosp_data_rfa <- hosp_data_rfa %>% rename(county = COUNTY)

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
   dplyr::select(RFA_ID, ZIP, county, ADMD_new)

RFA$Week <- floor_date(RFA$ADMD_new, unit = "week", week_start = 1)

county_mapping <- c(
   "1" = "Abbeville County", "2" = "Aiken County", "3" = "Allendale County", "4" = "Anderson County", "5" = "Bamberg County",
   "6" = "Barnwell County", "7" = "Beaufort County", "8" = "Berkeley County", "9" = "Calhoun County", "10" = "Charleston County",
   "11" = "Cherokee County", "12" = "Chester County", "13" = "Chesterfield County", "14" = "Clarendon County", "15" = "Colleton County",
   "16" = "Darlington County", "17" = "Dillon County", "18" = "Dorchester County", "19" = "Edgefield County", "20" = "Fairfield County",
   "21" = "Florence County", "22" = "Georgetown County", "23" = "Greenville County", "24" = "Greenwood County", "25" = "Hampton County",
   "26" = "Horry County", "27" = "Jasper County", "28" = "Kershaw County", "29" = "Lancaster County", "30" = "Laurens County",
   "31" = "Lee County", "32" = "Lexington County", "33" = "McCormick County", "34" = "Marion County", "35" = "Marlboro County",
   "36" = "Newberry County", "37" = "Oconee County", "38" = "Orangeburg County", "39" = "Pickens County", "40" = "Richland County",
   "41" = "Saluda County", "42" = "Spartanburg County", "43" = "Sumter County", "44" = "Union County", "45" = "Williamsburg County", "46" = "York County"
)

RFA$county <- county_mapping[as.character(RFA$county)]

## Training and Testing data ----------
RFA_training_data <- RFA %>%
   filter(ADMD_new >= "2020-03-02" & ADMD_new <= "2023-07-02")

RFA_test_data <- RFA %>%
   filter(ADMD_new >= "2023-07-03" & ADMD_new <= "2023-10-01")

## RFA_training_data-----------
RFA_training_grouped_data <- RFA_training_data %>%
   group_by(county, Week) %>%
   summarize(
      occurrences = n()
   )

all_combinations2 <- expand.grid(
   county = unique(RFA_training_grouped_data$county),
   Week = unique(RFA_training_grouped_data$Week)
)

RFA_training_grouped_data <- left_join(all_combinations2, RFA_training_grouped_data, by = c("county", "Week"))

RFA_training_grouped_data <- RFA_training_grouped_data %>%
   mutate(occurrences = replace_na(occurrences, 0))

## RFA_test_data-----------
RFA_test_grouped_data <- RFA_test_data %>%
   group_by(county, Week) %>%
   summarize(
      occurrences = n()
   )

all_combinations3 <- expand.grid(
   county = unique(RFA_test_grouped_data$county),
   Week = unique(RFA_test_grouped_data$Week)
)

RFA_test_grouped_data <- left_join(all_combinations3, RFA_test_grouped_data, by = c("county", "Week"))

RFA_test_grouped_data <- RFA_test_grouped_data %>%
   mutate(occurrences = replace_na(occurrences, 0))

# Final test and training data ---------

## Final training data----------
Final_training_data <- left_join(training_grouped_data_filled, RFA_training_grouped_data, by = c("county", "Week"))

Final_training_data <- Final_training_data %>%
   rename(hospitalisation_RFA = occurrences.y, hospitalisation_prisma = occurrences.x)
Final_training_data$log_Total_population <- log(Final_training_data$Total_population)



## Final test data----------
Final_test_data <- left_join(test_grouped_data_filled, RFA_test_grouped_data, by = c("county", "Week"))

Final_test_data <- Final_test_data %>%
   rename(hospitalisation_RFA = occurrences.y, hospitalisation_prisma = occurrences.x)
Final_test_data$log_Total_population <- log(Final_test_data$Total_population)



#Model--------------------
cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)

trControl <- trainControl(method = "cv", number = 5, search = "grid")

set.seed(1234)
sampled_data <- Final_training_data %>% sample_frac(0.9)

tuneGrid <- expand.grid(.mtry = c(8:13))

set.seed(1234)
rf_default <- train(hospitalisation_RFA ~ county + Week + hospitalisation_prisma
                    + log_Total_population,
                    data = sampled_data, method = "rf", trControl = trControl, tuneGrid = tuneGrid, ntree = 300)

print(rf_default)
importance_rf <- varImp(rf_default)
plot(importance_rf)

best_mtry <- rf_default$bestTune$mtry

tuneGrid_final <- expand.grid(.mtry = best_mtry)

set.seed(1234)
fit_rf1 <- train(hospitalisation_RFA ~ county + Week + hospitalisation_prisma
                 + log_Total_population,
                 data = sampled_data, method = "rf",
                 tuneGrid = tuneGrid_final, trControl = trControl, ntree = 300)

predictions_counts <- predict(fit_rf1, Final_test_data)
Final_test_data$predicted_hospitalisation <- predictions_counts

stopCluster(cl)
registerDoSEQ()

#Week-------------------
total_hospitalizations <- Final_test_data %>%
   group_by(county, Week) %>%
   summarise(
      total_observed_hospitalizations = sum(hospitalisation_RFA, na.rm = TRUE),
      total_predicted_hospitalizations = sum(predicted_hospitalisation, na.rm = TRUE)
   )

total_hospitalizations$Ai <-  pmin(total_hospitalizations$total_observed_hospitalizations, total_hospitalizations$total_predicted_hospitalizations) /
   pmax(total_hospitalizations$total_observed_hospitalizations, total_hospitalizations$total_predicted_hospitalizations)

total_hospitalizations_avg <- total_hospitalizations %>%
   group_by(county) %>%
   summarise(
      Ai_avg = mean(Ai, na.rm = TRUE),
      median_observed_hospitalizations = mean(total_observed_hospitalizations, na.rm = TRUE),
      median_predicted_hospitalizations = mean(total_predicted_hospitalizations, na.rm = TRUE))

# ---- TABLE 6 (county half, "sufficient Prisma coverage" row) -----------------
summary(total_hospitalizations_avg$Ai_avg)
unique(total_hospitalizations_avg$county)
# ------------------------------------------------------------------------------

# Finding the ZIPs for imputation-------------------
unique_county_data_before <- unique(RFA_test_grouped_data$county)
unique_county_data_after <- unique(Final_test_data$county)

removed_county <- setdiff(unique_county_data_before, unique_county_data_after)
removed_county <- data.frame(county = removed_county)

removed_county_data <- RFA %>% filter(county %in% removed_county$county)
removed_county_data <- removed_county_data[!is.na(removed_county_data$county), ]

#Imputation----------------
removed_county_data1 <- removed_county_data %>%
   group_by(county, Week) %>%
   summarize(
      total_observed_hospitalizations = n(),
   )

removed_county_data1$total_predicted_hospitalizations<- NA

removed_county_data1 <- removed_county_data1 %>%
   filter(Week >= "2023-07-03" & Week <= "2023-09-25")

total_hospitalizations1 <- total_hospitalizations %>%
   dplyr::select(-Ai)

imputation_data <- bind_rows(removed_county_data1, total_hospitalizations1)
imputation_data$county <- as.factor(imputation_data$county)
imputation_data1 <- imputation_data

imputation_data$total_observed_hospitalizations <- NULL

## Merge the datasets based on the county column
imputation_data <- imputation_data %>%
   left_join(census_data_county %>% dplyr::select(county, Total_population, Percent_0_19,
                                                  Percent_20_44,Percent_45_64,
                                                  Percent_Male, Percent_White), by = "county")

imputation_data$log_Total_population <- log(imputation_data$Total_population)
imputation_data$Total_population <- NULL

set.seed(1234)
mice_imputed <- data.frame(
   county = imputation_data$county,
   Week = imputation_data$Week,
   total_predicted_hospitalizations_before_imputation = imputation_data$total_predicted_hospitalizations,
   total_predicted_hospitalizations3 = complete(mice(imputation_data, method = "cart"))$total_predicted_hospitalizations
)

mice_imputed <- mice_imputed %>%
   left_join(imputation_data1 %>%
                dplyr::select(county, Week, total_observed_hospitalizations), by = c("county", "Week"))

mice_imputed$Ai <-  pmin(mice_imputed$total_observed_hospitalizations, mice_imputed$total_predicted_hospitalizations3) /
   pmax(mice_imputed$total_observed_hospitalizations, mice_imputed$total_predicted_hospitalizations3)

mice_imputed_avg <- mice_imputed %>%
   group_by(county) %>%
   summarise(
      Ai_avg = mean(Ai, na.rm = TRUE),
      median_observed_hospitalizations = mean(total_observed_hospitalizations, na.rm = TRUE),
      median_predicted_hospitalizations = mean(total_predicted_hospitalizations3, na.rm = TRUE))

# ---- TABLE 6 (county half, "overall" row: sufficient + imputed combined) ----
summary(mice_imputed_avg$Ai_avg)
unique(mice_imputed_avg$county)
# ------------------------------------------------------------------------------

######Accuracy for the imputed data--------------
mice_imputed_imp <- mice_imputed[is.na(mice_imputed$total_predicted_hospitalizations_before_imputation), ]

mice_imputed_avg_imp <- mice_imputed_imp %>%
   group_by(county) %>%
   summarise(
      Ai_avg = mean(Ai, na.rm = TRUE),
      median_observed_hospitalizations = mean(total_observed_hospitalizations, na.rm = TRUE),
      median_predicted_hospitalizations = mean(total_predicted_hospitalizations3, na.rm = TRUE))

# ---- TABLE 6 (county half, "insufficient Prisma coverage" row: imputed-only) --
summary(mice_imputed_avg_imp$Ai_avg)
unique(mice_imputed_avg_imp$county)
# ------------------------------------------------------------------------------

## Map of hospitalization observed/estimates/accuracy in all 352 county codes ------------------------
sc_counties <- tigris::counties(state = "SC", year = 2021, class = "sf")

hosp_county_esti_impu <- data.frame(county = as.character(mice_imputed_avg$county),
                                 Observed = mice_imputed_avg$median_observed_hospitalizations,
                                 Estimated = mice_imputed_avg$median_predicted_hospitalizations,
                                 Accuracy = mice_imputed_avg$Ai_avg)

sc_counties = left_join(sc_counties, hosp_county_esti_impu
                        %>% dplyr::select(Observed, Estimated, Accuracy, NAMELSAD = county), by = 'NAMELSAD')

###observed------------
sc_counties = mutate(sc_counties,
                     Observed_grouped = case_when(Observed < 50 ~ '1',
                                                  Observed >= 50 & Observed <100 ~ '2',
                                                  Observed >= 100 & Observed <200 ~ '3',
                                                  Observed >= 200 & Observed <500 ~ '4',
                                                  Observed >= 500 & Observed <30000 ~ '5'))

sc_counties$Observed_grouped <- factor(sc_counties$Observed_grouped,
                                       levels = c("5", "4", "3", "2", "1"),
                                       labels = c("500 and above", "200 - 500", "100 - 200", "50 - 100", "0 - 50"))

ggplot(sc_counties) +
   geom_sf(aes(fill = Observed_grouped), color = 'gray20') +
   scale_fill_manual(values = c('#4d004b', '#810f7c', '#8c96c6',  '#bfd3e6', '#d0d1e6'),
                     name = "Hospitalizations",
                     na.translate = TRUE,
                     na.value = 'white',
                     labels = c("500 and above", "200 - 500", "100 - 200", "50 - 100", "0 - 50", "No Data")) +
   ggtitle("Average weekly observed hospitalizations in all 46 counties based on SC RFA data") +
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
                     Estimated_grouped = case_when(Estimated < 50 ~ '1',
                                                   Estimated >= 50 & Estimated <100 ~ '2',
                                                   Estimated >= 100 & Estimated <200 ~ '3',
                                                   Estimated >= 200 & Estimated <500 ~ '4',
                                                   Estimated >= 500 & Estimated <30000 ~ '5'))

sc_counties$Estimated_grouped <- factor(sc_counties$Estimated_grouped,
                                        levels = c("5", "4", "3", "2", "1"),
                                        labels = c("500 and above", "200 - 500", "100 - 200", "50 - 100", "0 - 50"))

ggplot(sc_counties) +
   geom_sf(aes(fill = Estimated_grouped), color = 'gray20') +
   scale_fill_manual(values = c('#4d004b', '#810f7c', '#8c96c6', '#bfd3e6', '#d0d1e6'),
                     name = "Hospitalizations",
                     na.translate = TRUE,
                     na.value = 'white',
                     labels = c("500 and above", "200 - 500", "100 - 200", "50 - 100", "0 - 50", "No Data")) +
   ggtitle("Average weekly estimated hospitalizations in all 46 counties") +
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
                     Accuracy_grouped = case_when(Accuracy >= .40 & Accuracy <.50 ~ '1',
                                                  Accuracy >= .50 & Accuracy <.60 ~ '2',
                                                  Accuracy >= .60 & Accuracy <.70 ~ '3',
                                                  Accuracy >= .70 & Accuracy <.80 ~ '4',
                                                  Accuracy >= .80 & Accuracy <1.00 ~ '5'))

sc_counties$Accuracy_grouped <- factor(sc_counties$Accuracy_grouped,
                                       levels = c("5", "4", "3", "2", "1"),
                                       labels = c("80%-100%", "70%-80%", "60%-70%", "50%-60%", "40%-50%"))

ggplot(sc_counties) +
   geom_sf(aes(fill = Accuracy_grouped), color = 'gray0') +
   scale_fill_manual(values = c('#016c59', '#1c9099','#d9ef8b', '#ffffcc', '#fdd49e'),
                     name = "Accuracy",
                     na.translate = TRUE,
                     na.value = 'white',
                     labels = c("80%-100%", "70%-80%", "60%-70%", "50%-60%", "40%-50%", "No Data")) +
   ggtitle("Weekly median average percent agreement accuracy of observed and estimated hospitalizations") +
   theme_void() +
   theme(
      plot.title = element_text(hjust = 0.5, size = 25),
      legend.justification = c(0.1, 0.1),
      legend.position = c(0.1, 0.1),
      legend.title = element_text(size = 28),
      legend.key.size = unit(1, 'cm'),
      legend.text = element_text(size = 26)
   )

# Map of observed hospitalization based on Prisma data ------------------------
Final_test_data_p <- Final_test_data %>%
   group_by(county) %>%
   summarize(
      hospitalisation_prisma = mean(hospitalisation_prisma),
   )

sc_counties <- tigris::counties(state = "SC", year = 2021, class = "sf")

hosp_county_esti_impu <- data.frame(county = as.character(Final_test_data_p$county),
                                 Observed = Final_test_data_p$hospitalisation_prisma)

sc_counties = left_join(sc_counties, hosp_county_esti_impu
                        %>% dplyr::select(Observed, NAMELSAD = county), by = 'NAMELSAD')

###observed------------
sc_counties = mutate(sc_counties,
                     Observed_grouped = case_when(Observed < 3 ~ '1',
                                                  Observed >= 3 & Observed < 5 ~ '2',
                                                  Observed >= 5 & Observed < 20 ~ '3',
                                                  Observed >= 20 & Observed < 40 ~ '4',
                                                  Observed >= 40 & Observed < 200 ~ '5'))

sc_counties$Observed_grouped <- factor(sc_counties$Observed_grouped,
                                       levels = c( "5", "4", "3", "2", "1"),
                                       labels = c( "40 and above", "20 - 40", "5 - 20", "3 - 5", "0 - 3"))

ggplot(sc_counties) +
   geom_sf(aes(fill = Observed_grouped), color = 'gray20') +
   scale_fill_manual(values = c( '#4d004b', '#810f7c', '#8c96c6',  '#bfd3e6', '#d0d1e6'),
                     name = "Hospitalizations",
                     na.translate = TRUE,
                     na.value = 'white',
                     labels = c("30 and above", "15 - 30", "5 - 15", "3 - 5", "0 - 3", "No Data")) +
   ggtitle("Average weekly observed hospitalizations in 28 counties based on Prisma Health data") +
   theme_void() +
   theme(
      plot.title = element_text(hjust = 0.5, size = 25),
      legend.justification = c(0.1, 0.1),
      legend.position = c(0.1, 0.1),
      legend.title = element_text(size = 28),
      legend.key.size = unit(1, 'cm'),
      legend.text = element_text(size = 28)
   )
