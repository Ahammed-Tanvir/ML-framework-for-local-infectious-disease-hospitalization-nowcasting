# Short heading: ML-framework-for-local-infectious-disease-hospitalization-nowcasting

# Machine Learning Approaches for Real-Time ZIP Code and County-Level Estimation of State-Wide Infectious Disease Hospitalizations Using Local Health System Data

Reproduction code for:

> Ahammed T, Hossain MS, McMahan C, Rennert L. *Machine learning approaches for
> real-time ZIP code and county-level estimation of state-wide infectious
> disease hospitalizations using local health system data.* Epidemics.
> 2025;51:100823. https://doi.org/10.1016/j.epidem.2025.100823

This repo estimates and nowcasts COVID-19 hospitalizations at the ZIP-code and
county level in South Carolina, using Prisma Health EHR data plus SC Office of
Revenue and Fiscal Affairs (RFA) claims data, via negative binomial mixed
models and Random Forest, with missForest/MICE-based imputation for
under-covered geographies.

## Data availability

**No data is included in this repository.** Prisma Health EHR records and SC
RFA hospitalization claims are protected health information under a data use
agreement and cannot be shared publicly. Every script expects local file
paths for:

- Prisma Health COVID patient extract
- SC RFA hospitalization claims 
- SC Census/ZCTA demographic files (age, sex, race by ZCTA)
- ZIP-to-county crosswalk 
- County-level sociodemographic file
- 
Update the hardcoded paths at the top of each script before running.

## Requirements

R (tested with 4.x) and the following packages: `dplyr`, `tidyr`, `stringr`,
`readr`, `readxl`, `openxlsx`, `lme4`, `MASS`, `caret`, `randomForest`,
`missForest`, `mice`, `sf`, `tigris`, `ggplot2`, `data.table`, `doParallel`,
`lubridate`, `scales`.

## Pipeline / run order

Each numbered step is a separate R session **except** where noted — those
share in-memory objects and must be run back-to-back without clearing the
environment.

| Step | Script | Produces | Session |
|---|---|---|---|
| 1 | `step1_ZIP_6mo_estimate_and_impute_Table1_3_5_Fig1_2.R` | Table 1, Table 3 (ZIP half), Table 5 ZIP row "RF Model 2", Fig 1 (ZIP map), Fig 2 | new |
| 2 | `step2_ZIP_Table4_5_NBmodel2_basis_RUN_AFTER_STEP1.R` | Table 4 & 5 ZIP row "NB Model 2" | **same session as step 1** |
| 3 | `step3_County_FULL_construction_plus_analysis.R` | Table 2, Table 3 (county half), Fig 1 (4-county time series), Fig 3 | new |
| 4 | `step4_County_Table4_5_RFmodel1_basis_RUN_AFTER_STEP3.R` | Table 4 & 5 county row "RF Model 1" | **same session as step 3** |
| 5 | `step5_ZIP_weekly_Table6_Fig4.R` | Table 6 (ZIP half), Fig 4 | new |
| 6 | `step6_County_weekly_Table6_Fig5.R` | Table 6 (county half), Fig 5 | new |



## Citation

If you use this code, please cite the original paper (above).
