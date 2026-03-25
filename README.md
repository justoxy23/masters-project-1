# Mapping the Wait: Regional, Temporal and Socioeconomic Predictors of GP Appointment Delays in the NHS (2022–2024)

This repository contains the R code used for my MSc Health Informatics dissertation project, which explored patterns and predictors of GP appointment delays in England using national NHS datasets.

The project focused on understanding how appointment delays varied over time and across regions, and whether factors such as deprivation, geography, and GP workforce capacity were associated with longer waits.

## Project overview

The analysis combines multiple public NHS and geographic datasets to:

- examine national trends in GP appointment delays over time
- compare the distribution of long waits across regions in England
- integrate workforce, deprivation, and geographic information at practice level
- model factors associated with longer appointment waits using a negative binomial generalised linear mixed model

## Main analysis steps

The workflow in this repository includes:

1. **Loading and combining monthly NHS appointment data**
   - Merging multiple CSV files into a single dataset
   - Formatting dates and aggregating appointment counts by delay category

2. **Exploratory trend analysis**
   - Plotting monthly appointment trends
   - Calculating percentage change over time

3. **Practice-level data integration**
   - Combining appointment data with postcode, LSOA, deprivation, and workforce datasets
   - Cleaning missing values and preparing analysis-ready data

4. **Feature engineering**
   - Creating derived variables such as:
     - long waits
     - GPs per 1,000 patients
     - IMD quantiles
     - time trend and seasonal indicators
     - urban/rural classification

5. **Statistical modelling**
   - Fitting a negative binomial GLMM using `glmmTMB`
   - Including an offset for total appointments and a random intercept for ICB

6. **Visualisation**
   - Time series plots of appointment delays
   - Predicted long waits by NHS region
   - Spatial mapping of long waits across Integrated Care Boards

## Key packages used

- `dplyr`
- `data.table`
- `ggplot2`
- `readr`
- `stringr`
- `lubridate`
- `glmmTMB`
- `sf`
- `tmap`
- `RColorBrewer`
- `marginaleffects`

## Model summary

The main model used in this project was a **negative binomial generalised linear mixed model**, chosen because the outcome was count-based and overdispersed.

The model was used to assess whether long appointment waits were associated with:

- NHS region
- deprivation
- urban/rural classification
- GP workforce per 1,000 patients
- time trend
- seasonality

A random intercept for ICB was included to account for regional clustering.

## Notes on data

This repository contains the analysis code only.

The original datasets were drawn from public NHS and geographic sources and were merged and processed locally for the dissertation. File paths and filenames in the script reflect the original project working environment, so they may need to be adapted before rerunning the analysis.

## Outputs

The code produces:

- exploratory time-series plots
- summary tables
- model outputs and exponentiated coefficients
- marginal predictions
- choropleth maps comparing long waits across regions over time

## Future improvements

Potential extensions to this work include:
- modelling rates rather than raw counts in regional maps
- incorporating more granular patient- or practice-level contextual variables
- exploring alternative temporal or hierarchical modelling approaches

## Disclaimer

This repository is intended to demonstrate the analytical workflow used in the dissertation project. It is not intended as a production-ready package.

## Author

Terenia Lee Leh Yi  
MSc Bioinformatics and Systems Biology  
University of Manchester
