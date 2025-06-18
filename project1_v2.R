# ============================
# 📂 1. Load Required Libraries
# ============================
library(dplyr)
library(ggplot2)
library(data.table)
library(zoo)
library(stringr)
library(readr)

library(glmmTMB)

library(tmap)

# ===========================================
# 📂 2. Load & Combine Multiple CSV Files
# ===========================================
# Get a list of all CSV files in the directory
file_list <- list.files(pattern = "SUB_ICB_LOCATION_CSV_.*\\.csv")

# Read all CSV files into a list
data_list <- lapply(file_list, read.csv)

# combine all data frames into one 
combined_data <- do.call(rbind, data_list)

# Checking for duplicates (no duplicates)
sum(duplicated(combined_data))

# ===========================================
# 📂 3. Aggregate Monthly Data for Plotting
# ===========================================
# Format date
# Load dataset (assuming it's already read as combined_data)
setDT(combined_data)  # Convert to data.table for efficiency

# ✅ Fix Date Format
combined_data[, Appointment_Date := as.Date(Appointment_Date, format="%d%b%Y")]

# ✅ Create Month Column (YYYY-MM format for grouping)
combined_data[, Month := format(Appointment_Date, "%Y-%m")]

# ✅ Exclude "Unknown" and "Data quality" before aggregation
combined_data <- combined_data[!(TIME_BETWEEN_BOOK_AND_APPT %in% c("Unknown / Data Quality"))]

# ✅ Aggregate Monthly Data by Appointment Delay Category
combined_data_monthly <- combined_data[, 
                                       .(Total_Appointments = sum(COUNT_OF_APPOINTMENTS, na.rm=TRUE)), 
                                       by = .(Month, TIME_BETWEEN_BOOK_AND_APPT)]

# Convert Month to Date type for plotting
combined_data_monthly[, Month := as.Date(paste0(Month, "-01"))]

# Generate a sequence of months from July 2022 to October 2024
months_seq <- seq(as.Date("2022-07-01"), as.Date("2024-11-01"), by = "1 month")

# ✅ Remove unwanted labels (if necessary)
custom_labels <- months_seq[!(months_seq %in% as.Date(c("2022-09-01", "2024-11-01")))]

# ✅ Plot Monthly Trends
ggplot(combined_data_monthly, aes(x = Month, y = Total_Appointments, 
                                  color = TIME_BETWEEN_BOOK_AND_APPT, 
                                  group = TIME_BETWEEN_BOOK_AND_APPT)) +
  geom_line(size = 1) +  # Line plot
  geom_point(size = 2) +  # Add points
  labs(title = "Monthly Appointment Trends by Time Between Booking & Appointment",
       x = "Month",
       y = "Total Appointments",
       color = "Time Between Booking & Appointment") +
  theme_minimal() +
  scale_x_date(date_labels = "%b %Y", breaks = custom_labels) +
  scale_y_continuous(labels = scales::comma) +  # Ensure y-axis has numeric format with commas
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))  # Rotate x-axis labels

# ===========================================
# 📂 4. Calculate Percentage Change for Trends
# ===========================================

combined_data_monthly <- combined_data_monthly %>%
  arrange(Month) %>%
  group_by(TIME_BETWEEN_BOOK_AND_APPT) %>%
  mutate(Percentage_Change = (Total_Appointments / lag(Total_Appointments) - 1) * 100)

# Plot percentage change
ggplot(combined_data_monthly, aes(x = Month, y = Percentage_Change, 
                                  color = TIME_BETWEEN_BOOK_AND_APPT, 
                                  group = TIME_BETWEEN_BOOK_AND_APPT)) +
  geom_line(size = 1) +  
  geom_point(size = 2) +  
  labs(title = "Percentage Change in Monthly Appointments",
       x = "Month",
       y = "Percentage Change (%)",
       color = "Time Between Booking & Appointment") +
  theme_minimal() +
  scale_x_date(date_labels = "%b %Y", breaks = custom_labels) +  
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        legend.position = "bottom",
        plot.title = element_text(hjust = 0.5))

# ===========================================
# 📂 5. Loading All Practice Level Files and Combining with LSOA Information
# ===========================================
gp_postcode_info <- read.csv("gp-reg-pat-prac-all.csv")

lsoa_lookup <- read.csv("NSP21CL_MAY24_UK_LU.csv")

# 1️⃣ Get a list of all relevant CSV files
file_list <- list.files(pattern = "Practice_Level_Crosstab_.*\\.csv")

# 2️⃣ Load and process each file
aggregated_list <- lapply(file_list, function(file) {
  # Read the current file
  data <- fread(file)  # Fast reading with fread()
  
  # Extract the month & year from the filename
  month_year <- str_extract(file, "[A-Za-z]+_\\d{2}")  # Extracts "Oct_24" from "Practice_Level_Crosstab_Oct_24.csv"
  
  # Aggregate appointments per GP_CODE and TIME_BETWEEN_BOOK_AND_APPT
  aggregated_data <- data[, .(Total_Appointments = sum(COUNT_OF_APPOINTMENTS, na.rm=TRUE)), 
                          by = .(GP_CODE, TIME_BETWEEN_BOOK_AND_APPT)]
  
  # Add a column to track which file (month-year) this data came from
  # ✅ Add the extracted Month-Year from filename
  aggregated_data[, Month_Year := month_year]
  
  return(aggregated_data)
})

# 3️⃣ Combine all processed files into one data.table
aggregated_data <- rbindlist(aggregated_list)

setDT(aggregated_data)  # Convert to data.table
setDT(gp_postcode_info)  # Convert to data.table

# 5️⃣ Merge the aggregated data with the postcode information
aggregated_data <- gp_postcode_info[aggregated_data, on = .(CODE = GP_CODE), nomatch = 0]

# Select only required columns
final_data <- aggregated_data[, .(CODE, POSTCODE, TIME_BETWEEN_BOOK_AND_APPT, Total_Appointments, Month_Year)]

setDT(lsoa_lookup)

final_data[, POSTCODE := gsub(" ", "", POSTCODE)]
lsoa_lookup[, pcd7 := gsub(" ", "", pcd7)]

# Perform the join on CODE = pcd7 and keep only the required columns
final_data <- lsoa_lookup[final_data, on = .(pcd7 = POSTCODE), 
                       .(CODE, POSTCODE, TIME_BETWEEN_BOOK_AND_APPT, Total_Appointments, Month_Year,
                         oac11nm, wzc11nm, soac11nm, ladnm, lacnm)]


# ===========================================
# 📂 6. Data cleaning for final_data
# ===========================================
# Checking for duplicates (no duplicates)
sum(duplicated(final_data))

# Checking if all months included (all included)
unique_month_count <- uniqueN(final_data$Month_Year)
unique_month_count 

final_data <- final_data %>%
  filter(!(TIME_BETWEEN_BOOK_AND_APPT %in% c("Unknown / Data Issue")))

# Blank values converted to NA
final_data[final_data == ""] <- NA

# Count total NA values in the entire dataset
na_count <- sum(is.na(final_data))
na_count  

# Count NA values per column
na_per_column <- final_data[, lapply(.SD, function(x) sum(is.na(x)))]
na_per_column  

# Show rows that have any NA values (130161)
rows_with_na <- final_data[complete.cases(final_data) == FALSE]
rows_with_na  

# Left 950411 rows
final_data <- na.omit(final_data)

# Checking if all months included (all included)
unique_month_count <- uniqueN(final_data$Month_Year)
unique_month_count 

# Checking for duplicates (no duplicates)
sum(duplicated(final_data))

# ===========================================
# 📂 8. Data preparation for logistic regression
# ===========================================
# Get all CSV files in the directory
file_list <- list.files(pattern = ".*General Practice.*Detailed\\.csv$")

# Function to process each file
process_file <- function(file) {
  # Extract the year-month from the filename
  month_year <- str_extract(file, "(January|February|March|April|May|June|July|August|September|October|November|December) \\d{4}")
  formatted_month_year <- format(as.Date(paste0("01 ", month_year), "%d %B %Y"), "%b_%y")
  
  # Read only the required columns
  df <- read_csv(file, col_types = cols(
    PRAC_CODE = col_character(),
    TOTAL_PATIENTS = col_double(),
    TOTAL_GP_FTE = col_character()  # Read as character first to handle inconsistencies
  )) %>%
    select(PRAC_CODE, TOTAL_PATIENTS, TOTAL_GP_FTE) %>%  # Ensure only needed columns are included
    mutate(
      TOTAL_GP_FTE = as.numeric(TOTAL_GP_FTE),  # Convert to numeric (forces NAs where conversion fails)
      Year_Month = formatted_month_year
    )
  
  return(df)
}

# Process all files and combine them into one dataframe
gp_workforce <- bind_rows(lapply(file_list, process_file))
sum(is.na(gp_workforce$TOTAL_GP_FTE))

# Perform an inner join to keep only matching rows
final_data_test <- final_data %>%
  inner_join(gp_workforce %>% select(PRAC_CODE, Year_Month, TOTAL_PATIENTS, TOTAL_GP_FTE),
             by = c("CODE" = "PRAC_CODE", "Month_Year" = "Year_Month"))

# TOTAL_GP_FTE some is N/A
sum(is.na(final_data_test))
# Remove N/A rows
final_data_test <- na.omit(final_data_test)

# Add other factors we want(imd, icb, nhser, ur01ind)
ons_postcode_directory <- read.csv("ONSPD_FEB_2024_UK.csv")
# Remove spaces in `pcd` column in ons_postcode_directory
ons_postcode_directory <- ons_postcode_directory %>%
  mutate(pcd = str_replace_all(pcd, " ", ""))  # Remove spaces

# Join with final_data_test
final_data_test <- final_data_test %>%
  left_join(
    ons_postcode_directory %>% select(pcd, imd, icb, nhser, ur01ind),
    by = c("POSTCODE" = "pcd")
  )

final_data_test <- final_data_test %>%
  mutate(
    ur01ind_category = case_when(
      ur01ind %in% c(1, 5) ~ "Urban",
      ur01ind %in% c(2, 3, 4, 6, 7, 8) ~ "Rural",
      TRUE ~ NA_character_  # Assign NA for any unexpected values
    )
  )

# There is 2625 NA
sum(is.na(final_data_test))
# 525 rows does not match anytg in ons_postcode directory
colSums(is.na(final_data_test))

final_data_test <- na.omit(final_data_test)     

postcodes <- unique(final_data[, .(POSTCODE)])  # Extract column as a data.table
fwrite(postcodes, "postcodes.csv")  # Save as CSV

postcode_imd_lookup <- read.csv("2019-deprivation-by-postcode.csv")
unique(postcode_imd_lookup$Index.of.Multiple.Deprivation.Decile)
# 11 rows no imd decile
colSums(is.na(postcode_imd_lookup))
postcode_imd_lookup <- na.omit(postcode_imd_lookup)

final_data_test <- final_data_test %>%
  left_join(
    postcode_imd_lookup %>% select(Postcode, Index.of.Multiple.Deprivation.Decile),
    by = c("POSTCODE" = "Postcode")
  )

# ===========================================
# 📂 8. Negative Binomial Regression
# ===========================================
final_data_test <- final_data_test %>%
  mutate(
    long_waits = case_when(
      TIME_BETWEEN_BOOK_AND_APPT %in% c("15  to 21 Days", "22  to 28 Days", "More than 28 Days") ~ TOTAL_PATIENTS,
      TRUE ~ 0  # Assign 0 if the appointment wait time is short
    )
  )

# Variance > Mean so use negative binomial
mean(final_data_test_model_data_test$long_waits)
var(final_data_test_model_data_test$long_waits)

# Ensure Year_Month is in Date format
final_data_test_model_data <- final_data_test %>%
  mutate(
    Month_Year = as.Date(paste0("01-", Month_Year), format = "%d-%b_%y"),  # Convert to date format
    Time_Trend = as.integer(as.numeric(as.factor(Month_Year))),  # Assigns 1 to N sequentially
    Time_Seasonal = month(Month_Year)  # Extracts month as numeric (1-12)
  )

table(final_data_test_model_data$Time_Trend)  # Should show 1 to 25
table(final_data_test_model_data$Time_Seasonal)  # Should show values between 1-12

# Model cannot run because TOTAL_PATIENTS had 0 so have to exclude those GP clinics
sum(final_data_test_model_data$TOTAL_PATIENTS == 0, na.rm = TRUE)
final_data_test_model_data <- final_data_test_model_data %>%
  filter(TOTAL_PATIENTS > 0)

# Ensure categorical variables are factors
final_data_test_model_data <- final_data_test_model_data %>%
  mutate(
    icb = as.factor(icb),
    nhser = as.factor(nhser),
    imd = as.factor(imd),
    ur01ind_category = as.factor(ur01ind_category),
    Time_Seasonal = as.factor(Time_Seasonal),  # Treat as categorical for seasonality
    oac11nm = as.factor(oac11nm),
    wzc11nm = as.factor(wzc11nm), 
    soac11nm = as.factor(soac11nm), 
    ladnm = as.factor(ladnm), 
    lacnm = as.factor(lacnm)
  )

str(final_data_test_model_data)
colSums(is.na(final_data_test_model_data_test))

unique(final_data_test_model_data$TIME_BETWEEN_BOOK_AND_APPT)

# transforming time delay categories to numeric
final_data_test_model_data$Numeric_Delay <- factor(final_data_test_model_data$TIME_BETWEEN_BOOK_AND_APPT,
                                                        levels = c("Same Day", "1 Day", "2 to 7 Days", "8  to 14 Days", "15  to 21 Days", "22  to 28 Days", "More than 28 Days"),
                                                        labels = c(0, 1, 4, 11, 18, 25, 35)  # Midpoints of each range
)
final_data_test_model_data$Numeric_Delay <- as.numeric(as.character(final_data_test_model_data$Numeric_Delay))

# overdispersed so no poisson
mean_val <- mean(final_data_test_model_data$Numeric_Delay)
var_val <- var(final_data_test_model_data$Numeric_Delay)
overdispersion_ratio <- var_val / mean_val
overdispersion_ratio

# not a lot zeros so dont need zero-inflated nb
mean(final_data_test_model_data$Numeric_Delay == 0, na.rm = TRUE) * 100

# Aggregate data: Summing long waits for selected categories
final_data_test_model_data_test <- final_data_test_model_data %>%
  group_by(CODE, POSTCODE, icb, nhser, ur01ind_category, TOTAL_GP_FTE, TOTAL_PATIENTS, Index.of.Multiple.Deprivation.Decile, Time_Trend, Time_Seasonal) %>%
  summarise(
    long_waits = sum(Total_Appointments[TIME_BETWEEN_BOOK_AND_APPT %in% c("15  to 21 Days", "22  to 28 Days", "More than 28 Days")], na.rm = TRUE),
    Total_Appointments = sum(Total_Appointments, na.rm = TRUE)  # Sum all appointments per GP
  ) %>%
  ungroup()

# add adjusted measure of GP workforce
final_data_test_model_data_test <- final_data_test_model_data_test %>%
  mutate(GP_per_1000 = (TOTAL_GP_FTE / TOTAL_PATIENTS) * 1000)

# decile to quantile
final_data_test_model_data_test <- final_data_test_model_data_test %>%
  mutate(IMD_Quantile = case_when(
    Index.of.Multiple.Deprivation.Decile %in% c(1, 2) ~ 1,
    Index.of.Multiple.Deprivation.Decile %in% c(3, 4) ~ 2,
    Index.of.Multiple.Deprivation.Decile %in% c(5, 6) ~ 3,
    Index.of.Multiple.Deprivation.Decile %in% c(7, 8) ~ 4,
    Index.of.Multiple.Deprivation.Decile %in% c(9, 10) ~ 5
  ),
  IMD_Quantile = factor(IMD_Quantile)  # Explicitly convert to factor
  )


model_nb <- glmmTMB(
  long_waits ~ nhser + IMD_Quantile + ur01ind_category + 
    GP_per_1000 + Time_Trend + Time_Seasonal + (1 | icb) + offset(log(Total_Appointments)),
  family = nbinom2,
  data = final_data_test_model_data_test
)

summary(model_nb)

# overdispersed so no poisson
mean_val <- mean(final_data_test_model_data$long_waits)
var_val <- var(final_data_test_model_data$long_waits)
overdispersion_ratio <- var_val / mean_val
overdispersion_ratio

# Most predictors returned significant hence we should look at effect size
exp_coef <- exp(fixef(model_nb)$cond)  # Converts log-scale estimates into meaningful effect sizes
exp_coef


#############test plot
# Create a separate dataset for time series analysis
library(lubridate)
final_data_time_series <- final_data %>%
  select(Month_Year, TIME_BETWEEN_BOOK_AND_APPT, Total_Appointments) %>%
  mutate(
    Month_Year = parse_date_time(Month_Year, "b_y") # Convert "Apr_23" to "2023-04-01"
  ) %>%
  group_by(Month_Year, TIME_BETWEEN_BOOK_AND_APPT) %>%
  summarise(Total_Appointments = sum(Total_Appointments, na.rm = TRUE)) %>%
  ungroup()

ggplot(final_data_time_series , aes(x = Month_Year, y = Total_Appointments, color = TIME_BETWEEN_BOOK_AND_APPT)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  theme_minimal() +
  labs(
    title = "Time Series of Appointment Delays Over Time",
    x = "Month",
    y = "Total Appointments",
    color = "Time Delay Category"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

################### marginal effects
install.packages("marginaleffects")
library(marginaleffects)

marginal_effects <- predictions(model_nb)
marginal_effects

ls("package:marginaleffects")

nhser_summary <- marginal_effects %>%
  group_by(nhser) %>%
  summarise(
    avg_predicted_waits = mean(estimate, na.rm = TRUE),  # Average predicted long waits per region
    se_predicted = sd(estimate, na.rm = TRUE) / sqrt(n()),  # Standard error
    min_predicted = min(estimate, na.rm = TRUE),  # Minimum predicted long waits
    max_predicted = max(estimate, na.rm = TRUE)   # Maximum predicted long waits
  ) %>%
  arrange(desc(avg_predicted_waits))

ggplot(nhser_summary, aes(x = reorder(nhser, avg_predicted_waits), y = avg_predicted_waits)) +
  geom_col(fill = "steelblue") +
  geom_errorbar(aes(ymin = min_predicted, ymax = max_predicted), width = 0.2) +
  coord_flip() +
  labs(
    title = "Predicted Long Waits by NHS Region",
    x = "NHS Region",
    y = "Predicted Long Waits"
  ) +
  theme_minimal()

########################## spatial data
# Unzip this file. You can do it with R (as below), or clicking on the object you downloaded.
unzip("Integrated_Care_Boards_April_2023_Shapefile.zip", junkpaths = FALSE)

# Read this shape file with the sf library.
library(sf)
my_sf <- read_sf("ICB_APR_2023_EN_BGC.shp")

# Basic plot of this shape file:
par(mar = c(0, 0, 0, 0))
plot(st_geometry(my_sf), col = "#f2f2f2", bg = "skyblue", lwd = 0.25, border = 0)

filtered_final_data_test_model_data_test <- final_data_test_model_data_test[final_data_test_model_data_test$Time_Trend %in% c(1, 25), ]

summary_by_icb <- filtered_final_data_test_model_data_test %>%
  group_by(icb, Time_Trend) %>%
  summarise(
    total_long_waits = sum(long_waits),
    .groups = "drop"
  )

# merge my dataset with spatial data
# Perform the join
merged_icb_data <- summary_by_icb %>%
  left_join(my_sf, by = c("icb" = "ICB23CD"))

merged_icb_data_22 <- merged_icb_data[merged_icb_data$Time_Trend %in% c(1), ]
merged_icb_data_24 <- merged_icb_data[merged_icb_data$Time_Trend %in% c(25), ]

# Make sure the variable you are studying is numeric
merged_icb_data_22$total_long_waits <- as.numeric(merged_icb_data_22$total_long_waits)
merged_icb_data_24$total_long_waits <- as.numeric(merged_icb_data_24$total_long_waits)

# Palette of 30 colors
library(RColorBrewer)
my_colors <- brewer.pal(9, "Reds")
my_colors <- colorRampPalette(my_colors)(30)

# Attribute the appropriate color to each country
class_of_country <- cut(summary_by_icb$total_long_waits, 30)
my_colors <- my_colors[as.numeric(class_of_country)]

# Restore sf class after join
merged_icb_data_22 <- st_as_sf(merged_icb_data_22)

# Make the plot
plot(st_geometry(merged_icb_data_22),
     col = my_colors,
     bg = "#A6CAE0"
)

# Restore sf class after join
merged_icb_data_24 <- st_as_sf(merged_icb_data_24)

# Make the plot
plot(st_geometry(merged_icb_data_24),
     col = my_colors,
     bg = "#A6CAE0"
)


########################
# Get combined range from both datasets
all_vals <- c(merged_icb_data_22$total_long_waits, merged_icb_data_24$total_long_waits)

# Define shared breaks (e.g. every 50,000)
breaks_shared <- seq(0, max(all_vals, na.rm = TRUE), by = 50000)

map_2022 <- tm_shape(merged_icb_data_22) +
  tm_polygons("total_long_waits",
              palette = "Reds",
              breaks = breaks_shared,
              showNA = TRUE,          # show NA values
              textNA = "Missing",     # label for NA in legend
              colorNA = "grey80",
              title = "Long Wait Appointments") +
  tm_layout(
    main.title = "October 2022",
    legend.show = FALSE,
    bg.color = "#A6CAE0"
  )

map_2022

map_2024 <- tm_shape(merged_icb_data_24) +
  tm_polygons("total_long_waits",
              palette = "Reds",
              breaks = breaks_shared,
              title = "Long Wait Appointments") +
  tm_layout(
    main.title = "October 2024",
    legend.outside = TRUE,
    legend.outside.position = "right",
    bg.color = "#A6CAE0"
  )

tmap_arrange(map_2022, map_2024, ncol = 3)


################
# Shared breaks across both maps
all_vals <- c(merged_icb_data_22$total_long_waits, merged_icb_data_24$total_long_waits)
breaks_shared <- seq(0, max(all_vals, na.rm = TRUE), by = 50000)

# First map (2022)
map_2022 <- tm_shape(merged_icb_data_22) +
  tm_polygons("total_long_waits",
              palette = "Reds",
              breaks = breaks_shared,
              showNA = FALSE) +
  tm_layout(
    main.title = "October 2022",
    legend.show = FALSE,
    bg.color = "#A8DADC",
    outer.margins = 0
  )

map_2022

# Second map (2024)
map_2024 <- tm_shape(merged_icb_data_24) +
  tm_polygons("total_long_waits",
              palette = "Reds",
              breaks = breaks_shared,
              showNA = FALSE) +
  tm_layout(
    main.title = "October 2024",
    legend.show = FALSE,
    bg.color = "#A8DADC",
    outer.margins = 0
  )

map_2024

# Legend-only panel (uses dummy shape and legend)
legend_panel <- tm_shape(merged_icb_data_24) +
  tm_polygons("total_long_waits",
              palette = "Reds",
              breaks = breaks_shared,
              showNA = TRUE,
              textNA = "No Data",
              colorNA = "#A9A9A9",
              title = "Delayed Appointment Counts (>=15 Days)") +
  tm_layout(
    legend.only = TRUE,
    legend.outside = FALSE,
    legend.position = c("left","center"),
    bg.color = "#A8DADC"
  )

legend_panel

# Arrange all three panels in a row
tmap_arrange(map_2022, map_2024, legend_panel, ncol = 3)

############################################ confidence intervals
exp(confint(model_nb, method = "Wald"))












# Full join to preserve all geometries
merged_icb_data_22 <- left_join(my_sf, summary_by_icb %>% filter(Time_Trend == 1), by = c("ICB23CD" = "icb"))
merged_icb_data_24 <- left_join(my_sf, summary_by_icb %>% filter(Time_Trend == 25), by = c("ICB23CD" = "icb"))

# Shared breaks across both maps
all_vals <- c(merged_icb_data_22$total_long_waits, merged_icb_data_24$total_long_waits)
breaks_shared <- seq(0, max(all_vals, na.rm = TRUE), by = 50000)

map_2022 <- tm_shape(merged_icb_data_22) +
  tm_polygons("total_long_waits",
              palette = "Reds",
              breaks = breaks_shared,
              showNA = TRUE,
              textNA = "No Data",
              colorNA = "#A9A9A9") +  # Sets the fill for NA regions
  tm_layout(
    legend.show = FALSE,
    bg.color = "#FFFFFF",
    outer.margins = 0
  )

map_2022

map_2024 <- tm_shape(merged_icb_data_24) +
  tm_polygons("total_long_waits",
              palette = "Reds",
              breaks = breaks_shared,
              showNA = TRUE,
              textNA = "No Data",
              colorNA = "#A9A9A9") +
  tm_layout(
    legend.show = FALSE,
    bg.color = "#FFFFFF",
    outer.margins = 0
  )

map_2024

summary(merged_icb_data_22$total_long_waits)
summary(merged_icb_data_24$total_long_waits)

legend_panel <- tm_shape(merged_icb_data_24) +
  tm_polygons("total_long_waits",
              palette = "Reds",
              breaks = breaks_shared,
              showNA = TRUE,
              textNA = "No Data",
              colorNA = "#A9A9A9",
              title = "Appointments Delayed 
           ≥15 Days") +
  tm_layout(
    legend.only = TRUE,
    legend.position = c("left", "center"),
    bg.color = "#FFFFFF"
  )

legend_panel


tmap_arrange(map_2022, map_2024)
