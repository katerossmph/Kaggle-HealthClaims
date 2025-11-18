# ============================================================
# 04_eda.R
# Exploratory Data Analysis for Healthcare Claims Dataset
# ============================================================


######## 1. Setup & Pre-Processing ########
install.packages("skimr")

# Load packages
library(tidyverse)
library(janitor)
library(lubridate)
library(here)
library(dplyr)
library(skimr)

# Load cleaned dataset
source("scripts/01_data_cleaning.R")

# ---- Missingness ----
missing_summary <- clean_df %>%
  summarise(across(everything(), ~mean(is.na(.)))) %>%
  pivot_longer(everything(), 
               names_to = "variable", 
               values_to = "missing_pct") %>%
  arrange(desc(missing_pct))

  # write_csv(missing_summary, here("outputs/tables/missingness_summary.csv"))

# ---- Duplicates ----
duplicate_rows <- clean_df %>%
  get_dupes(name, hospital, date_of_admission)

  # write_csv(duplicate_rows, here("outputs/tables/duplicate_rows.csv"))

# ---- Data type Checks ----
type_check <- sapply(clean_df, class)

  # write_csv(as.data.frame(type_check), here("outputs/tables/data_types.csv"))


# ---- Adding New Variables ----

clean_df <- clean_df %>%
  mutate(
    # Adding an Age_Group Variable
    age_group = cut(age, breaks = seq(0, 100, by = 10), right = FALSE, include.lowest = TRUE),
    age_group = gsub("\\[|\\)", "", age_group),
    age_group = gsub(",", "-", age_group),
    
    # Length of Stay in Days
    length_of_stay = as.numeric(discharge_date - date_of_admission), 
    
    # Billing per day
    billing_per_day = billing_amount / length_of_stay
  )

# ============================================================
# 2. Univariate Analysis
# ============================================================

### Numeric Variables Exploration ###
numeric_vars <- clean_df %>%
  select(age, billing_amount, length_of_stay, billing_per_day)

# Summary Stats Table
numeric_summary <- skim(numeric_vars)

# Histograms (faceted) of Numeric Variables
numeric_long <- numeric_vars %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value")

p_numeric_hist <- ggplot(numeric_long, aes(x = value)) + 
  geom_histogram(bins = 30, fill = "steelblue", color = "white")  + 
  facet_wrap(~variable, scales = "free") +
  labs(title = "Distribution of Numeric Variables",
       x = "Value", y = "Count") +
  theme_minimal()

ggsave(here("output/figures/numeric_histograms.png"), p_numeric_hist, width = 10, height = 6)


##### Categorical Variables ##### 

cat_variables <- c("gender", "blood_type", "medical_condition", "insurance_provider",
                   "admission_type", "medication", "test_results")

# Frequency Tables (top 10 if too many levels)
for (var in cat_variables) {
  freq_table <- clean_df %>%
    count(.data[[var]]) %>%
    arrange(desc(n))
  
  write.csv(freq_table, here(paste0("output/tables/freq_", var, ".csv")))
  
  top_n <- freq_table %>%
    slice_head(n = 10)
  
  p <- ggplot(top_n, aes(x = reorder(.data[[var]], n), y = n, fill = n)) +
    geom_col(show.legend = FALSE) +
    coord_flip() +
    labs(title = paste("Top 10", var), x = var, y = "Count") +
    theme_minimal()
  
  ggsave(here(paste0("output/figures/top10_", var, ".png")), p, width = 8, height = 4)
}

# ============================================================
# 3. Bivariate & Multivariate Analysis
# ============================================================

## ---- Medical Condition X gender ----
condition_by_gender <- clean_df %>%
  count(medical_condition, gender) %>%
  arrange(desc(n))

ggplot(condition_by_gender, aes(x = reorder(medical_condition, n), y = n, fill = gender)) +
  geom_col(position = "dodge")+
  coord_flip() + 
  labs(title = "Count of Medical Condition by Gender",
       x = "Medical Condition", y = "Count") +
  theme_minimal()

ggsave("output/figures/cpmdotopm_by_gender.png", width = 8, height = 4)


## ---- Medical_condition x age_group ----
condition_by_age <- clean_df %>%
  count(age_group, medical_condition)

ggplot(condition_by_age, aes(x = age_group, y = n, fill = medical_condition)) +
  geom_col(position = "dodge") +
  labs(title = "Medical Condition Distribution by Age Group",
       x = "Age Group",
       y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("output/figures/condition_by_age.png", width = 8, height = 4)

## ---- Billing Amount X Admission Type ----
ggplot(clean_df, aes(admission_type, billing_amount)) +
  geom_boxplot() +
  coord_flip() +
  labs(title = "Billing Amount by Admissions Type",
       y = "Billing Amount", x = "Admission Type") +
  theme_minimal()

ggsave(here("outputs/figures/billing_by_admission_type.png"), width = 8, height = 4)


## ---- Billing Amount x Insurance Provider ----
ggplot(clean_df, aes(x = insurance_provider, y = billing_amount)) +
  geom_boxplot() +
  # geom_text(aes(label = billing_amount), position = "dodge")+
  coord_flip() +
  labs(title = "Billing Amount by Insurance Provider", 
       x = "Insurance Provider", y = "Billing Amount") +
  theme_minimal()

ggsave("output/figures/billing_by_insurance.png", width = 8, height = 4)

## ---- Cross-tab: condition x insurance ----
cross_tab_condition_insurance <- clean_df %>%
  tabyl(medical_condition, insurance_provider)

write_csv(cross_tab_condition_insurance,
          here("outputs/tables/condition_by_insurance.csv"))

## ---- Length of stay by condition ----
los_by_condition <- clean_df %>%
  group_by(medical_condition) %>%
  summarise(
    mean_los = mean(length_of_stay, na.rm = TRUE),
    median_los = median(length_of_stay, na.rm = TRUE),
    n = n()
  ) %>%
  arrange(desc(mean_los))

write_csv(los_by_condition,
          here("outputs/tables/length_of_stay_by_condition.csv"))


# ---- Billing by insurance provider ----
billing_by_insurance <- clean_df %>%
  group_by(insurance_provider) %>%
  summarise(
    mean_billing = mean(billing_amount, na.rm = TRUE),
    median_billing = median(billing_amount, na.rm = TRUE),
    total_claims = n()
  ) %>%
  arrange(desc(mean_billing))

write_csv(billing_by_insurance,
          here("outputs/tables/billing_by_insurance.csv"))


## ---- Age x Gender Pyramid ----

pyramid_df <- clean_df %>%
  mutate(
    # Create age groups
    age_group = cut(age, 
                    breaks = seq(0, 100, by = 10),
                    right = FALSE, 
                    include.lowest = TRUE),
    
    # Cleaning up labels
    age_group = gsub("\\[|\\)", "", age_group), # Removes Brackets
    age_group = gsub(",", "-", age_group)       # Replaces comma with dash
  ) %>%
  
  # Counting by age_group and gender
  count(age_group, gender) %>%
  
  mutate(
    n = ifelse(gender == "Male", -n, n), # Male values are negative
    
    # Prepare labels for inside bars
    label_text = abs(n),
    label_y = n/2
  ) 


ggplot(pyramid_df, aes(x = age_group, y = n, fill = gender)) +
  geom_bar(stat = "identity", width = 0.9) +
  geom_text(aes(y = label_y, label = label_text),
            color = "white", size = 3.5) +
  coord_flip() +
  scale_y_continuous(labels = abs) +
  labs(title = "Age Pyramid",
       x = "Age Group", y = "Population Count") + 
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(here("output/figures/age_histogram.png"))

# ============================================================
# 4. Time Series Analysis
# ============================================================

# ---- Weekly Admissions ----
admission_weekly <- clean_df %>%
  mutate(week = floor_date(date_of_admission, unit = "week")) %>%
  count(week) # Creates a new df with columns: week, n

ggplot(admission_weekly, aes(x = week, y = n)) +
  geom_line(color = "steelblue") +
  geom_point(color = "steelblue", size = 0.8) +
  labs(title = "Weekly Admissions Over Time", 
       x = "Week", y = "Number of Admissions") +
  theme_minimal()

ggsave("output/figures/weekly_admissions.png", width = 8, height = 4)

  ## ---- Admissions by Day of Week ----
admissions_dow <- clean_df %>%
  mutate(dow = wday(date_of_admission, label = TRUE)) %>%
  count(dow)

p_adm_dow <- ggplot(admissions_dow, aes(x = dow, y = n)) +
   geom_col(fill = "steelblue") +
  labs(title = "Admissions by Day of Week", 
       x = "Day of Week", y = "Count") +
  theme_minimal()

ggsave(here("output/figures/admissions_by_dow.png"), p_adm_dow, width = 8, heigh = 4)

## ---- Admissions over time ----
admissions_time <- clean_df %>% 
  count(date_of_admission)

ggplot(admissions_time, aes(date_of_admission, n)) +
  geom_line() +
  labs(title = "Admissions Over Time", 
       x = "Date", y = "Number of Admissions") +
  theme_minimal()

ggsave(here("outputs/figures/admissions_over_time.png"))

# ============================================================
# 5. Correlation & Dimensionality Reduction
# ============================================================


corr_df <- clean_df %>%
  select(age, billing_amount, length_of_stay,
         gender_code, blood_type_code, medical_condition_code,
         insurance_provider_code, admission_type_code,
         medication_code, test_results_code)

corr_matrix <- round(cor(corr_df, use = "pairwise.complete.obs"), 3)

write_csv(as.data.frame(corr_matrix),
          here("outputs/tables/correlation_matrix.csv"))

# Optional correlation plot
corr_plot <- ggcorr(corr_df, label = TRUE)
ggsave(here("outputs/figures/correlation_plot.png"), corr_plot)


# ============================================================
# 6. Healthcare-specific EDA
# ============================================================

# ---- Top medical conditions ----
top_conditions <- clean_df %>%
  count(medical_condition, sort = TRUE)

write_csv(top_conditions,
          here("outputs/tables/top_medical_conditions.csv"))

# ============================================================
# 7. Save session info for reproducibility
# ============================================================
writeLines(capture.output(sessionInfo()),
           here("outputs/session_info/eda_session_info.txt"))