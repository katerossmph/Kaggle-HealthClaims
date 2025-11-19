##########################################################
# Healthcare Dataset Cleaning Template
# Author: Kate Ross
# Date: 2025-11-17
# Purpose: Systematic and reproducible cleaning of 
# synthetic Kaggle healthcare dataset
##########################################################


#### 00. Load Libraries ####

library(dplyr)
library(readr)
library(janitor)
library(lubridate)
library(stringr)
library(tibble)
library(tidyr) 

#### 01. Load Raw Data ####
claims <- read_csv(here::here("data", "raw", "healthcare_dataset.csv"))

# INSPECT THE DATA
  # check the structure: see columns, types, and sample rows
glimpse(claims)
str(claims)

  # Check Summary Statistics
summary(claims)

  # Check missing values
colSums(is.na(claims))

#### 02. Standardizing Column Names ####
names(claims) <- tolower(gsub(" ", "_", names(claims)))      # Lowercase names, replace spaces with _

#### 3. Converting Variable Types ####

# Categorical Variables
cat_variables <- c("gender", "blood_type", "medical_condition", 
                   "insurance_provider", "admission_type", "medication", "test_results")

clean_data_pipeline <- function(df) { ## Cleaning the dataframe 
  df_clean <- df %>%
    
    # Trim whitespace in character columns
    mutate(across(where(is.character), ~str_trim(.))) %>%
    
    # Standardize capitalization (Title case)
    mutate(across(where(is.character), ~str_to_title(.))) %>%
    
    # Convert dates
    mutate(across(c(date_of_admission, discharge_date), as.Date)) %>%
    
    # Replace empty strings with NA
    mutate(across(where(is.character), ~na_if(., "")))
  
  return(df_clean)
}

clean_df <- clean_data_pipeline(claims)


recode_cat_to_num <- function(df, cat_vars) { ## Recoding categorical variables to have numerical values
  
  mappings_list <- list()
  
  for(var in cat_vars) {
    # Ensure factor
    df[[var]] <- as.factor(df[[var]])
    
    # Create numeric code column
    code_var <- paste0(var, "_code")
    df[[code_var]] <- as.numeric(df[[var]])
    
    # Create mapping table
    mapping <- data.frame(
      variable = var,
      level = levels(df[[var]]),
      code = 1:length(levels(df[[var]]))
    )
    mappings_list[[var]] <- mapping
  }
  
  # Combine mappings
  mappings_df <- do.call(rbind, mappings_list)
  
  return(list(coded_data = df, mappings = mappings_df))
}

# Apply Re-coding
result <- recode_cat_to_num(clean_df, cat_variables)
clean_df <- result$coded_data
categorical_mappings <- result$mapping

#### Save Outputs ####
write.csv(clean_df, "data/processed/clean_data_coded.csv", row.names = FALSE)
write.csv(categorical_mappings, "data/processed/categorical_mappings.csv", row.names = FALSE)