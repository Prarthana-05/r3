# Load necessary libraries
install.packages("naniar", repos = "https://cloud.r-project.org/")
install.packages("skimr", repos = "https://cloud.r-project.org/")
install.packages("stringi", repos = "https://cloud.r-project.org/")
library(naniar)
library(skimr)

# ---------------------------------------------------------
# 1. Simulate the Adult Dataset & Introduce Missingness
# ---------------------------------------------------------
# 1. Define the exact path to the file uploaded to Colab
file_path_adult <- "C:\\Users\\YASH\\Downloads\\adult\\adult.data"

# 2. Define the column names based on the dataset attributes
adult_columns <- c("age", "workclass", "fnlwgt", "education", 
                   "education_num", "marital_status", "occupation", 
                   "relationship", "race", "sex", "capital_gain", 
                   "capital_loss", "hours_per_week", "native_country", "income")

# 3. Read the CSV using the path, ensuring R knows there is no header
adult_data <- read.csv(file_path_adult, 
                       header = FALSE, 
                       col.names = adult_columns, 
                       strip.white = TRUE) # strip.white removes the trailing spaces after commas

# ---------------------------------------------------------
# Task 1: Identify different forms of missing data
# ---------------------------------------------------------
cat("\n--- Identifying Missing Data Types ---\n")
cat("Total NAs in age:", sum(is.na(adult_data$age)), "\n")
cat("Total NaNs in age:", sum(is.nan(adult_data$age)), "\n")
cat("Total blank strings in workclass:", sum(adult_data$workclass == ""), "\n")

# Demonstrating NULL
null_example <- NULL
cat("Is null_example NULL?", is.null(null_example), "\n")
# Note: NULL represents the absence of an object, it cannot be stored inside a standard data frame column like NA.

# Variable-wise summary
print(naniar::miss_var_summary(adult_data))

# ---------------------------------------------------------
# Task 3: Custom Median-Imputation Function
# ---------------------------------------------------------
impute_median <- function(num_vector) {
  # Calculate median of valid observations
  valid_median <- median(num_vector, na.rm = TRUE)
  
  # Replace missing observations (NA or NaN)
  num_vector[is.na(num_vector) | is.nan(num_vector)] <- valid_median
  return(num_vector)
}

# ---------------------------------------------------------
# Task 2 & 4: Treatment Strategy & Analysis
# ---------------------------------------------------------
# Visualize missingness BEFORE cleaning
# gg_miss_var(adult_data) # Uncomment to plot in your environment

# Convert impossible ages to NA
adult_data$age <- ifelse(adult_data$age == 999, NA, adult_data$age)

# Replace blank strings in categorical data
adult_data$workclass <- ifelse(adult_data$workclass == "", "Unknown", adult_data$workclass)

# Impute missing numeric values using the custom function
adult_data$cleaned_age <- impute_median(adult_data$age)

# Identify complete cases
complete_obs <- adult_data[complete.cases(adult_data), ]

# ---------------------------------------------------------
# Task 5: Validate the cleaned dataset
# ---------------------------------------------------------
cat("\n--- Post-Cleaning Validation ---\n")
cat("Max age (should not be 999):", max(adult_data$cleaned_age, na.rm = TRUE), "\n")
cat("Blanks in workclass (should be 0):", sum(adult_data$workclass == ""), "\n")
cat("NAs in cleaned_age (should be 0):", sum(is.na(adult_data$cleaned_age)), "\n")

# Comprehensive summary using skimr
skim_output <- skimr::skim(adult_data)
print(skim_output)

# Save deliverables
write.csv(adult_data, "C:\\Users\\YASH\\Downloads\\adult\\cleaned_adult_data.csv", row.names = FALSE)