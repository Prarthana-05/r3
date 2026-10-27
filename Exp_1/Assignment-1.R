tryCatch({
  air_data <- read.csv("C:\\Users\\YASH\\Downloads\\beijing+multi+site+air+quality+data\\PRSA2017_Data_20130301-20170228\\PRSA_Data_20130301-20170228\\PRSA_Data_Aotizhongxin_20130301-20170228.csv")
  print("--- First six records ---")
  print(head(air_data))
  
  print("--- Structure of the dataset ---")
  str(air_data)
  
  print(paste("Total Rows:", nrow(air_data)))
  print(paste("Total Columns:", ncol(air_data)))
  print(paste("Contains missing values:", any(is.na(air_data))))
  print(paste("Total missing values in dataset:", sum(is.na(air_data))))
}, error = function(e) {
  message("Error: The file could not be found, opened, or the format is incorrect.")
})

temperature <- c(28, 30, NA, 32)
missing_object <- NULL
undefined_value <- 0/0

cat("\n--- Task 2: NA, NULL, NaN ---\n")
print(paste("is.na(temperature observation 3):", is.na(temperature)[3])) 
print(paste("is.null(missing_object):", is.null(missing_object)))
print(paste("is.nan(undefined_value):", is.nan(undefined_value)))

missing_summary <- function(df) {
  vars <- c("PM2.5", "PM10", "SO2", "NO2", "TEMP", "WSPM", "wd")
  total_records <- nrow(df)
  
  summary_df <- data.frame(
    Variable = character(),
    Total_Records = numeric(),
    Missing_Values = numeric(),
    Missing_Percentage = numeric(),
    stringsAsFactors = FALSE
  )
  
  for (v in vars) {
    if (v %in% colnames(df)) {
      missing_vals <- sum(is.na(df[[v]]))
      missing_pct <- (missing_vals / total_records) * 100
      
      if (missing_pct > 20) {
        warning(paste("Variable", v, "contains more than 20% missing values."))
      }
      
      summary_df <- rbind(summary_df, data.frame(
        Variable = v,
        Total_Records = total_records,
        Missing_Values = missing_vals,
        Missing_Percentage = round(missing_pct, 2)
      ))
    }
  }
  return(summary_df)
}

cat("\n--- Task 3: Missing Value Summary ---\n")
summary_table <- missing_summary(air_data)
print(summary_table)

air_data$pollution_ratio <- air_data$PM2.5 / air_data$PM10

# Identify and replace NaN and infinite values with NA
air_data$pollution_ratio[is.nan(air_data$pollution_ratio) | is.infinite(air_data$pollution_ratio)] <- NA

# ==========================================
# Task 5: Handle Missing Numerical Values Using a Loop
# ==========================================
numeric_variables <- c("PM2.5", "PM10", "SO2", "NO2", "TEMP", "WSPM")

cat("\n--- Task 5: Handle Missing Numerical Values ---\n")
for (var in numeric_variables) {
  if (var %in% colnames(air_data)) {
    missing_before <- sum(is.na(air_data[[var]]))
    med_val <- median(air_data[[var]], na.rm = TRUE)
    
    # Replace missing values with calculated median
    air_data[[var]][is.na(air_data[[var]])] <- med_val
    missing_after <- sum(is.na(air_data[[var]]))
    
    cat("Variable name:", var, "\n")
    cat("Number of missing values before treatment:", missing_before, "\n")
    cat("Median used for replacement:", med_val, "\n")
    cat("Number of missing values after treatment:", missing_after, "\n\n")
  }
}

# ==========================================
# Task 6: Handle Missing Categorical Values
# ==========================================
calculate_mode <- function(x) {
  ux <- unique(na.omit(x))
  ux[which.max(tabulate(match(x, ux)))]
}

cat("--- Task 6: Handle Missing Categorical Values (wd) ---\n")
missing_wd_before <- sum(is.na(air_data$wd))
wd_mode <- calculate_mode(air_data$wd)

# Replace missing categoricals with mode
air_data$wd[is.na(air_data$wd)] <- wd_mode
missing_wd_after <- sum(is.na(air_data$wd))

cat("Variable: wd\n")
cat("Missing before:", missing_wd_before, "\n")
cat("Missing after:", missing_wd_after, "\n\n")

# ==========================================
# Task 7: Implement Error Handling
# ==========================================
clean_variable <- function(dataset, var_name) {
  tryCatch({
    if (!(var_name %in% colnames(dataset))) {
      stop("The variable does not exist.")
    }
    if (!is.numeric(dataset[[var_name]])) {
      stop("A categorical variable is passed instead of a numerical variable.")
    }
    if (all(is.na(dataset[[var_name]]))) {
      stop("The variable contains only missing values.")
    }
    
    med_val <- median(dataset[[var_name]], na.rm = TRUE)
    if (is.na(med_val)) {
      stop("The median cannot be calculated.")
    }
    
    # Replace missing values utilizing median
    dataset[[var_name]][is.na(dataset[[var_name]])] <- med_val
    return(dataset[[var_name]])
    
  }, error = function(e) {
    message(paste("Error in cleaning process for variable '", var_name, "': ", e$message, sep=""))
    return(NULL) 
  })
}

# Compiling before and after data for targeted variables
variables_plot <- c("PM2.5", "PM10", "SO2", "NO2", "TEMP", "WSPM", "wd")
missing_before_plot <- c(925, 718, 935, 1023, 20, 14, 81)
missing_after_plot <- c(0, 0, 0, 0, 0, 0, 0)

plot_data <- matrix(c(missing_before_plot, missing_after_plot), nrow = 2, byrow = TRUE)
colnames(plot_data) <- variables_plot
rownames(plot_data) <- c("Before Cleaning", "After Cleaning")

# Generate grouped Bar Chart using R Base Graphics
barplot(plot_data, 
        beside = TRUE, 
        col = c("coral", "lightgreen"), 
        main = "Missing Values Before and After Data Cleaning",
        xlab = "Variables", 
        ylab = "Number of Missing Values",
        legend.text = rownames(plot_data),
        args.legend = list(x = "topright"))


write.csv(air_data, "C:\\Users\\YASH\\Downloads\\cleaned_air_quality_data.csv", row.names = FALSE)
cat("--- Task 10: Dataset exported successfully! ---\n")
