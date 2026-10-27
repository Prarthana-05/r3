##############################################################
# AIR QUALITY DATA CLEANING PRACTICAL
# PART 1
##############################################################

# Install required packages
packages <- c("ggplot2", "reshape2")

for(pkg in packages){
  if(!require(pkg, character.only = TRUE)){
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

##############################################################
# TASK 1 : IMPORT DATASET
##############################################################

# Upload dataset in Google Colab
system("python3 -c \"from google.colab import files; files.upload()\"")

# Change this only if your uploaded filename is different
file_name <- "PRSA_Data_Aotizhongxin_20130301-20170228.csv"

data <- tryCatch({

  read.csv(file_name)

}, error=function(e){

  stop("Dataset not found. Please upload the CSV file.")

})

cat("=====================================\n")
cat("Dataset Loaded Successfully\n")
cat("=====================================\n\n")

cat("First 6 Records\n")
print(head(data))

cat("\nDataset Structure\n")
str(data)

cat("\nRows :", nrow(data), "\n")
cat("Columns :", ncol(data), "\n")

cat("\nContains Missing Values : ",
    any(is.na(data)), "\n")

cat("Total Missing Values : ",
    sum(is.na(data)), "\n")


##############################################################
# TASK 2 : NA, NULL and NaN
##############################################################

cat("\n=====================================\n")
cat("TASK 2 : NA, NULL, NaN\n")
cat("=====================================\n")

temperature <- c(28, 30, NA, 32)

missing_object <- NULL

undefined_value <- 0/0

cat("\nTemperature Vector\n")
print(temperature)

cat("\nis.na()\n")
print(is.na(temperature))

cat("\nNULL Object\n")
print(missing_object)

cat("\nis.null()\n")
print(is.null(missing_object))

cat("\nNaN Value\n")
print(undefined_value)

cat("\nis.nan()\n")
print(is.nan(undefined_value))


##############################################################
# TASK 3 : Missing Summary Function
##############################################################

missing_summary <- function(df){

  summary_table <- data.frame(
    Variable=character(),
    Total_Records=integer(),
    Missing_Values=integer(),
    Missing_Percentage=numeric(),
    stringsAsFactors = FALSE
  )

  for(col in names(df)){

    total <- nrow(df)

    missing <- sum(is.na(df[[col]]))

    percentage <- round((missing/total)*100,2)

    summary_table <- rbind(
      summary_table,
      data.frame(
        Variable=col,
        Total_Records=total,
        Missing_Values=missing,
        Missing_Percentage=percentage
      )
    )

    if(percentage > 20){
      warning(paste(col,
      "contains more than 20% missing values"))
    }

  }

  return(summary_table)

}

selected_variables <- data[c(
"PM2.5",
"PM10",
"SO2",
"NO2",
"TEMP",
"WSPM",
"wd"
)]

summary_result <- missing_summary(selected_variables)

cat("\nMissing Summary Table\n")

print(summary_result)
##############################################################
# TASK 4 : Identify Invalid Numerical Results
##############################################################

cat("\n============================\n")
cat("TASK 4 : Pollution Ratio\n")
cat("============================\n")

data$pollution_ratio <- data$PM2.5 / data$PM10

cat("NA Count :", sum(is.na(data$pollution_ratio)), "\n")
cat("NaN Count :", sum(is.nan(data$pollution_ratio)), "\n")
cat("Positive/Negative Infinite Count :",
    sum(is.infinite(data$pollution_ratio)), "\n")

# Replace NaN and Infinite values with NA
data$pollution_ratio[
  is.nan(data$pollution_ratio) |
  is.infinite(data$pollution_ratio)
] <- NA

cat("Replacement Completed\n")


##############################################################
# TASK 5 : Handle Missing Numerical Values Using Loop
##############################################################

numeric_variables <- c(
  "PM2.5",
  "PM10",
  "SO2",
  "NO2",
  "TEMP",
  "WSPM"
)

comparison_table <- data.frame(
  Variable=character(),
  Missing_Before=integer(),
  Missing_After=integer(),
  Values_Replaced=integer(),
  stringsAsFactors=FALSE
)

for(variable in numeric_variables){

  cat("\n----------------------------------\n")

  if(variable %in% names(data)){

    before <- sum(is.na(data[[variable]]))

    median_value <- median(data[[variable]], na.rm=TRUE)

    data[[variable]][is.na(data[[variable]])] <- median_value

    after <- sum(is.na(data[[variable]]))

    cat("Variable :", variable, "\n")
    cat("Missing Before :", before, "\n")
    cat("Median Used :", median_value, "\n")
    cat("Missing After :", after, "\n")

    comparison_table <- rbind(
      comparison_table,
      data.frame(
        Variable=variable,
        Missing_Before=before,
        Missing_After=after,
        Values_Replaced=before-after
      )
    )

  }
  else{

    cat(variable,"does not exist.\n")

  }

}


##############################################################
# TASK 6 : Handle Missing Categorical Values
##############################################################

calculate_mode <- function(x){

  x <- x[!is.na(x)]

  unique_values <- unique(x)

  unique_values[
    which.max(tabulate(match(x, unique_values)))
  ]

}

before_wd <- sum(is.na(data$wd))

mode_value <- calculate_mode(data$wd)

data$wd[is.na(data$wd)] <- mode_value

after_wd <- sum(is.na(data$wd))

cat("\nMode of wd :", mode_value, "\n")
cat("Missing Before :", before_wd, "\n")
cat("Missing After :", after_wd, "\n")

comparison_table <- rbind(
  comparison_table,
  data.frame(
    Variable="wd",
    Missing_Before=before_wd,
    Missing_After=after_wd,
    Values_Replaced=before_wd-after_wd
  )
)


##############################################################
# TASK 7 : Error Handling
##############################################################

clean_variable <- function(df, variable){

  tryCatch({

    if(!(variable %in% names(df))){

      stop("Variable does not exist.")

    }

    if(!is.numeric(df[[variable]])){

      stop("Variable is categorical.")

    }

    if(all(is.na(df[[variable]]))){

      stop("Variable contains only NA values.")

    }

    med <- median(df[[variable]], na.rm=TRUE)

    if(is.na(med)){

      stop("Median cannot be calculated.")

    }

    df[[variable]][is.na(df[[variable]])] <- med

    cat(variable,"cleaned successfully.\n")

    return(df[[variable]])

  },

  error=function(e){

    cat("Error :",e$message,"\n")

    return(df[[variable]])

  })

}

# Example Calls
data$TEMP <- clean_variable(data,"TEMP")

clean_variable(data,"InvalidVariable")

clean_variable(data,"wd")

##############################################################
# TASK 8 : Compare Missing Values Before and After Cleaning
##############################################################

cat("\n=========================================\n")
cat("TASK 8 : Missing Value Comparison\n")
cat("=========================================\n")

print(comparison_table)

cat("\nInterpretation:\n")

if(all(comparison_table$Missing_After == 0)){
  cat("All selected missing values have been successfully handled.\n")
}else{
  cat("Some missing values are still present in the dataset.\n")
}


##############################################################
# TASK 9 : Visualization
##############################################################

library(ggplot2)

plot_data <- data.frame(
  Variable = rep(comparison_table$Variable,2),
  Missing = c(comparison_table$Missing_Before,
              comparison_table$Missing_After),
  Status = c(
    rep("Before Cleaning",nrow(comparison_table)),
    rep("After Cleaning",nrow(comparison_table))
  )
)

ggplot(plot_data,
       aes(x=Variable,
           y=Missing,
           fill=Status))+

geom_bar(stat="identity",
         position="dodge")+

labs(
title="Missing Values Before and After Cleaning",
x="Variables",
y="Number of Missing Values"
)+

theme_minimal()

cat("\nBar Chart Generated Successfully.\n")


##############################################################
# TASK 10 : Export Cleaned Dataset
##############################################################

write.csv(
  data,
  "cleaned_air_quality_data.csv",
  row.names=FALSE
)

cat("\nCleaned dataset exported successfully.\n")
cat("Filename : cleaned_air_quality_data.csv\n")


##############################################################
# FINAL OUTPUTS
##############################################################

cat("\n=========================================\n")
cat("PRACTICAL COMPLETED SUCCESSFULLY\n")
cat("=========================================\n")

cat("\nFiles Generated:\n")
cat("1. Missing Summary Table\n")
cat("2. Comparison Table\n")
cat("3. Missing Value Bar Chart\n")
cat("4. cleaned_air_quality_data.csv\n")