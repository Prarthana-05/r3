# Load necessary library for benchmarking
install.packages("microbenchmark", repos = "https://cloud.r-project.org/")
library(microbenchmark)


file_path_heart <- "C:\\Users\\YASH\\Downloads\\archive_(11)\\heart_disease_uci.csv"
heart_data <- read.csv(file_path_heart)


clean_bp_loop <- function(bp_vector) {
  cleaned_bp <- numeric(length(bp_vector))
  for (i in 1:length(bp_vector)) {
    val <- bp_vector[i]
    if (is.na(val)) {
      cleaned_bp[i] <- NA
    } else if (val < 0) {
      cleaned_bp[i] <- NA
    } else if (val > 250) {
      cleaned_bp[i] <- 250
    } else {
      cleaned_bp[i] <- val
    }
  }
  return(cleaned_bp)
}

clean_bp_vectorized <- function(bp_vector) {
  bp_vector <- ifelse(bp_vector < 0, NA, bp_vector)
  bp_vector <- ifelse(bp_vector > 250, 250, bp_vector)
  return(bp_vector)
}

cat("\n--- Execution Time Comparison ---\n")
benchmark_results <- microbenchmark(
  Loop = clean_bp_loop(heart_data$trestbps),
  Vectorized = clean_bp_vectorized(heart_data$trestbps),
  times = 100
)
print(benchmark_results)

heart_data$cleaned_trestbps <- clean_bp_vectorized(heart_data$trestbps)


safe_ratio_calc <- function(chol_val, bp_val) {
  tryCatch({
    if (is.na(bp_val) || bp_val == 0) {
      stop("Denominator is zero or NA.")
    }
    return(chol_val / bp_val)
  }, error = function(e) {
    message("Warning: ", e$message, " Returning NA.")
    return(NA)
  })
}

cat("\n--- Testing tryCatch ---\n")
test_ratio <- safe_ratio_calc(200, 0)

cat("\n--- Validation ---\n")
cat("Missing BP values:", sum(is.na(heart_data$cleaned_trestbps)), "\n")
cat("Min BP:", min(heart_data$cleaned_trestbps, na.rm = TRUE), "\n")
cat("Max BP:", max(heart_data$cleaned_trestbps, na.rm = TRUE), "\n")
cat("Mean BP:", mean(heart_data$cleaned_trestbps, na.rm = TRUE), "\n")
cat("Median BP:", median(heart_data$cleaned_trestbps, na.rm = TRUE), "\n")

write.csv(heart_data, "cleaned_heart_data.csv", row.names = FALSE)