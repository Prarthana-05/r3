options(repos = c(CRAN = "https://cloud.r-project.org/"))
install.packages(c("readr", "jsonlite", "readxl", "dplyr", "DBI", "RSQLite"), type = "binary")
install.packages(c("readxl", "readr", "jsonlite", "writexl", "dplyr"), type= "binary")
library(readr)
library(jsonlite)
library(readxl)
library(dplyr)
library(DBI)
library(RSQLite)
library(writexl)

raw_data <- read_excel("C:\\Users\\YASH\\Desktop\\Yash\\R_programming\\R_Programming_1\\Lab_3\\Online Retail.xlsx")


transactions <- raw_data %>%
  select(InvoiceNo, StockCode, CustomerID, Quantity, InvoiceDate)
write_csv(transactions, "C:\\Users\\YASH\\Desktop\\Yash\\R_programming\\R_Programming_1\\Lab_3\\transactions.csv")


products <- raw_data %>%
  select(StockCode, Description, UnitPrice) %>%
  distinct(StockCode, .keep_all = TRUE)
write_json(products, "C:\\Users\\YASH\\Desktop\\Yash\\R_programming\\R_Programming_1\\Lab_3\\products.json", pretty = TRUE)


customers <- raw_data %>%
  select(CustomerID, Country) %>%
  filter(!is.na(CustomerID)) %>%
  distinct(CustomerID, .keep_all = TRUE)
write_xlsx(customers, "C:\\Users\\YASH\\Desktop\\Yash\\R_programming\\R_Programming_1\\Lab_3\\customers.xlsx")

cat("Generated: transactions.csv, products.json, and customers.xlsx\n")

transactions <- read_csv("C:\\Users\\YASH\\Desktop\\Yash\\R_programming\\R_Programming_1\\Lab_3\\transactions.csv")
products <- fromJSON("C:\\Users\\YASH\\Desktop\\Yash\\R_programming\\R_Programming_1\\Lab_3\\products.json")
customers <- read_excel("C:\\Users\\YASH\\Desktop\\Yash\\R_programming\\R_Programming_1\\Lab_3\\customers.xlsx")

transactions_clean <- transactions %>%
  filter(!is.na(CustomerID), !is.na(StockCode), Quantity > 0) %>%
  distinct()

products_clean <- products %>%
  filter(!is.na(StockCode), UnitPrice > 0) %>%
  distinct()

customers_clean <- customers %>%
  filter(!is.na(CustomerID)) %>%
  distinct()

integrated_data <- transactions_clean %>%
  inner_join(products_clean, by = "StockCode") %>%
  inner_join(customers_clean, by = "CustomerID")

integrated_data <- integrated_data %>%
  mutate(Revenue = Quantity * UnitPrice)

print(dim(integrated_data))

unmatched_products <- anti_join(transactions_clean, products_clean, by = "StockCode")
unmatched_customers <- anti_join(transactions_clean, customers_clean, by = "CustomerID")

cat("Unmatched transaction records due to missing products:", nrow(unmatched_products), "\n")
cat("Unmatched transaction records due to missing customers:", nrow(unmatched_customers), "\n")

total_revenue <- sum(integrated_data$Revenue, na.rm = TRUE)
cat("Total Sales Revenue: $", total_revenue, "\n")

top_5_products <- integrated_data %>%
  group_by(Description) %>%
  summarise(Total_Revenue = sum(Revenue)) %>%
  arrange(desc(Total_Revenue)) %>%
  head(5)

top_5_countries <- integrated_data %>%
  group_by(Country) %>%
  summarise(Total_Revenue = sum(Revenue)) %>%
  arrange(desc(Total_Revenue)) %>%
  head(5)

top_5_customers <- integrated_data %>%
  group_by(CustomerID) %>%
  summarise(Total_Purchase_Value = sum(Revenue)) %>%
  arrange(desc(Total_Purchase_Value)) %>%
  head(5)

customer_summary <- integrated_data %>%
  group_by(CustomerID, Country) %>%
  summarise(Total_Purchase_Value = sum(Revenue), .groups = 'drop') %>%
  mutate(Customer_Segment = case_when(
    Total_Purchase_Value < 500 ~ "Low Value",
    Total_Purchase_Value >= 500 & Total_Purchase_Value < 2000 ~ "Medium Value",
    Total_Purchase_Value >= 2000 & Total_Purchase_Value < 5000 ~ "High Value",
    Total_Purchase_Value >= 5000 ~ "Premium",
    TRUE ~ "Unknown"
  ))


con <- dbConnect(RSQLite::SQLite(), "C:\\Users\\YASH\\Desktop\\Yash\\R_programming\\R_Programming_1\\Lab_3\\retail_database.sqlite")
dbWriteTable(con, "retail_sales", integrated_data, overwrite = TRUE)

query_top_customers <- "
  SELECT CustomerID, SUM(Revenue) AS TotalRevenue 
  FROM retail_sales 
  GROUP BY CustomerID 
  ORDER BY TotalRevenue DESC 
  LIMIT 5;
"
sql_top_customers <- dbGetQuery(con, query_top_customers)
print(sql_top_customers)

query_revenue_country <- "
  SELECT Country, SUM(Revenue) AS TotalRevenue 
  FROM retail_sales 
  GROUP BY Country 
  ORDER BY TotalRevenue DESC;
"
sql_revenue_country <- dbGetQuery(con, query_revenue_country)
print(sql_revenue_country)

dbDisconnect(con)
