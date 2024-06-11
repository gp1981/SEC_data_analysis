# Author: gp1981
# Purpose: Contains the script of utility functions for processing, analyzing, and visualizing SEC data.
# Disclaimer: This script is intended for educational purposes only and should not be used for investment decisions. Use at your own risk.

# Import required libraries
library(tidyverse)
library(openxlsx)
library(lubridate)
library(purrr)
library(jsonlite)
library(progress)
library(zoo)

# Function to unnest a list -----------------------------------------------
unnest_list <- function(x) {
  purrr::map(x, ~{
    if (is.list(.x)) {
      unnest_list(.x)
    } else {
      .x
    }
  })
}


# Read and combine SEC JSON files  ----------------------------------------
#'
#' This function reads a specified number of JSON files randomly selected from
#' a folder and combines them into a dataframe.
#' @source("setup.qmd")           # Sourcing necessary libraries

#' @param folder_path The path to the folder containing JSON files.
#' @param num_files_to_select The number of files to randomly select and process.
#'
#' @return A dataframe containing the combined data.
#'
#' @examples
#' \dontrun{
#' result <- read_combine_json_files("/path/to/your/json/files", 20)
#' }
#' 
#' 
# # TEMPORARY CODE (READ SETUP.R)
folder_path <- "~/Downloads/SEC/companyfacts_test"
num_files_to_select = 10
# Currently the dataframe returned for 10k companies in company_List would ~217 GB

df_Facts_multi_files <- function(folder_path, num_files_to_select = NULL) {

  ## Step 1 - preparation of the files ---------------------------------------
  
  # List all the JSON files in the folder
  json_files <- list.files(folder_path, pattern = "*.json", full.names = TRUE)
  
  # # Shuffle the list of files
  # set.seed(123)  # Set seed for reproducibility
  # json_files <- sample(json_files)
  
  # Check if the company_List exist
  if (!exists("company_List")) {
    # Define user headers
    headers <- c('User-Agent' = 'email@address.com')
    
    # Retrieve company list
    company_List <- retrieve_Company_List(headers)
  }
  
  # Create a dataframe of file paths
  files_df <- data.frame(file_path = json_files, stringsAsFactors = FALSE)
  
  # Extract CIK suffix from the file path
  files_df <- files_df %>%
    mutate(cik_suffix = sub(".*/CIK(\\d+)\\.json", "\\1", file_path))
  
  # Filter relevant files based on CIK suffix
  filtered_files_df <- files_df %>%
    filter(cik_suffix %in% company_List$cik_str)
  
  # Join with company_List to filter only those companies belonging to company_List
  filtered_files_df <- filtered_files_df %>%
    left_join(company_List, by = c("cik_suffix" = "cik_str"))
    
    
    ## Step 2 - preparation of the cycle over the files ------------------------
  
  # Initialize an empty dataframe to store the combined data
  combined_df_BS <- data.frame()
  combined_df_IS <- data.frame()
  combined_df_CF <- data.frame()
  
  # Determine the number of files to select
  if (is.null(num_files_to_select)) {
    num_files_to_select <- length(json_files)
  } else if (num_files_to_select < 1 || num_files_to_select > length(json_files)) {
    stop("Invalid value for 'num_files_to_select'.")
  }
  
  # Initialize the progress bar
  pb <- progress_bar$new(format = "[:bar] :percent :eta", total = num_files_to_select)
  
  # Process files
  for (i in 1:num_files_to_select) {
    # Read the JSON data for the current file
    company_Data <- fromJSON(json_files[i], flatten = TRUE)
    
    # Check if the company_Data$cik exists
    if (is.null(company_Data$cik)) {
      message(sprintf("Skipping file %s: Missing 'cik' in the JSON data.", json_files[i]))
      next  # Skip to the next iteration
    }
    
    # Check if the company_Data$entityName is not empty
    if (is.null(company_Data$entityName) || company_Data$entityName == "") {
      message(sprintf("Skipping file %s: 'entityName' is missing or empty in the JSON data.", json_files[i]))
      next  # Skip to the next iteration
    }
    
    # Check if the company_Data$facts is not empty
    if (is.null(company_Data$facts) || length(company_Data$facts) == 0) {
      message(sprintf("Skipping file %s: 'facts' is missing or empty in the JSON data.", json_files[i]))
      next  # Skip to the next iteration
    }
    
    # Identify the cik code of the entity and nsure cik has 10 digits
    company_details_cik <- sprintf("%010d", company_Data$cik)
    
    # Check if the cik is in the company_List
    if (!company_details_cik %in% company_List$cik_str) {
      message(sprintf("Skipping file %s: CIK %s not in the list of public traded operating companies of SEC.", json_files[i], company_details_cik))
      next  # Skip to the next iteration
    }
    
    # Turn the data in the single json file from list to dataframe
    df_Facts <- Fundamentals_to_Dataframe_multi_files(company_Data,
                                                      company_details_cik,
                                                      company_List)
  
    # Increment the progress bar
    pb$tick()
    

    
    # df_std_BS <- BS_std_multi_file(df_Facts)
    # df_std_IS <- IS_std_multi_file(df_Facts) #<<<<<<<< >>>>>>>
    # df_std_CS <- CS_std_multi_file(df_Facts)
#     
#     # Combine the data directly
#     combined_BS_df <- rbind(combined_df, df_Facts)
#     
#     # Increment the progress bar
#     pb$tick()
#     
#     # Return the combined dataframe
    return(combined_df)
  }
}

# Calculate Trailing quarter values ----------------------------------------

# Function to Calculate Trailing Months Value
calculate_trailing_months <- function(df, trailing_months) {
  # Validate trailing_months input
  if (!trailing_months %in% c(3, 6, 9, 12)) {
    stop("Invalid value for trailing_months. Must be one of 3, 6, 9, or 12.")
  }
  
  # Select columns to remove
  columns_to_remove <- c("cik", "entityName", "sic", "sicDescription", "tickers")
  
  # Check if columns exist in the dataframe
  columns_exist <- all(columns_to_remove %in% colnames(df))
  
  if (columns_exist) {
    df <- df %>%
      select(-one_of(columns_to_remove))
  } else {
    message("Columns to remove do not exist in the dataframe.")
  }
  
  # Ensure "end" column is of Date type
  df <- df %>%
    mutate(end = as.Date(end))
  
  # Sort by the "end" date
  df <- df %>%
    arrange(desc(end))
  
  # Calculate number of trailing quarters
  trailing_quarters <- trailing_months / 3
  
  # Apply rolling sum for each column except "end" and rename
  df_rolling <- df %>%
    mutate(across(
      .cols = -end, 
      .fns = ~ rollapply(.x, width = trailing_quarters, FUN = sum, fill = NA, align = "left"),
      .names = "{.col}_{trailing_months}m"
    )) %>%
    select(end, matches(paste0("_", trailing_months, "m$")))
  
  return(df_rolling)
}

# Example Usage
# df <- data.frame(
#   end = as.Date(c("2023-12-31",  "2023-09-30", "2023-06-30", "2023-03-31" )),
#   revenue = c(100, 150, 120, 170),
#   net_income = c(10, 20, 15, 25)
# )
# trailing_df <- trailing_months(df, 6)
# print(trailing_df)


# Export dataframe to excel  ----------------------------------------

dataframe2xlsx <- function(df,name_output) {
  
  ## Create workbook
  wb <- createWorkbook()
  
  ## Add worksheets
  addWorksheet(wb, "Data")
  
  # Write df to "Data" worksheet if provided
  
  writeDataTable(
    wb,
    "Data",
    x = as.data.frame(df),
    colNames = TRUE,
    tableStyle = "TableStyleLight9",
    tableName = "Data_Full_List"
  )
  
  # Format the date without hyphens
  formatted_date <- format(lubridate::today(), "%Y%m%d")
  
  # Save workbook
  saveWorkbook(wb,
               file = paste0("output/",name_output, "_", formatted_date, ".xlsx"),
               overwrite = TRUE)
  # Check https://cran.r-project.org/web/packages/openxlsx/openxlsx.pdf
}


# Calculate Cumulative Values and Transpose DataFrame --------------------

calculate_cumulative <- function(data, months) {
  data %>%
    group_by(year = year(end)) %>%
    mutate(across(where(is.numeric), ~ rollsumr(.x, months, fill = NA, align = "right"))) %>%
    ungroup() %>%
    select(-year)
}

calculate_cumulative_values <- function(df) {
  # Ensure date is sorted
  df <- df %>% arrange(end)
  
  # Calculate cumulative values for 6, 9, and 12 months
  df_6m <- calculate_cumulative(df, 6)
  df_9m <- calculate_cumulative(df, 9)
  df_12m <- calculate_cumulative(df, 12)
  
  # Add suffix to column names to differentiate
  colnames(df_6m) <- paste0(colnames(df), "_6m")
  colnames(df_9m) <- paste0(colnames(df), "_9m")
  colnames(df_12m) <- paste0(colnames(df), "_12m")
  
  # Combine the original and cumulative data frames
  combined_df <- bind_cols(df, df_6m, df_9m, df_12m)
  
  return(combined_df)
}

# Function to transpose DataFrame with column ordering based on another standardized financial statement
transpose_df_standardized <- function(df, order_df) {
  # Define the standardized statement file path
  BS_path <- here(data_dir, "standardized_BS.xlsx")
  IS_path <- here(data_dir, "standardized_IS.xlsx")
  CF_path <- here(data_dir, "standardized_CF.xlsx")
  
  # Read the appropriate standardized order file
  order_df <- switch(order_df,
                     "standardized_BS" = read.xlsx(BS_path, sheet = "Sheet1"),
                     "standardized_IS" = read.xlsx(IS_path, sheet = "Sheet1"),
                     "standardized_CF" = read.xlsx(CF_path, sheet = "Sheet1"),
                     stop("Invalid order_df_name. Must be 'standardized_BS', 'standardized_IS', or 'standardized_CF'.")
  )
  
  
  # Filter applicable records from order_df
  applicable_labels <- unique(order_df$standardized_label) %>% as.data.frame() %>% 
    `colnames<-`("standardized_label")
  
  # Select columns to remove
  columns_to_remove <- c("cik", "entityName", "sic", "sicDescription", "tickers")
  
  # Check if columns exist in the dataframe
  columns_exist <- all(columns_to_remove %in% colnames(df))
  
  if (columns_exist) {
    df <- df %>%
      select(-one_of(columns_to_remove))
  } else {
    message("Columns to remove do not exist in the dataframe.")
  }
  
  # Transpose DataFrame
  df_t <- df %>%
    pivot_longer(cols = -end, names_to = "Financial Item", values_to = "Value") %>%
    pivot_wider(names_from = end, values_from = Value) 
  
  # Filter and reorder df_t$Concept based on applicable_labels
  col_order <- applicable_labels[applicable_labels$standardized_label %in% df_t$`Financial Item`, "standardized_label"]
  
  
  # Reorder rows in df_t based on col_order
  df_t <- df_t %>%
    arrange(match("Financial Item", col_order))
  
  return(df_t)
}
# Example usage with your df_std_IS_CF
# df_std_IS_CF <- calculate_cumulative_values(df_std_IS_CF)
# df_std_IS_CF_t <- transpose_df(df_std_IS_CF)
# 
# # Display transposed dataframe
# print(df_std_IS_CF_t)

# Function to transpose DataFrame with column ordering based on another standardized financial statement
transpose_df <- function(df, order_df) {

  # Transpose DataFrame
  df_t <- df %>%
    pivot_longer(cols = -end, names_to = "Financial Item", values_to = "Value") %>%
    pivot_wider(names_from = end, values_from = Value) 
  
  # Filter and reorder df_t$Concept based on applicable_labels
  col_order <- order_df
  
  
  # Reorder rows in df_t based on col_order
  df_t <- df_t %>%
    arrange(match("Financial Item", col_order))
  
  return(df_t)
}