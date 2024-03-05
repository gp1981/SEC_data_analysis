# Author: gp1981
# Purpose: Contains the script of utility functions for processing, analyzing, and visualizing SEC data.
# Disclaimer: This script is intended for educational purposes only and should not be used for investment decisions. Use at your own risk.

# Import required libraries
library(tidyverse)
library(purrr)
library(jsonlite)
library(progress)

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
