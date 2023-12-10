# Author: gp1981
# Purpose: Contains the script to retrieve company data from SEC filings.
# Disclaimer: This script is intended for educational purposes only and should not be used for investment decisions. Use at your own risk.

# Function to retrieve list of companies ----------------------------------

retrieve_Company_List <- function(headers) {
  # Retrieve company tickers list
  company_Tickers <- GET("https://www.sec.gov/files/company_tickers.json", add_headers(headers))
  
  # Check for HTTP errors
  if (http_error(company_Tickers)) {
    stop("Failed to retrieve company list. HTTP error: ", http_status(company_Tickers)$message)
  }
  
  # Proceed with data extraction
  company_Tickers_List <- fromJSON(httr::content(company_Tickers, as = "text"))
  
  # Convert the JSON list to a data frame
  company_List <- as.data.frame(t(sapply(company_Tickers_List, unlist)), stringsAsFactors = FALSE)
  
  # Add zeros to CIK
  company_List$cik_str <- sprintf("%010s", company_List$cik_str)
  
  return(company_List)
}


# Function to retrieve company data ---------------------------------------

retrieve_Company_Data <- function(headers, cik) {
  # Retrieve company metadata
  company_Metadata <- GET(paste0("https://data.sec.gov/submissions/CIK", cik, ".json"), add_headers(headers))
  
  # Check for HTTP errors in company_Metadata request
  if (http_error(company_Metadata)) {
    stop("Failed to retrieve company metadata. HTTP error: ", http_status(company_Metadata)$message)
  }
  
  # Process and adjust JSON data
  company_Metadata <- fromJSON(httr::content(company_Metadata, as = "text"))
  
  # Retrieve company facts
  company_Facts <- GET(paste0("https://data.sec.gov/api/xbrl/companyfacts/CIK", cik, ".json"), add_headers(headers))
  
  # Check for HTTP errors in company_Facts request
  if (http_error(company_Facts)) {
    stop("Failed to retrieve company facts. HTTP error: ", http_status(company_Facts)$message)
  }
  
  # Process and adjust JSON data
  company_Facts <- fromJSON(httr::content(company_Facts, as = "text"))
  
  # Retrieve company facts
  company_Concept <- GET(paste0("https://data.sec.gov/api/xbrl/companyconcept/CIK", cik, "/us-gaap/Assets.json"), add_headers(headers))
  
  # Check for HTTP errors in company_Concept request
  if (http_error(company_Concept)) {
    stop("Failed to retrieve company concept data. HTTP error: ", http_status(company_Concept)$message)
  }
  
  # Process and adjust JSON data
  company_Concept <- fromJSON(httr::content(company_Concept, as =  "text"))
  
  # Prepare output
  company_Data <- list(
    company_Metadata = company_Metadata,
    company_Facts = company_Facts,
    company_Concept = company_Concept
  )
  return(company_Data)
}

bs_std <- function(df_Facts) {
  # Define the standardized balancesheet file path
  balancesheet_path <- here(data_dir, "standardized_balancesheet.xlsx")
  
  # Read the standardized_balancesheet.xlsx file
  standardized_balancesheet <- read.xlsx(balancesheet_path, sheet = "Sheet1")
  
  # Rename standardized_balancesheet column df_Facts_label to perform left_join
  standardized_balancesheet <- standardized_balancesheet %>% 
    rename(label = df_Facts_label)
  
  # Merge df_Facts with standardized_balancesheet
  df_std_BS <- df_Facts %>%
    left_join(standardized_balancesheet, by = "label") %>% 
    select(standardized_balancesheet_label, everything(), -df_Fact_Description)
  
  # 01 - Data cleaning ------------------------------------------------------
  # This code filters rows in df_std_BS based on whether there's a "/A" in the 'form' column. Rows with "/A" are retained if any row in their group contains it. Relevant columns are selected, the data is arranged by descending 'end' date,  and for each unique 'val', the row with the most recent 'end' date is kept.
  
  
  df_std_BS <- df_std_BS %>%
    # Filter out rows without standardized_balancesheet_label
    filter(!is.na(standardized_balancesheet_label)) %>% 
    # Group by end period (end) and label
    group_by(end, label) %>%
    # Arrange by descending end date within each group
    arrange(desc(end)) %>%
    # Add a column indicating if any row in the group has a form ending with /A
    mutate(
      has_form_A = any(grepl("/A$", form))
    ) %>%
    # Filter rows based on the condition:
    # - Retain rows without /A
    # - Retain rows with /A if there's at least one row with /A in the group
    filter(!has_form_A | (has_form_A & grepl("/A$", form))) %>%
    # Remove the temporary column and non-relevant attributes
    select(-has_form_A, -fy, -fp, -form, -filed, -start) %>%
    # Select relevant columns
    select(standardized_balancesheet_label, end, val) %>%
    # Arrange by descending end date
    arrange(desc(end)) %>% 
    # Remove grouping
    ungroup() %>%
    # Group by val and arrange by descending end date within each group
    group_by(val) %>%
    arrange(desc(end)) %>%
    # Retain only the first row within each group
    slice_head(n = 1) %>%
    # Remove grouping
    ungroup()
  
  # This code ensures that for each standardized_balancesheet_label and end combination, only the row with the most recent filing date is retained
  
  # Clear the dataframe with the most recent form for each end period
  df_std_BS <- df_std_BS %>%
    # Filter out rows without standardized_balancesheet_label
    filter(!is.na(standardized_balancesheet_label)) %>% 
    # Convert 'end' column to date format using lubridate (ymd function)
    mutate(end = ymd(end),) %>%
    # Group by standardized_balancesheet_label and end date
    group_by(standardized_balancesheet_label, end) %>%
    # Filter rows with the most recent filing date within each group
    filter(end == max(end)) %>%
    # Remove grouping to perform further operations
    ungroup() %>%
    # Select relevant columns
    select(end,standardized_balancesheet_label, val)
  
  # This code ensures that for each unique combination of label and end, only the row with the highest "val" is kept, effectively handling duplicates in the dataset
  df_std_BS <- df_std_BS %>%
    # Group by standardized_balancesheet_label, end, and arrange by descending val
    group_by(standardized_balancesheet_label, end) %>%
    arrange(desc(val)) %>%
    # Retain only the first row within each group (highest val)
    slice_head(n = 1) %>%
    # Remove grouping for further operations
    ungroup()
  
  
  # 02 - Pivot df_std_BS in a dataframe format -----------------------------------
  # Transforms your data from a long format with multiple rows per observation to a wide format where each observation is represented by a single row with columns corresponding to different labels
  
  # Build df_std_BS dataframe pivoting the standardized labels into columns
  df_std_BS <- df_std_BS %>%
    mutate({
      # Check if both Total Long Term Assets and Total Current Assets exist
      if (!exists("Total Long Term Assets") && !exists("Total Current Assets")) {
        stop("This entity does not have a standard filing style and therefore it is excluded from the analysis")
      }
      
      # If Total Long Term Assets does not exist and Total Current Assets exists, calculate it
      if (!exists("Total Long Term Assets")) {
        `Total Long Term Assets` = `Total Assets` - `Total Current Assets`
      }
      
      # If Total Current Assets does not exist, create a new column
      if (!exists("Total Current Assets")) {
        `Total Current Assets` = `Total Assets` - `Total Long Term Assets`
      }
      
      # If Other Current Assets does not exist, create a new column
      `Other Current Assets` = if (!exists("Other Current Assets")) `Total Current Assets` - (coalesce(`Cash & Cash Equivalent`, 0) + coalesce(`Marketable Securities, Current`, 0) + coalesce(`Total Accounts Receivable`, 0) + coalesce(`Total Inventory`, 0)) else `Other Current Assets`
      
      # If Other Non Current Assets does not exist, create a new column
      `Other Non Current Assets` = if (!exists("Other Non Current Assets")) `Total Long Term Assets` - (coalesce(`Marketable Securities, Non Current`, 0) + coalesce(`Property, Plant and Equipment`, 0) + coalesce(`Intangible Assets (excl. goodwill)`, 0) + coalesce(`Goodwill`, 0)) else `Other Non Current Assets`
      
      # Check if both Total Long Term Liabilities and Total Current Liabilities exist
      if (!exists("Total Long Term Liabilities") && !exists("Total Current Liabilities")) {
        stop("This entity does not have a standard filing style and therefore it is excluded from the analysis")
      }
      
      # If Total Long Term Liabilities does not exist and Total Current Liabilities exists, calculate it
      if (!exists("Total Long Term Liabilities")) {
        `Total Long Term Liabilities` = `Total Liabilities` - `Total Current Liabilities`
      }
      
      # If Total Current Liabilities does not exist, create a new column
      if (!exists("Total Current Liabilities")) {
        `Total Current Liabilities` = `Total Liabilities` - `Total Long Term Liabilities`
      }
      
      # If Other Current Liabilities does not exist, create a new column
      `Other Current Liabilities` = if (!exists("Other Current Liabilities")) `Total Current Liabilities` - (coalesce(`Accounts Payable`, 0) + coalesce(`Tax Payable`, 0) + coalesce(`Commercial papers`, 0) + coalesce(`Short-Term Debt`, 0) + coalesce(`Operating Lease, Liability, Current`, 0) + coalesce(`Finance Lease, Liability, Current`, 0)) else `Other Current Liabilities`
      
      # If Other Long Term Liabilities does not exist, create a new column
      `Other Long Term Liabilities` = if (!exists("Other Long Term Liabilities")) `Total Long Term Liabilities` - (coalesce(`Long Term Debts`, 0) + coalesce(`Operating Lease, Liability, Non Current`, 0) + coalesce(`Finance Lease, Liability, Non Current`, 0)) else `Other Long Term Liabilities`
      
      # Check if Total Company Stockholders Equity exists
      if (!exists("Total Company Stockholders Equity")) {
        stop("This entity does not have a standard filing style and therefore it is excluded from the analysis")
      }
      
      # If Other Stockholders Equity does not exist, create a new column
      `Other Stockholders Equity` = if (!exists("Other Stockholders Equity")) `Total Company Stockholders Equity` - (coalesce(`Common Stock & Additional paid-in capital`, 0) + coalesce(`Common Stock, Value, Issued`, 0) + coalesce(`Additional Paid in Capital`, 0) + coalesce(`Preferred Stock`, 0) + coalesce(`Retained Earnings`, 0) + coalesce(`Accumulated other comprehensive income (loss)`, 0))
    })

  return(df_std_BS)
}



