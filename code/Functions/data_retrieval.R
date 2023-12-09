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
  # This code filters rows in df_std_BS based on the presence of "/A" in the 'form' column, ensuring rows with "/A" are retained if any in their group have it. It then selects relevant columns, arranges by descending 'end' date, and for each unique 'val', keeps the row with the most recent 'end' date.
  
  df_std_BS <- df_std_BS %>%
    # Filter out rows without standardized_balancesheet_label
    filter(!is.na(standardized_balancesheet_label)) %>% 
    # Group by fiscal year (fy), fiscal period (fp), and label
    group_by(fy, fp, label) %>%
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
    # Remove the temporary column
    select(-has_form_A) %>%
    # Select relevant columns
    select(standardized_balancesheet_label, end, val, fy, fp, form, filed, start) %>%
    # Arrange by descending end date
    arrange(desc(end)) %>% 
    # Remove grouping
    ungroup() %>% 
    # Remove column label
    select(-label) %>%
    # Group by val and arrange by descending end date within each group
    group_by(val) %>%
    arrange(desc(end)) %>%
    # Retain only the first row within each group
    slice_head(n = 1) %>%
    # Remove grouping
    ungroup()

  # Ensures that for each standardized_balancesheet_label and end combination, only the row with the most recent filing date is retained
  
  # Clear the dataframe with the most recent form for each end period
  df_std_BS <- df_std_BS %>%
    # Filter out rows without standardized_balancesheet_label
    filter(!is.na(standardized_balancesheet_label)) %>% 
    # Convert 'end' and 'filed' columns to date format using lubridate (ymd function)
    mutate(end = ymd(end), filed = ymd(filed)) %>%
    # Group by standardized_balancesheet_label and end date
    group_by(standardized_balancesheet_label, end) %>%
    # Filter rows with the most recent filing date within each group
    filter(filed == max(filed)) %>%
    # Remove grouping to perform further operations
    ungroup() %>%
    # Select relevant columns
    select(standardized_balancesheet_label, end, val, fy, fp, form, filed, start)
  
  # 02 - Mapping with df_Facts--------------------------------------------------------------
  # Creates a mapping (df_std_BS_map) by matching standardized_balancesheet_label from df_std_BS with the corresponding description from df_Facts.
  
  # Create a map with df_Facts
  df_std_BS_map <- df_std_BS %>%
    # Select the standardized_balancesheet_label column
    select(standardized_balancesheet_label) %>% 
    # Rename the column to 'label'
    rename(label = standardized_balancesheet_label) %>% 
    # Perform a left join with df_Facts using the 'label' column
    left_join(df_Facts, by = "label") %>%
    # Rename the 'label' column back to 'standardized_balancesheet_label'
    rename(standardized_balancesheet_label = label) %>%
    # Select columns for mapping
    select(standardized_balancesheet_label, description) %>% 
    # Retain distinct combinations
    distinct()
  
  # 03 - Pivot df_std_BS in a dataframe format -----------------------------------
  # Transforms your data from a long format with multiple rows per observation to a wide format where each observation is represented by a single row with columns corresponding to different labels
  
  # Build df_std_BS dataframe pivoting the standardized labels into columns
  df_std_BS <- df_std_BS %>%
    # Pivot the data using standardized_balancesheet_label as column names
    pivot_wider(
      names_from = standardized_balancesheet_label,
      values_from = val
    ) %>%
    # Arrange the dataframe in descending order based on the 'end' column
    arrange(desc(end))
  
  # >>>---- CONTINUE HERE - CHECK RESULTS ----<<<<<
  # There are instances in which the filing includes comparison with previous reporting period. In such instances additional details of the previous reporting period are included. The following code merge the row with referring to the same period end where these additional details are provided.
  
  # 04 - Add missing columns (Facts) ----------------------------------------
  
  # For the same "fy", "fp", we need to add to df_std_BS the following rows before the pivot_wider:
  #   
  #   new row: if Total Current Assets does not exist then it creates a new row whose val is Total Assets - Total Long Term Assets and standardized_balancesheet_label is Total Current Assets
  # 
  # new row: if Other Current Assets does not exist then it creates a new row whose val is Total Current Assets - (Cash and Cash Equivalent + Marketable Securities, Current  + Total Accounts Receivable + Total Inventory) and standardized_balancesheet_label is Other Current Assets 
  # 
  # new row: if Total Long Term Assets does not exist then it creates a new row whose val is  val of Total Assets - val of Total Current Assets and standardized_balancesheet_label is Total Long Term Assets
  # 
  # new row: if Other Non Current Assets  does not exist then it creates a new row whose val is val of  Total Long Term Assets - val of (Marketable Securities, Non Current + Property, Plant and Equipment+ Intangible Assets (excl. goodwill) + Goodwill) and standardized_balancesheet_label is Other Non Current Assets 
  # 
  # new row: if Total Current Liabilities  does not exist then it creates a new row whose val is  val of Liabilities - val of Liabilities, Non Current and standardized_balancesheet_label is Total Current Liabilities 
  # 
  # new row: if Other Current Liabilities does not exist then it creates a new row whose val is val of  Total Current Liabilities - val of  (Accounts Payable, Current + Taxes Payable, Current + Commercial Paper + Long Term Debt, Current Maturities + Operating Lease, Liability, Current + Finance Lease, Liability, Current) and standardized_balancesheet_label is  Other Current Liabilities
  # 
  # new row: if Total Long Term Liabilities does not exist then it creates a new row whose val is  val of Total Liabilities - val of Total Current Liabilities and standardized_balancesheet_label is  Total Long Term Liabilities
  # 
  # 
  # new row: if Other Long Term Liabilities does not exist then it creates a new row whose val is val of  Total Long Term Liabilities - val of (Long Term Debts - Operating Lease, Liability, Non Current + Finance Lease, Liability, Non Current and standardized_balancesheet_label is  Other Long Term Liabilities 
  #
  # new row: if Other Stockholders Equity does not exist then it creates a new row whose val is val of   Total Company Stockholders Equity - val of  (Common Stock + Additional Paid in Capital + Preferred Stock + Retained Earnings + Accumulated other comprehensive income (loss)) and standardized_balancesheet_label is  Other Stockholders Equity   
  
  return(df_std_BS)
}



