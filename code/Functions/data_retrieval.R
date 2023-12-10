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

# Function to rebuild the balancesheet financial statement ---------------------------------------

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
    select(standardized_balancesheet_label, everything())
  
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
    # Select relevant columns
    select(label,standardized_balancesheet_label, end, val) %>%
    # Arrange by descending end date
    arrange(desc(end)) %>% 
    # Remove grouping
    ungroup() %>%
    # Group by and arrange by descending end date within each group
    group_by(label, end) %>%
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
  # This code transforms the data from a long format with multiple rows per observation to a wide format where each observation is represented by a single row with columns corresponding to different labels
  
  df_std_BS <- df_std_BS %>%
    # Pivot the data using standardized_balancesheet_label as column names
    pivot_wider(
      names_from = standardized_balancesheet_label,
      values_from = val
    ) %>%
    # Arrange the dataframe in descending order based on the 'end' column
    arrange(desc(end))
  
  # 03 - Add new columns for standardization of the financial report -----------------------------------
  # This code add the missing columns to the df_std_BS based on the standardized_balancesheet.xls and perform checks
  
  # Step 1 - identify missing columns from standardized_balancesheet
  df_std_BS_missing <- setdiff(standardized_balancesheet$standardized_balancesheet_label, 
                               colnames(df_std_BS)) 
  
  # Step 2 - Checks existance of key financial Facts
  if (!("Total Assets" %in% colnames(df_std_BS)) || !("Total Liabilities" %in% colnames(df_std_BS))) {
    stop("Total Assets or Total Liabilities is missing. The entity is not adequate for financial analysis.")
  }
  
  if (!("Total Liabilities & Stockholders Equity" %in% colnames(df_std_BS))) {
    stop("Total Liabilities & Stockholders Equity is missing. The entity is not adequate for financial analysis.")
  }
  
  if (!("Total Current Assets" %in% colnames(df_std_BS)) && !("Total Non Current Assets" %in% colnames(df_std_BS))) {
    stop("Both Total Current Assets and Total Non Current Assets are missing. The entity is not adequate for financial analysis.")
  }
  
  if (!("Total Current Liabilities" %in% colnames(df_std_BS)) && !("Total Non Current Liabilities" %in% colnames(df_std_BS))) {
    stop("Both Total Current Liabilities and Total Non Current Liabilities are missing. The entity is not adequate for financial analysis.")
  }
  
  # Step 3 - add missing columns
  df_std_BS[,c(df_std_BS_missing)] <- NA
  
  # It creates a vector columns_to_add containing the names of the columns you want to add.
  columns_to_add <- c("Total Current Assets", "Total Non Current Assets", "Other Current Assets", "Other Non Current Assets","Other Current Liabilities","Other Non Current Liabilities")
  
  # It checks which columns from columns_to_add are not already present in df_std_BS using the !(columns_to_add %in% colnames(df_std_BS)) condition.
  columns_to_add <- columns_to_add[!(columns_to_add %in% colnames(df_std_BS))]
  
  # It then adds only the missing columns to df_std_BS and initializes them with NA.
  if (length(columns_to_add) > 0) {
    df_std_BS[, columns_to_add] <- NA
  }
  
  # Step 4 - evaluate missing columns and other
  df_std_BSt <- df_std_BS %>%
    mutate(
      # Evaluate expressions for newly added columns with NA
      `Total Current Assets` = ifelse(is.na(`Total Current Assets`), `Total Assets` - `Total Non Current Assets`, `Total Current Assets`),
      `Total Non Current Assets` = ifelse(is.na(`Total Non Current Assets`), `Total Assets` - `Total Current Assets`, `Total Non Current Assets`),
      `Other Current Assets` = ifelse(is.na(`Other Current Assets`), `Total Current Assets` - (`Cash & Cash Equivalent` + `Marketable Securities Current` + `Total Accounts Receivable` + `Total Inventory`), `Other Current Assets`),
      `Other Non Current Assets` = ifelse(is.na(`Other Non Current Assets`), `Total Non Current Assets` - (`Marketable Securities Non Current` + `Property Plant and Equipment` + `Intangible Assets (excl. goodwill)` + `Goodwill`), `Other Non Current Assets`),
      `Total Current Liabilities` = ifelse(is.na(`Total Current Liabilities`), `Total Liabilities` - `Total Non Current Liabilities`, `Total Current Liabilities`),
      `Other Current Liabilities` = ifelse(is.na(`Other Current Liabilities`), `Total Current Liabilities` - (`Accounts Payable` + `Tax Payable` +  `Short Term Debt` + `Operating Lease Liability Current` + `Finance Lease Liability Current`), `Other Current Liabilities`),
      `Other Non Current Liabilities` = ifelse(is.na(`Other Non Current Liabilities`), `Total Non Current Liabilities` - (`Non Current Debts` + `Operating Lease Liability Non Current` + `Finance Lease Liability Non Current`), `Other Non Current Liabilities`),
      `Total Stockholders Equity` = ifelse(is.na(`Total Stockholders Equity`), `Total Liabilities & Stockholders Equity` - `Total Liabilities`, `Total Stockholders Equity`),
      `Total Liabilities & Stockholders Equity` = ifelse(is.na(`Total Liabilities & Stockholders Equity`), `Total Assets`, `Total Liabilities & Stockholders Equity`)
    )
  
  # Step 5 - Order columns based on standardized_balancesheet_label
  column_order <- standardized_balancesheet$standardized_balancesheet_label
  df_std_BS <- df_std_BS[, c("end", column_order)]
  
  return(df_std_BS)
}



