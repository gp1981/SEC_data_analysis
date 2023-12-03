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
    select(standardized_balancesheet_label,everything(),-df_Fact_Description,)
  
  # Filter out records not associated with standardized_balancesheet to create the mapping with df_Facts
  df_std_BS_map <- df_std_BS %>%
    filter(!is.na(standardized_balancesheet_label)) %>% 
    select(standardized_balancesheet_label,label,description) %>% 
    distinct()
  
  # Filter out records not associated with standardized_balancesheet to create the pivot
  df_std_BS <- df_std_BS %>%
    filter(!is.na(standardized_balancesheet_label)) %>% 
    select(standardized_balancesheet_label,end,val,fy,fp,form,filed,start)
  
  
  # Pivot the data to the desired structure
  # This function introduces a grouping by label, fy and fp and adds a logical column (has_form_A and has_form) to identify whether there is a row with /A and a row without /A for the same fy and fp. Rows without /A and with a corresponding row with /A will be filtered out. 
  df_std_BS_test <- df_std_BS %>%
    filter(!is.na(standardized_balancesheet_label)) %>% 
    mutate(end = ymd(end), filed = ymd(filed)) %>%  # convert to date format
    group_by(standardized_balancesheet_label, end) %>%
    filter(filed == max(filed)) %>%  # filter rows with the most recent filing date
    ungroup() %>%
    select(standardized_balancesheet_label, end, val, fy, fp, form, filed, start) %>%
    pivot_wider(
      names_from = standardized_balancesheet_label,
      values_from = val
    ) %>%
    arrange(desc(end))
  # <---- NEED TO CHECK STILL DUPLICATED ROW WITH 1 VARIABLE see 2020-12-31 NA ---->
  # Perform the calculation for additional records in the balancesheet
  df_std_BS <- df_std_BS %>%
    mutate(
      'Total Long Term Assets' = ifelse(
        !is.na('Total Long Term Assets'),
        'Total Assets' - 'Total Current Assets',
        NA_real_
      )
    )
      
      ,
      'Other Current Assets' = ifelse(
        !is.na('Total Current Assets') &
          !is.na('Cash & Cash Equivalent') &
          !is.na('Marketable Securities, Current') &
          !is.na('Total Accounts Receivable') &
          !is.na('Total Inventories'),
        'Total Current Assets' - 
          ('Cash & Cash Equivalent' + 
             'Marketable Securities, Current' + 
             'Total Accounts Receivable' + 
             'Total Inventories'),
        NA_real_
      )
  
  ,
      'Other Long Term Assets' = ifelse(
        !is.na('Total Long Term Assets') &
          !is.na('Marketable Securities, Non Current') &
          !is.na('Property, Plant and Equipment') &
          !is.na('Intangible Assets (excl. goodwill)') &
          !is.na('Goodwill'),
        'Total Long Term Assets' - 
          ('Marketable Securities, Non Current' + 
             'Property, Plant and Equipment' +
             'Intangible Assets (excl. goodwill)' + 
             'Goodwill'),
        NA_real_
      ),
      'Other Current Liabilities' = ifelse(
        !is.na('Total Current Liabilities') &
          !is.na('Accounts Payable') &
          !is.na('Tax Payable') &
          !is.na('Commercial papers') &
          !is.na('Short-Term Debt') &
          !is.na('Operating Lease, Liability, Current'),
        'Total Current Liabilities' - 
          ('Accounts Payable' + 'Tax Payable' + 
             'Commercial papers' + 
             'Short-Term Debt' +
             'Operating Lease, Liability, Current'),
        NA_real_
      ),
      'Other Long Term Liabilities' = ifelse(
        !is.na('Total Long Term Liabilities') &
          !is.na('Long Term Debts') &
          !is.na('Operating Lease, Liability, Non Current') &
          !is.na('Finance Lease, Liability, Non Current'),
        'Total Long Term Liabilities' -
          ('Long Term Debts' + 
             'Operating Lease, Liability, Non Current' + 
             'Finance Lease, Liability, Non Current'),
        NA_real_
      ),
      'Other Company Stockholders Equity' = ifelse(
        !is.na('Total Company Stockholders Equity') &
          !is.na('Common Stock & Additional paid-in capital') &
          !is.na('Common Stock, Value, Issued') &
          !is.na('Additional Paid in Capital') &
          !is.na('Preferred Stock') &
          !is.na('Retained Earnings') &
          !is.na('Accumulated other comprehensive income (loss)'),
        'Total Company Stockholders Equity' -
          ('Common Stock & Additional paid-in capital' + 
             'Common Stock, Value, Issued' + 
             'Additional Paid in Capital' + 
             'Preferred Stock' + 
             'Retained Earnings' +
             'Accumulated other comprehensive income (loss)'),
        NA_real_
      )
    )
  
  # Reorder columns dynamically based on the order in standardized_balancesheet
  column_order <- standardized_balancesheet$standardized_balancesheet_label
  
  # Check if each column in column_order exists in df_std_BS, if not, remove it from the order
  column_order <- column_order[column_order %in% colnames(df_std_BS)]
  
  # Reorder columns for better readability
  df_std_BS <- df_std_BS[, c("end", "fy", "fp", "form", column_order)]
  
 return(df_std_BS)
}


