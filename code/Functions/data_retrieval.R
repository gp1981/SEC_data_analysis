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
  
  # Filter out records not associated with standardized_balancesheet to create the mapping with df_Facts
  df_std_BS_map <- df_std_BS %>%
    filter(!is.na(standardized_balancesheet_label)) %>% 
    select(standardized_balancesheet_label, label, description) %>% 
    distinct()
  
  # Clean up the df_std_BS by retaining the latest amended financials with form e.g. "10K/A"
  df_std_BS <- df_std_BS %>%
    group_by(fy, fp, label) %>%
    arrange(desc(end)) %>%
    mutate(
      has_form_A = any(grepl("/A$", form))
    ) %>%
    filter(!has_form_A | (has_form_A & grepl("/A$", form))) %>%
    select(-has_form_A) %>%
    ungroup() %>%
    select(-label, -description, standardized_balancesheet_label, end, val, fy, fp, form, filed, start)
  
  
  df_std_BS <- df_std_BS %>%
    filter(!is.na(standardized_balancesheet_label)) %>% 
    mutate(end = ymd(end), filed = ymd(filed)) %>%  # convert to date format
    group_by(standardized_balancesheet_label, end) %>%
    filter(filed == max(filed)) %>%  # filter rows with the most recent filing date
    ungroup() %>%
    select(standardized_balancesheet_label, end, val, fy, fp, form, filed, start)
  
  
  # Build df_std_BS dataframe pivoting the standardized labels into columns
  df_std_BS_pivoted <- df_std_BS%>%
    pivot_wider(
      names_from = standardized_balancesheet_label,
      values_from = val
    ) %>%
    arrange(desc(end))
  
  return(df_std_BS_pivoted)
}



