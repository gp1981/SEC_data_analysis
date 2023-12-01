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

bs_std <- function(df_Facts, balancesheet_path) {
  # Read the standardized_balancesheet.xlsx file
  standardized_balancesheet <- read.xlsx(balancesheet_path, sheet = "Sheet1")
  
  
  # Select relevant columns from df_Facts
  df_Facts_subset <- df_Facts %>%
    select(label, end, fy, fp, form, val)
  
  # Merge with standardized_balancesheet to get the corresponding df_Facts_label
  df_std_BS <- df_Facts_subset %>%
    left_join(standardized_balancesheet, by = c("label" = "df_Facts_label"))
  
  # Filter out records not associated with standardized_balancesheet
  df_std_BS <- df_std_BS %>% 
    filter(!is.na(standardized_balancesheet_label)) %>% 
    select(-c(label, df_Fact_Description))
  
  # Pivot the data to the desired structure
  df_std_BS <- df_std_BS %>%
    pivot_wider(
      names_from = standardized_balancesheet_label,
      values_from = val
    ) %>% 
    arrange(desc(end))
  
  # Reorder columns dynamically based on the order in standardized_balancesheet
  column_order <- standardized_balancesheet$standardized_balancesheet_label
  
  # Check if each column in column_order exists in df_std_BS, if not, remove it from the order
  column_order <- column_order[column_order %in% colnames(df_std_BS)]
  
  # Reorder columns for better readability
  df_std_BS <- df_std_BS[, c("end", "fy", "fp", "form", column_order)]
  
 return(df_std_BS)
}


