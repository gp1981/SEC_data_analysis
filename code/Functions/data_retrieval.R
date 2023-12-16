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

# Function to unnest list company_Facts  ---------------------------------------

# Function to unnest list company_Facts using parallel processing. This function takes a list of financial facts (company_Facts_us_gaap) and unnests it, creating a data frame with relevant information including values, labels, descriptions, and references to the original list.

FactsList_to_Dataframe <- function(company_Facts_us_gaap) {
  # Use parallel processing with future_map_dfr to apply the operation on each list concurrently
  df_units <- furrr::future_map_dfr(names(company_Facts_us_gaap), function(list_name) {
    # Extract the relevant information from the 'units' list and create a tibble
    df_list <- company_Facts_us_gaap[[list_name]]$units$USD %>%
      as_tibble() %>%
      # Add columns with 'label', 'description', and 'us_gaap_reference'
      mutate(
        label = company_Facts_us_gaap[[list_name]]$label,
        description = company_Facts_us_gaap[[list_name]]$description,
        us_gaap_reference = list_name
      )
    
    return(df_list)
  })
  
  # Mutate to reduce values in millions by dividing by 1 million
  df_units <- df_units %>%
    mutate(val = val / 1e6)
  
  return(df_units)
}


# Example usage:
# df_Facts_result <- FactsList_to_Dataframe(company_Facts$facts$`us-gaap`)


# Example usage:
# df_Facts_result <- FactsList_to_Dataframe(company_Facts$facts$`us-gaap`)

# Function to rebuild the balancesheet financial statement ---------------------------------------

bs_std <- function(df_Facts) {
  # 01 - Join standardized_balancesheet ------------------------------------------------------
  # Define the standardized balancesheet file path
  balancesheet_path <- here(data_dir, "standardized_balancesheet.xlsx")
  
  # Read the standardized_balancesheet.xlsx file
  standardized_balancesheet <- read.xlsx(balancesheet_path, sheet = "Sheet1")
  
  # Rename standardized_balancesheet column df_Fact_Description to perform left_join
  standardized_balancesheet <- standardized_balancesheet %>% 
    rename(description = df_Facts_Description)
  
  # Merge df_Facts with standardized_balancesheet based on description and period
  df_std_BS <- df_Facts %>%
    left_join(standardized_balancesheet, by = "description") %>%
    select(standardized_balancesheet_label, everything())
  
  # 02 - Data cleaning ------------------------------------------------------
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
    select(end, standardized_balancesheet_label, everything()) %>%
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
  
  # Sum the "val" values for rows with the same standardized_balancesheet_label
  df_std_BS <- df_std_BS %>%
    group_by(end,standardized_balancesheet_label) %>%
    arrange(desc(fy), desc(fp)) %>%  # Arrange by descending fy and fp within each group
    filter(row_number() == 1) %>%    # Keep only the first row within each group
    summarise(val = sum(val, na.rm = TRUE),
              description = paste(description, collapse = "\n")) %>%
    ungroup()
  
  # 03 - Pivot df_std_BS in a dataframe format -----------------------------------
  # This code transforms the data from a long format with multiple rows per observation to a wide format where each observation is represented by a single row with columns corresponding to different labels
  
  df_std_BS <- df_std_BS %>%
    select(end,standardized_balancesheet_label,val) %>% 
    # Pivot the data using standardized_balancesheet_label as column names
    pivot_wider(
      names_from = standardized_balancesheet_label,
      values_from = val
    ) %>%
    # Arrange the dataframe in descending order based on the 'end' column
    arrange(desc(end))
  
  # 04 - Add new columns for standardization -----------------------------------
  # This code add the missing columns to the df_std_BS based on the standardized_balancesheet.xls and perform checks
  
  # Step 1 - identify missing columns from standardized_balancesheet
  df_std_BS_missing <- setdiff(standardized_balancesheet$standardized_balancesheet_label,
                               colnames(df_std_BS)) 
  
  # Step 2 - Check if key financial Facts exist
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
  # It creates a vector columns_to_add containing the names of the columns to add.
  columns_to_add <- c("Total Current Assets", "Total Non Current Assets", "Other Current Assets", "Other Non Current Assets", "Other Current Liabilities", "Other Non Current Liabilities")
  
  # It checks which columns from columns_to_add are not already present in df_std_BS
  columns_to_add <- columns_to_add[!(columns_to_add %in% colnames(df_std_BS))]
  
  #<<<<>>>>> It then adds only the missing columns to df_std_BS and initializes them with NA.
  if (length(columns_to_add) > 0) {
    # Create a tibble with NAs and the columns to add
    columns_to_add_df <- tibble(!!columns_to_add := NA_real_)
    
    # Left join the new columns to df_std_BS
    df_std_BS <- left_join(df_std_BS, columns_to_add_df, by = character())
  }
  
  # Step 4 - evaluate missing columns and other
  df_std_BS <- df_std_BS %>%
    mutate(
      # Replace NAs with empty values
      across(everything(), ~ifelse(is.na(.), NA, as.numeric(.))),
      # Evaluate expressions for newly added columns with NA
      `Total Current Assets` = case_when(
        is.na(`Total Current Assets`) ~ `Total Assets` - `Total Non Current Assets`,
        TRUE ~ `Total Current Assets`
      ),
      `Total Non Current Assets` = case_when(
        is.na(`Total Non Current Assets`) ~ `Total Assets` - `Total Current Assets`,
        TRUE ~ `Total Non Current Assets`
      ),
      `Other Current Assets` = case_when(
        is.na(`Other Current Assets`) ~ `Total Current Assets` - (`Cash & Cash Equivalent` + `Marketable Securities Current` + `Total Accounts Receivable` + `Total Inventory`),
        TRUE ~ `Other Current Assets`
      ),
      `Other Non Current Assets` = case_when(
        is.na(`Other Non Current Assets`) ~ `Total Non Current Assets` - (`Marketable Securities Non Current` + `Property Plant and Equipment` + `Intangible Assets (excl. goodwill)` + `Goodwill`),
        TRUE ~ `Other Non Current Assets`
      ),
      `Total Current Liabilities` = case_when(
        is.na(`Total Current Liabilities`) ~ `Total Liabilities` - `Total Non Current Liabilities`,
        TRUE ~ `Total Current Liabilities`
      ),
      `Other Current Liabilities` = case_when(
        is.na(`Other Current Liabilities`) ~ `Total Current Liabilities` - (`Accounts Payable` + `Tax Payable` +  `Short Term Debt` + `Operating Lease Liability Current` + `Finance Lease Liability Current`),
        TRUE ~ `Other Current Liabilities`
      ),
      `Other Non Current Liabilities` = case_when(
        is.na(`Other Non Current Liabilities`) ~ `Total Non Current Liabilities` - (`Non Current Debts` + `Operating Lease Liability Non Current` + `Finance Lease Liability Non Current`),
        TRUE ~ `Other Non Current Liabilities`
      ),
      `Total Stockholders Equity` = case_when(
        is.na(`Total Stockholders Equity`) ~ `Total Liabilities & Stockholders Equity` - `Total Liabilities`,
        TRUE ~ `Total Stockholders Equity`
      ),
      `Total Liabilities & Stockholders Equity` = case_when(
        is.na(`Total Liabilities & Stockholders Equity`) ~ `Total Assets`,
        TRUE ~ `Total Liabilities & Stockholders Equity`
      )
    )
  
  
  
  # Step 5 - Order columns based on standardized_balancesheet_label
  custom_order <- c(
    "Cash & Cash Equivalent",
    "Marketable Securities Current",
    "Total Inventory",
    "Other Current Assets",
    "Total Current Assets",
    "Marketable Securities Non Current",
    "Property Plant and Equipment",
    "Intangible Assets (excl. goodwill)",
    "Goodwill",
    "Other Non Current Assets",
    "Total Non Current Assets",
    "Total Assets",
    "Accounts Payable",
    "Tax Payable",
    "Short Term Debt",
    "Operating Lease Liability Current",
    "Finance Lease Liability Current",
    "Other Current Liabilities",
    "Total Current Liabilities",
    "Non Current Debts",
    "Operating Lease Liability Non Current",
    "Finance Lease Liability Non Current",
    "Other Non Current Liabilities",
    "Total Non Current Liabilities",
    "Total Liabilities",
    "Preferred Stock",
    "Retained Earnings",
    "Accumulated other comprehensive income (loss)",
    "Minority interest",
    "Total Stockholders Equity",
    "Total Liabilities & Stockholders Equity")
  
  
  df_std_BS <- df_std_BS[, c("end", custom_order)]
  
  return(df_std_BS)
}



