# Author: gp1981
# Purpose: Contains the script to retrieve company data from SEC filings.
# Disclaimer: This script is intended for educational purposes only and should not be used for investment decisions. Use at your own risk.

# Function to retrieve list of companies ----------------------------------
# Function to operating companies from SEC database.

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
# Function to retrieve company data based on cik code from SEC database.

retrieve_Company_Data <- function(headers, cik) {
  # Retrieve company metadata
  company_Metadata <- GET(paste0("https://data.sec.gov/submissions/CIK", cik, ".json"), add_headers(headers))
  
  # Check for HTTP errors in company_Metadata request
  if (http_error(company_Metadata)) {
    stop("Failed to retrieve company metadata. HTTP error: ", http_status(company_Metadata)$message)
  }
  
  # Process and adjust JSON data
  company_Metadata <- fromJSON(httr::content(company_Metadata, as = "text"))
  
  # Retrieve company Facts
  company_Facts <- GET(paste0("https://data.sec.gov/api/xbrl/companyfacts/CIK", cik, ".json"), add_headers(headers))
  
  # Check for HTTP errors in company_Facts request
  if (http_error(company_Facts)) {
    stop("Failed to retrieve company facts. HTTP error: ", http_status(company_Facts)$message)
  }
  
  # Process and adjust JSON data
  company_Facts <- fromJSON(httr::content(company_Facts, as = "text"))
  
  # Retrieve company Concepts
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

# Function to create a dataframe of fundamentals  ---------------------------------------
# Function to unnest list company_Facts using parallel processing. This function takes company_Data and unnests it, creating a data frame with relevant information including values, labels, descriptions, etc.

Fundamentals_to_Dataframe <- function(company_Data) {
  # Ensure cik has 10 digits
  company_details_cik <- sprintf("%010d", company_Data$company_Facts$cik)
  
  # Extract tickers separately
  company_details_ticker <- company_Data$company_Metadata$tickers[1]
  
  # Create a vector with the modified details
  company_details <- c(
    company_details_cik,
    company_Data$company_Facts$entityName,
    company_Data$company_Metadata$sic,
    company_Data$company_Metadata$sicDescription,
    company_details_ticker
  )
  
  # Create a data frame with the details
  details_df <- as.data.frame(t(company_details))
  colnames(details_df) <- c("cik", "entityName", "sic", "sicDescription","tickers")
  
  # Retrieve company_Facts data
  company_Facts_us_gaap <- company_Data$company_Facts$facts$`us-gaap`
  
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
  
  # Replicate details_df to match the number of rows in df_units
  details_replicated <- details_df[rep(seq_len(nrow(details_df)), each = nrow(df_units)), ]
  
  # Bind the two data frames together
  df_units <- bind_cols(df_units, details_replicated)
  
  
  # Mutate to reduce values in millions by dividing by 1 million
  df_units <- df_units %>%
    mutate(
      val = val / 1e6
    )
  
  
  return(df_units)
}

# Function to create a dataframe of fundamentals from json SEC files ---------------------------------------
# Function to unnest list company_Facts using parallel processing. This function takes company_Data and unnests it, creating a data frame with relevant information including values, labels, descriptions, etc.

Fundamentals_to_Dataframe_multi_files <- function(company_Data,company_details_cik,company_List) {
  
  # # Create a vector with the modified details
  # company_details <- c(
  #   company_details_cik,
  #   company_Data$company_Facts$entityName,
  #   company_Data$company_Metadata$sic,
  #   company_Data$company_Metadata$sicDescription,
  #   company_details_ticker
  # )
  
  # # Create a data frame with the details
  # details_df <- as.data.frame(t(company_details))
  # colnames(details_df) <- c("cik", "entityName", "sic", "sicDescription","tickers")
  
  # Retrieve company_Facts data
  company_Facts_us_gaap <- company_Data$facts$`us-gaap`
  
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
  
  # Add the corresponding ticker to the dataframe
  df_units <- df_units %>% 
    mutate(
      entityName = company_Data$entityName,
      cik = company_details_cik,
      tickers = company_List[company_List$cik_str == company_details_cik,]$cik_str)
  
  # # Replicate details_df to match the number of rows in df_units
  # details_replicated <- details_df[rep(seq_len(nrow(details_df)), each = nrow(df_units)), ]
  # 
  # # Bind the two data frames together
  # df_units <- bind_cols(df_units, details_replicated)
  # 
  
  # Mutate to reduce values in millions by dividing by 1 million
  df_units <- df_units %>%
    mutate(
      val = val / 1e6
    )
  
  
  return(df_units)
}

# Function to rebuild the balancesheet statement ---------------------------------------
# Function to create a dataframe representative of the quarterly balance sheet of the entity. The basis for the dataframe is a standardized balance sheet (standardized_balancesheet.xlsx). 

BS_std <- function(df_Facts) {
  # 01 - Join standardized_balancesheet ------------------------------------------------------
  # Define the standardized balancesheet file path
  BS_path <- here(data_dir, "standardized_BS.xlsx")
  
  # Read the standardized_balancesheet.xlsx file
  standardized_BS <- read.xlsx(BS_path, sheet = "Sheet1")
  
  # Rename standardized_balancesheet column df_Fact_Description to perform left_join
  standardized_BS <- standardized_BS %>% 
    rename(description = df_Facts_Description)
  
  # Merge df_Facts with standardized_balancesheet based on description and period
  df_std_BS <- df_Facts %>%
    left_join(standardized_BS, by = "description") %>%
    filter(Financial.Report == "BS") %>% 
    select(standardized_label, everything())
  
  # 02 - Data cleaning ------------------------------------------------------
  # This code filters rows based on whether there's a "/A" in the 'form' column. Rows with "/A" are retained if any row in their group contains it. Relevant columns are selected.
  
  # Change format of start and end dates from characters to date
  df_std_BS <- df_std_BS %>%
    mutate(end = as.Date(end),
           start = as.Date(start),
           filed = as.Date(filed),
           val = as.numeric(val))
  
  df_std_BS <- df_std_BS %>%
    # Filter out rows without standardized_label
    filter(!is.na(standardized_label)) %>% 
    # Group by end period (end) and label
    group_by(end, description) %>%
    # Arrange by descending end date within each group
    arrange(desc(end)) %>%
    # Add a column indicating if any row in the group has a form ending with /A
    mutate(
      has_form_A = grepl("/A$", form)
    ) %>%
    # Filter rows based on the condition:
    # - Retain rows without /A
    # - Retain rows with /A if there's at least one row with /A in the group
    filter(!has_form_A | (has_form_A & grepl("/A$", form))) %>%
    # Select relevant columns
    select(end, standardized_label, everything(),-has_form_A) %>%
    # Arrange by descending end date
    arrange(desc(end)) %>% 
    # Remove grouping
    ungroup() %>% 
    # Group by and arrange by descending filed date within each group
    group_by(standardized_label, end) %>% 
    # Arrange by filed date
    arrange(desc(filed)) %>% 
    # Retain only the first row i.e.. most recent by "filed" date
    slice_head(n = 1) %>% 
    # Remove grouping
    ungroup()
  
  # Split the 'end' column into 'year_end' and 'quarter_end'
  df_std_BS <- df_std_BS %>%
    mutate(
      year_end = as.integer(lubridate::year(end)),
      quarter_end = as.integer(lubridate::quarter(end)),
      year_start = case_when(
        Financial.Report == "BS" & is.na(as.integer(lubridate::year(start))) ~ as.integer(lubridate::year(end)), TRUE ~ as.integer(lubridate::year(start))),               
      quarter_start = case_when(
        Financial.Report == "BS" & is.na(as.integer(lubridate::quarter(start))) ~ as.integer(lubridate::quarter(end)), TRUE ~ as.integer(lubridate::quarter(start)))   
    ) 
  
  # Prepare dataframe for pivot
  df_std_BS_pivot <- df_std_BS %>%
    # Group by standardized_lable and fiscal period
    group_by(end,standardized_label,year_end,quarter_end, accn,cik,sic, sicDescription,tickers, Financial.Report) %>% 
    # Sum Quarterly_val within the group
    summarise(
      quarterly_val= sum(val)
    ) %>%
    ungroup() %>% 
    select(end, standardized_label, quarterly_val, year_end, quarter_end, everything())
  
  # 03 - Pivot df_std_BS in a dataframe format ------------------------------------------------------
  # This code transforms the data from a long format with multiple rows per observation to a wide format where each observation is represented by a single row with columns corresponding to different Concepts
  
  df_std_BS <- df_std_BS_pivot %>%
    select(end,standardized_label,quarterly_val) %>% 
    # Pivot the data using standardized_cashflow_label as column names
    pivot_wider(
      names_from = standardized_label,
      values_from = quarterly_val
    ) %>%
    # Arrange the dataframe in descending order based on the 'end' column
    arrange(desc(end))
  
  # 04 - Add new columns for standardization -----------------------------------
  # This code add the missing columns to the df_std_BS based on the standardized_balancesheet.xls and perform checks
 
   ## Step 1 - Check key financial Concepts -----------------------------------
  # It checks whether specific columns exist or are empty. If so it stops or remove corresponding rows
  if (!("Total Assets" %in% colnames(df_std_BS)) || !("Total Liabilities" %in% colnames(df_std_BS))) {
    stop("Total Assets or Total Liabilities is missing. The entity is not adequate for financial analysis.")
  }
  
  if (!("Total Liabilities & Stockholders Equity" %in% colnames(df_std_BS))) {
    stop("Total Liabilities & Stockholders Equity is missing. The entity is not adequate for financial analysis.")
  }
  
  if (!("Total Current Assets" %in% colnames(df_std_BS))) {
    stop("Total Current Assets is missing. The entity is not adequate for financial analysis.")
  }
  
  if (!("Total Current Liabilities" %in% colnames(df_std_BS))) {
    stop("Both Total Current Liabilities is missing. The entity is not adequate for financial analysis.")
  }
  
  # Remove rows where key financial Concepts are empty (or NA)
  df_std_BS <- df_std_BS %>%
    filter(  
      any(!is.na(`Total Liabilities & Stockholders Equity`) | `Total Liabilities & Stockholders Equity` != ""),
      any(!is.na(`Total Assets`) | `Total Assets` != ""),
      any(!is.na(`Total Liabilities`) | `Total Liabilities` != "")
    )
  
  ## Step 2 - Add missing columns -----------------------------------
  # It checks which columns from columns_to_add are not already present in df_std_BS
  columns_to_add <- setdiff(standardized_BS$standardized_label,colnames(df_std_BS)) 
  
  #It then adds only the missing columns to df_std_BS and initializes them with NA.
  if (length(columns_to_add) > 0) {
    
    # Add columns to the dataframe
    df_std_BS[,columns_to_add] <- NA
  }
  # Prepare company details to add to df_std_BS as additional columns
  df_Facts_columns_to_add <- df_Facts[1:nrow(df_std_BS), ]
  
  # Add company details columns to df_std_BS
  df_std_BS <- cbind(df_std_BS,df_Facts_columns_to_add[,c("cik","entityName","sic","sicDescription","tickers")])
  
  ## Step 3 - Calculate newly added columns columns -----------------------------------
  # Evaluate expressions for newly added columns 
  df_std_BS <- df_std_BS %>%
    mutate(
      `Total Current Assets` = pmax(0, case_when(
        is.na(`Total Current Assets`) ~ coalesce(`Total Assets`,0) - coalesce(`Total Non Current Assets`,0),
        TRUE ~ coalesce(`Total Current Assets`,0)
      )),
      `Total Non Current Assets` = pmax(0, case_when(
        is.na(`Total Non Current Assets`) ~ coalesce(`Total Assets`,0) - coalesce(`Total Current Assets`,0),
        TRUE ~ coalesce(`Total Non Current Assets`,0)
      )),
      `Other Current Assets` = pmax(0, case_when(
        is.na(`Other Current Assets`) ~ coalesce(`Total Current Assets`,0) - (coalesce(`Cash & Cash Equivalent`,0) + coalesce(`Marketable Securities Current`,0) + coalesce(`Total Accounts Receivable`,0) + coalesce(`Total Inventory`,0) + coalesce(`Prepaid Expenses`,0)),
        TRUE ~ coalesce(`Other Current Assets`,0)
      )),
      `Other Non Current Assets` = pmax(0, case_when(
        is.na(`Other Non Current Assets`) ~ coalesce(`Total Non Current Assets`,0) - (coalesce(`Marketable Securities Non Current`,0) + coalesce(`Property Plant and Equipment`,0) + coalesce(`Intangible Assets (excl. goodwill)`,0) + coalesce(`Goodwill`,0)),
        TRUE ~ coalesce(`Other Non Current Assets`,0)
      )),
      `Total Non Current Liabilities` = pmax(0, case_when(
        is.na(`Total Non Current Liabilities`) ~ coalesce(`Total Liabilities`,0) - coalesce(`Total Current Liabilities`,0),
        TRUE ~ coalesce(`Total Non Current Liabilities`,0)
      )),
      `Other Current Liabilities` = pmax(0, case_when(
        is.na(`Other Current Liabilities`) ~ coalesce(`Total Current Liabilities`,0) - (coalesce(`Accounts Payable`,0) + coalesce(`Tax Payable`,0) +  coalesce(`Current Debts`,0) + coalesce(`Operating Lease Liability Current`,0)),
        TRUE ~ coalesce(`Other Current Liabilities`,0)
      )),
      `Other Non Current Liabilities` = pmax(0, case_when(
        is.na(`Other Non Current Liabilities`) ~ coalesce(`Total Non Current Liabilities`,0) - (coalesce(`Non Current Debts`,0) + coalesce(`Operating Lease Liability Non Current`,0)),
        TRUE ~ coalesce(`Other Non Current Liabilities`,0)
      )),
      `Total Stockholders Equity` = case_when(
        is.na(`Total Stockholders Equity`) ~ coalesce(`Total Liabilities & Stockholders Equity`,0) - coalesce(`Total Liabilities`,0),
        TRUE ~ coalesce(`Total Stockholders Equity`,0)
      )
    )
  
  ## Step 4 - Order columns based on standardized_balancesheet_label -----------------------------------
  custom_order <- unique(standardized_BS[,1])
  
  # Reorder the columns as per standardized_balancesheet.xlsx
  df_std_BS <- df_std_BS[, c("end", custom_order)]
  # Add the columns with the metadata
  df_std_BS <- df_std_BS %>% 
    mutate(
      cik = df_Facts$cik[1],
      entityName = df_Facts$entityName[1],
      sic = df_Facts$sic[1],
      sicDescription = df_Facts$sicDescription[1],
      tickers = df_Facts$tickers[1]
    )
  
  return(df_std_BS)
}

# Function to rebuild the income statement ---------------------------------------
# Function to create a dataframe representative of the quarterly income statement of the entity. The basis for the dataframe is a standardized income statement (standardized_incomestatement.xlsx). In case of quarters that are missing the data (Facts) are estimated. The estimate of the data of the missing quarters is calculated based on the yearly data available. The difference between the yearly data and the data from the available quarter is then allocated equally to the missing quarters.

IS_std <- function(df_Facts) {
  # 01 - Join standardized_IS ------------------------------------------------------
  # Define the standardized incomestatement file path
  IS_path <- here(data_dir, "standardized_IS.xlsx")
  
  # Read the standardized_IS.xlsx file
  standardized_IS <- read.xlsx(IS_path, sheet = "Sheet1")
  
  # Rename standardized_IS column df_Fact_Description to perform left_join
  standardized_IS <- standardized_IS %>%
    rename(description = df_Facts_Description)
  
  # Merge df_Facts with standardized_IS based on description and period
  df_std_IS <- df_Facts %>%
    left_join(standardized_IS, by = "description") %>%
    filter(Financial.Report == "IS") %>% 
    select(standardized_label, everything(), -df_Facts_us_gaap_references)
  
  # 02 - Data cleaning ------------------------------------------------------
  # This code filters rows in df_std_IS based on whether there's a "/A" in the 'form' column. Rows with "/A" are retained if any row in their group contains it. Relevant columns are selected.
  
  # Change format of start and end dates from characters to date
  df_std_IS <- df_std_IS %>%
    mutate(end = as.Date(end),
           start = as.Date(start),
           filed = as.Date(filed),
           val = as.numeric(val))
  
  df_std_IS <- df_std_IS %>%
    # Filter out rows without standardized_label
    filter(!is.na(standardized_label)) %>% 
    # Group by end period (end) and label
    group_by(end, description) %>%
    # Arrange by descending end date within each group
    arrange(desc(end)) %>%
    # Add a column indicating if any row in the group has a form ending with /A
    mutate(
      has_form_A = grepl("/A$", form)
    ) %>%
    # Filter rows based on the condition:
    # - Retain rows without /A
    # - Retain rows with /A if there's at least one row with /A in the group
    filter(!has_form_A | (has_form_A & grepl("/A$", form))) %>%
    # Select relevant columns
    select(end, standardized_label, everything(),-has_form_A) %>%
    # Arrange by descending end date
    arrange(desc(end)) %>% 
    # Remove grouping
    ungroup() 
  
  # Split the 'end' and 'start' columns into 'year_end/start' and 'quarter_end/start'
  df_std_IS <- df_std_IS %>%
    mutate(
      year_end = as.integer(lubridate::year(end)),
      quarter_end = as.integer(lubridate::quarter(end)),
      year_start = case_when(
        Financial.Report == "BS" & is.na(as.integer(lubridate::year(start))) ~ as.integer(lubridate::year(end)), TRUE ~ as.integer(lubridate::year(start))),               
      quarter_start = case_when(
        Financial.Report == "BS" & is.na(as.integer(lubridate::quarter(start))) ~ as.integer(lubridate::quarter(end)), TRUE ~ as.integer(lubridate::quarter(start)))   
    ) 
  
  # 03 - Handling cumulative values and estimating missing quarters ------------------------------------------------------

  # Perform additional data processing to clean data set and estimate cumulative values from existing data
  
  # Estimate number of quarters underlying a Fact of financial item i.e. val
  df_std_IS <- df_std_IS %>%
    group_by(description, year_end) %>%
    arrange(desc(quarter_end), desc(quarter_start)) %>%
    mutate(
      cumulative_quarters = case_when(
        year_end == year_start & quarter_end != quarter_start ~ quarter_end - quarter_start + 1,
        TRUE ~ 1)
    )%>% 
    ungroup()
  
  # Remove duplicated from multiple filings retaining the rows with the most recent "filed" date with the largest number of quarters covered
  df_std_IS <- df_std_IS %>%
    group_by(description, year_end, quarter_end) %>% 
    # Arrange by descending "filed" date and cumulative quarters
    arrange(desc(filed), desc(cumulative_quarters)) %>%
    # Retain the first in the group ordered by "filed" date
    distinct(description, end, .keep_all = TRUE) %>% 
    # Keep only the first occurrence of each unique combination of description and end date
    ungroup()
 
  # Calculate the number of distinct quarters and the missing ones represented by each description in the dataset.
  df_std_IS_quarter_summary <- df_std_IS %>%
    group_by(description) %>%
    # Summarize the number of distinct quarters represented for each description
    summarise(total_quarters_end = n_distinct(quarter_end)) %>% 
    # Calculate number of quarters missing in the grouping
    mutate(
      "No. Quarters Missing" = ifelse(total_quarters_end == 4, 0, 4 - total_quarters_end)
    ) %>% 
    ungroup()
  
  # Calculate preliminary quarterly_val based on the number of quarters within group_by description and year_end 
  df_std_IS <- df_std_IS %>% 
    # Group by year and description
    group_by(description, year_end) %>%
    # Calculate quarterly_Val based on the number of cumulative quarters
    mutate(
      quarterly_val = val / cumulative_quarters,
      # Calculate the number of rows in the group_by
      count_rows = n()
    ) %>% 
    ungroup()
  
  # Adjust quarterly_val based on existing of cumulative values
  df_std_IS <- df_std_IS %>% 
    # Group by description and year_end
    group_by(description, year_end) %>%
    # Arrange the dataframe to properly position lead() values
    arrange(desc(cumulative_quarters), desc(quarter_end), quarter_start) %>% 
    mutate(
      # Column to count the records in the group
      count_rows = n(),
      # Recalculate quarterly_val for specific cases as differences from lead() values
      quarterly_val = case_when(  
        cumulative_quarters == 1 ~ quarterly_val,
        cumulative_quarters >= 2 & 
          count_rows > 1 &
          !is.na(lead(quarter_end)) &
          !is.na(lead(quarter_start)) &
          quarter_end == lead(quarter_end) + 1 &
          quarter_start == lead(quarter_start) ~ val - lead(val),
        cumulative_quarters >= 2 & 
          count_rows > 1 &
          !is.na(lead(quarter_end)) &
          !is.na(lead(quarter_start)) &
          quarter_end == lead(quarter_end) + 2 &
          quarter_start == lead(quarter_start) ~ val - lead(val) - quarterly_val,
        cumulative_quarters >= 2 & 
          count_rows > 1 &
          !is.na(lead(quarter_end)) &
          !is.na(lead(quarter_start)) &
          quarter_end == lead(quarter_end) + 3 &
          quarter_start == lead(quarter_start) ~ val - lead(val) - 2 * quarterly_val,
        cumulative_quarters >= 2 & 
          count_rows > 1 &
          !is.na(lead(quarter_end)) &
          !is.na(lead(quarter_start)) &
          quarter_end == lead(quarter_end)&
          quarter_start == lead(quarter_start) + 1 ~ val - lead(val),
        TRUE ~ quarterly_val),
      
      # Detect quarterly_val modified   
      modified_quarterly_val = case_when(
        cumulative_quarters == 1 ~ FALSE,
        cumulative_quarters >= 2 & 
          count_rows > 1 &
          !is.na(lead(quarter_end)) &
          !is.na(lead(quarter_start)) &
          quarter_end == lead(quarter_end) + 1 &
          quarter_start == lead(quarter_start) ~ TRUE,
        cumulative_quarters >= 2 & 
          count_rows > 1 &
          !is.na(lead(quarter_end)) &
          !is.na(lead(quarter_start)) &
          quarter_end == lead(quarter_end) + 2 &
          quarter_start == lead(quarter_start) ~ TRUE,
        cumulative_quarters >= 2 & 
          count_rows > 1 &
          !is.na(lead(quarter_end)) &
          !is.na(lead(quarter_start)) &
          quarter_end == lead(quarter_end) + 3 &
          quarter_start == lead(quarter_start) ~ TRUE,
        TRUE ~ FALSE)  
    ) %>% 
    ungroup() %>% 
    select(end, standardized_label, val, quarterly_val, year_end, quarter_end, quarter_start, cumulative_quarters, count_rows, modified_quarterly_val, everything())  
  
  # Filter out rows with duplicated val for the same standardized label
  df_std_IS <- df_std_IS %>%
    # Group by the standardized label for the same year and same quarter
    group_by(standardized_label, year_end, quarter_end) %>%
    # Arrange to by quarters_end and descending date "filed"
    arrange(desc(quarter_end),desc(filed)) %>%
    # Keep only the first occurrence of 'val' within each group
    filter(!duplicated(standardized_label,val)) %>%
    ungroup()
  
  # Prepare dataframe for pivot
  df_std_IS_pivot <- df_std_IS %>%
    # Group by standardized_lable and fiscal period
    group_by(end,standardized_label,year_end,quarter_end, accn,cik,sic, sicDescription,tickers, Financial.Report) %>% 
    # Sum Quarterly_val within the group
    summarise(
      quarterly_val= sum(quarterly_val)
    ) %>%
    ungroup() %>% 
    select(end, standardized_label, quarterly_val, year_end, quarter_end, everything())
  
  # 03 - Pivot df_std_IS in a dataframe format
  # This code transforms the data from a long format with multiple rows per observation to a wide format where each observation is represented by a single row with columns corresponding to different Concepts
  
  df_std_IS <- df_std_IS_pivot %>%
    select(end,standardized_label,quarterly_val) %>% 
    # Pivot the data using standardized_cashflow_label as column names
    pivot_wider(
      names_from = standardized_label,
      values_from = quarterly_val
    ) %>%
    # Arrange the dataframe in descending order based on the 'end' column
    arrange(desc(end))
  
  # 04 - Add new columns for standardization -----------------------------------
  # This code add the missing columns to the df_std_IS based on the standardized_incomestatement.xls and perform checks
  
  ## Step 1 - Check key financial Concepts -----------------------------------
  # It checks whether specific columns exist or are empty. If so it stops or remove corresponding rows
  if (!("Gross Profit" %in% colnames(df_std_IS)) || !("Operating Income" %in% colnames(df_std_IS)) || !("Net Income (loss)" %in% colnames(df_std_IS)) ) {
    stop("Gross Profit or Operating Income or Net Income (loss) is missing. The entity is not adequate for financial analysis.")
  }
  
  # Remove rows where key financial Concepts are empty (or NA)
  df_std_IS <- df_std_IS %>%
    filter(
      any(!is.na(`Gross Profit`) | `Gross Profit` != ""),
      any(!is.na(`Operating Income`) | `Operating Income` != ""),
      any(!is.na(`Net Income (loss)`) | `Net Income (loss)` != "")
    )
  
  ## Step 2 - Add missing columns -----------------------------------
  # It checks which columns from columns_to_add are not already present in df_std_IS
  columns_to_add <- setdiff(standardized_IS$standardized_label,colnames(df_std_IS)) 
  
  #It then adds only the missing columns to df_std_IS and initializes them with NA.
  if (length(columns_to_add) > 0) {
    
    # Add columns to the dataframe
    df_std_IS[,columns_to_add] <- NA
  }
  
  ## Step 3 - Calculate newly added columns columns -----------------------------------
  # Evaluate expressions for key financial Concepts 
  
  df_std_IS <- df_std_IS %>%
    mutate(
      `Revenue` = case_when(
        is.na(`Revenue`) ~ coalesce(`Cost of Revenue`,0) + coalesce(`Gross Profit`,0),
        TRUE ~ coalesce(`Revenue`,0)
      ),
      `Cost of Revenue` = case_when(
        is.na(`Cost of Revenue`) ~ coalesce(`Revenue`,0) - coalesce(`Gross Profit`,0),
        TRUE ~ coalesce(`Cost of Revenue`,0)
      ),
      `Other Non Operating Income (Loss) Net` = case_when(
        is.na(`Other Non Operating Income (Loss) Net`) ~ coalesce(`Gross Profit`,0) - (coalesce(`Research and development`,0) + coalesce(`Sales general and administrative costs`,0) + coalesce(`Operating Income`,0)),
      ),
      `Other income (expense) Net` = case_when(
        is.na(`Other income (expense) Net`) ~ coalesce(`Operating Income`,0) - (coalesce(`Interest Income`,0) + coalesce(`Interest Expense`,0) + coalesce(`Income Before Income Tax`,0)),
      ),
    ) %>% 
    mutate_all(~round(., digits = 4))  # Adjust the number of digits as needed
  
  ## Step 4 - Order columns based on standardized_incomestatement_label -----------------------------------
  custom_order <- unique(standardized_IS[,1])
  # Reorder the columns as per standardized_incomestatement.xlsx
  df_std_IS <- df_std_IS[, c("end", custom_order)]
  # Add the columns with the metadata
  df_std_IS <- df_std_IS %>% 
    mutate(
      cik = df_Facts$cik[1],
      entityName = df_Facts$entityName[1],
      sic = df_Facts$sic[1],
      sicDescription = df_Facts$sicDescription[1],
      tickers = df_Facts$tickers[1]
    )
  
  
  return(df_std_IS)
}

# Function to rebuild the Income and Cash Flow statements ---------------------------------------
# Function to create a dataframe representative of the quarterly Cash Flow statement of the entity. The basis for the dataframe is a Cash Flow statement (standardized_CF.xlsx). In case of quarters that are missing the data (Facts) are estimated. The estimate of the data of the missing quarters is calculated based on the yearly data available. The difference between the yearly data and the data from the available quarters is then allocated equally to the missing quarters.

CF_std <- function(df_Facts) {
  # 01 - Join standardized Cashflow (CF) ------------------------------------------------------
  # Define the standardized CF file path
  CF_path <- here(data_dir, "standardized_CF.xlsx")
  
  # Read the standardized_CF.xlsx file
  standardized_CF <- read.xlsx(CF_path, sheet = "Sheet1")
  
  # Rename standardized_label column df_Fact_Description to perform left_join
  standardized_CF <- standardized_CF %>%
    rename(description = df_Facts_Description)
  
  # Merge df_Facts with standardized_CF based on description and period
  df_std_CF <- df_Facts %>%
    left_join(standardized_CF, by = "description") %>%
    filter(Financial.Report == "CF") %>% 
    select(standardized_label, everything(), -df_Facts_us_gaap_references)
  
  # 02 - Data cleaning ------------------------------------------------------
  # This code filters rows based on whether there's a "/A" in the 'form' column. Rows with "/A" are retained if any row in their group contains it. Relevant columns are selected.
  
  # Change format of start and end dates from characters to date
  df_std_CF <- df_std_CF %>%
    mutate(end = as.Date(end),
           start = as.Date(start),
           filed = as.Date(filed),
           val = as.numeric(val))
  
  df_std_CF <- df_std_CF %>%
    # Filter out rows without standardized_label
    filter(!is.na(standardized_label)) %>% 
    # Group by end period (end) and label
    group_by(end, description) %>%
    # Arrange by descending end date within each group
    arrange(desc(end)) %>%
    # Add a column indicating if any row in the group has a form ending with /A
    mutate(
      has_form_A = grepl("/A$", form)
    ) %>%
    # Filter rows based on the condition:
    # - Retain rows without /A
    # - Retain rows with /A if there's at least one row with /A in the group
    filter(!has_form_A | (has_form_A & grepl("/A$", form))) %>%
    # Select relevant columns
    select(end, standardized_label, everything(),-has_form_A) %>%
    # Arrange by descending end date
    arrange(desc(end)) %>% 
    # Remove grouping
    ungroup() 
  
  # Split the 'end' and 'start' columns into 'year_end/start' and 'quarter_end/start'
  df_std_CF <- df_std_CF %>%
    mutate(
      year_end = as.integer(lubridate::year(end)),
      quarter_end = as.integer(lubridate::quarter(end)),
      year_start = case_when(
        Financial.Report == "BS" & is.na(as.integer(lubridate::year(start))) ~ as.integer(lubridate::year(end)), TRUE ~ as.integer(lubridate::year(start))),               
      quarter_start = case_when(
        Financial.Report == "BS" & is.na(as.integer(lubridate::quarter(start))) ~ as.integer(lubridate::quarter(end)), TRUE ~ as.integer(lubridate::quarter(start)))   
    ) 
  
  # 03 - Handling cumulative values and estimating missing quarters ------------------------------------------------------
  
  # Perform additional data processing to clean data set and estimate cumulative values from existing data
  
  # Estimate number of quarters underlying a Fact of financial item i.e. val
  df_std_CF <- df_std_CF %>%
    group_by(description, year_end) %>%
    arrange(desc(quarter_end), desc(quarter_start)) %>%
    mutate(
      cumulative_quarters = case_when(
        year_end == year_start & quarter_end != quarter_start ~ quarter_end - quarter_start + 1,
        TRUE ~ 1)
    )%>% 
    ungroup()
  
  # Remove duplicated from multiple filings retaining the rows with the most recent "filed" date with the largest number of quarters covered
  df_std_CF <- df_std_CF %>%
    group_by( description, year_end, quarter_end) %>% 
    # Arrange by descending "filed" date and cumulative quarters
    arrange(desc(filed), desc(cumulative_quarters)) %>%
    # Retain the first in the group ordered by "filed" date
    distinct(description, end, .keep_all = TRUE) %>% 
    # Keep only the first occurrence of each unique combination of description and end date
    ungroup()
  
  # Calculate the number of distinct quarters and the missing ones represented by each description in the dataset.
  df_std_CF_quarter_summary <- df_std_CF %>%
    group_by(description) %>%
    # Summarize the number of distinct quarters represented for each description
    summarise(total_quarters_end = n_distinct(quarter_end)) %>% 
    # Calculate number of quarters missing in the grouping
    mutate(
      "No. Quarters Missing" = ifelse(total_quarters_end == 4, 0, 4 - total_quarters_end)
    ) %>% 
    ungroup()
  
  # Calculate preliminary quarterly_val based on the number of quarters within group_by description and year_end 
  df_std_CF <- df_std_CF %>% 
    # Group by year and description
    group_by(description, year_end) %>%
    # Calculate quarterly_Val based on the number of cumulative quarters
    mutate(
      quarterly_val = val / cumulative_quarters,
      # Calculate the number of rows in the group_by
      count_rows = n()
    ) %>% 
    ungroup()

  # Adjust quarterly_val based on existing of cumulative values
  df_std_CF <- df_std_CF %>% 
    # Group by description and year_end
    group_by(description, year_end) %>%
    # Arrange the dataframe to properly position lead() values
    arrange(desc(cumulative_quarters), desc(quarter_end), quarter_start) %>% 
    mutate(
      # Column to count the records in the group
      count_rows = n(),
      # Recalculate quarterly_val for specific cases as differences from lead() values
      quarterly_val = case_when(  
        cumulative_quarters == 1 ~ quarterly_val,
        cumulative_quarters >= 2 & 
          count_rows > 1 &
          !is.na(lead(quarter_end)) &
          !is.na(lead(quarter_start)) &
          quarter_end == lead(quarter_end) + 1 &
          quarter_start == lead(quarter_start) ~ val - lead(val),
        cumulative_quarters >= 2 & 
          count_rows > 1 &
          !is.na(lead(quarter_end)) &
          !is.na(lead(quarter_start)) &
          quarter_end == lead(quarter_end) + 2 &
          quarter_start == lead(quarter_start) ~ val - lead(val) - quarterly_val,
        cumulative_quarters >= 2 & 
          count_rows > 1 &
          !is.na(lead(quarter_end)) &
          !is.na(lead(quarter_start)) &
          quarter_end == lead(quarter_end) + 3 &
          quarter_start == lead(quarter_start) ~ val - lead(val) - 2 * quarterly_val,
        cumulative_quarters >= 2 & 
          count_rows > 1 &
          !is.na(lead(quarter_end)) &
          !is.na(lead(quarter_start)) &
          quarter_end == lead(quarter_end)&
          quarter_start == lead(quarter_start) + 1 ~ val - lead(val),
        TRUE ~ quarterly_val),
      
      # Detect quarterly_val modified   
      modified_quarterly_val = case_when(
        cumulative_quarters == 1 ~ FALSE,
        cumulative_quarters >= 2 & 
          count_rows > 1 &
          !is.na(lead(quarter_end)) &
          !is.na(lead(quarter_start)) &
          quarter_end == lead(quarter_end) + 1 &
          quarter_start == lead(quarter_start) ~ TRUE,
        cumulative_quarters >= 2 & 
          count_rows > 1 &
          !is.na(lead(quarter_end)) &
          !is.na(lead(quarter_start)) &
          quarter_end == lead(quarter_end) + 2 &
          quarter_start == lead(quarter_start) ~ TRUE,
        cumulative_quarters >= 2 & 
          count_rows > 1 &
          !is.na(lead(quarter_end)) &
          !is.na(lead(quarter_start)) &
          quarter_end == lead(quarter_end) + 3 &
          quarter_start == lead(quarter_start) ~ TRUE,
        TRUE ~ FALSE)  
    ) %>% 
    ungroup() %>% 
    select(end, standardized_label, val, quarterly_val, year_end, quarter_end, quarter_start, cumulative_quarters, count_rows, modified_quarterly_val, everything())  
  
  # Filter out rows with duplicated val for the same standardized label
  df_std_CF <- df_std_CF %>%
    # Group by the standardized label for the same year and same quarter
    group_by(standardized_label, year_end, quarter_end) %>%
    # Arrange to by quarters_end and descending date "filed"
    arrange(desc(quarter_end),desc(filed)) %>%
    # Keep only the first occurrence of 'val' within each group
    filter(!duplicated(standardized_label,val)) %>%
    ungroup()
  
  
  # Prepare dataframe for pivot
  df_std_CF_pivot <- df_std_CF %>%
    # Group by standardized_lable and fiscal period
    group_by(end,standardized_label,year_end,quarter_end, accn,cik,sic, sicDescription,tickers, Financial.Report) %>% 
    # Sum Quarterly_val within the group
    summarise(
      quarterly_val= sum(quarterly_val)
    ) %>%
    ungroup() %>% 
    select(end, standardized_label, quarterly_val, year_end, quarter_end, everything())
  
  # 04 - Cash Flow - Pivot df_std_CF in horizontal format
  # This code transforms the data from a long format with multiple rows per observation to a wide format where each observation is represented by a single row with columns corresponding to different Concepts
  
  df_std_CF <- df_std_CF_pivot %>%
    select(end,standardized_label,quarterly_val) %>% 
    # Pivot the data using standardized_cashflow_label as column names
    pivot_wider(
      names_from = standardized_label,
      values_from = quarterly_val
    ) %>%
    # Arrange the dataframe in descending order based on the 'end' column
    arrange(desc(end))
  
  # 04 - Add new columns for standardization -----------------------------------
  # This code add the missing columns to the df_std_CF based on the standardized_cashflow.xls and perform checks
  
  ## Step 1 - Check key financial Concepts -----------------------------------
  # It checks whether specific columns exist or are empty. If so it stops or remove corresponding rows
  if (!("(Operating Activities) Cash Flow from Operating Activities" %in% colnames(df_std_CF)) || !("(Investing Activities) Cash Flow from Investing Activities" %in% colnames(df_std_CF)) || !("(Financing Activities) Cash Flow from Financing Activities" %in% colnames(df_std_CF))) {
    stop("Cash Flow from Operating activities or Investing activities or Financing activities is missing. The entity is not adequate for financial analysis.")
  }
  
  # Remove rows where key financial Concepts are empty (or NA)
  df_std_CF <- df_std_CF %>%
    filter(
      any(!is.na(`(Operating Activities) Cash Flow from Operating Activities`) | `(Operating Activities) Cash Flow from Operating Activities` != ""),
      any(!is.na(`(Investing Activities) Cash Flow from Investing Activities`) | `(Investing Activities) Cash Flow from Investing Activities` != ""),
      any(!is.na(`(Financing Activities) Cash Flow from Financing Activities`) | `(Financing Activities) Cash Flow from Financing Activities` != "")
    )
  
  ## Step 2 - Add missing columns -----------------------------------
  # It checks which columns from columns_to_add are not already present in df_std_CF
  columns_to_add <- setdiff(standardized_CF$standardized_label,colnames(df_std_CF)) 
  
  #Adds only the missing columns to df_std_CF and initializes them with NA.
  if (length(columns_to_add) > 0) {
    
    # Add columns to the dataframe
    df_std_CF[,columns_to_add] <- NA
  }
  
  ## Step 3 - Calculate newly added columns columns -----------------------------------
  # Evaluate expressions for key financial Concepts 
  
  df_std_CF <- df_std_CF %>%
    mutate(
      `(Operating Activities) Change in Other Operating Activities` =  
        coalesce(`(Operating Activities) Cash Flow from Operating Activities`,0) - 
        (coalesce(`(Operating Activities) Cash Flow Depreciation, Depletion, Ammortization`,0) +
           coalesce(`(Operating Activities) Change in Accounts Receivable`,0) +
           coalesce(`(Operating Activities) Change in Inventory`,0) +
           coalesce(`(Operating Activities) Change in Prepaid expenses and other assets`,0) +
           coalesce(`(Operating Activities) Change in Accounts Payable`,0) + 
           coalesce(`(Operating Activities) Change in Accounts Taxes Payable`,0) + 
           coalesce(`(Operating Activities) Change in Reserve for Sales Return and allowances`,0) +
           coalesce(`(Operating Activities) Deferred Income Tax`,0) +
           coalesce(`(Operating Activities) Stock-based Compensation`,0)
        ),
      ,
      `(Investing Activities) Change in Other Investing Activities` =  
        coalesce(`(Investing Activities) Cash Flow from Investing Activities`,0) - 
        (coalesce(`(Investing Activities) Purchase of Property, Plant and Equipment`,0) + 
           coalesce(`(Investing Activities) Proceeds from Asset Sales`,0) +
           coalesce(`(Investing Activities) Purchase of Businesses`,0) +
           coalesce(`(Investing Activities) Purchase of Marketable Securities and Investment`,0) +
           coalesce(`(Investing Activities) Proceeds from sale or maturity of Marketable Securities and Investment`,0) +
           coalesce(`(Investing Activities) Proceeds from maturities of Marketable Securities and Investment`,0)
        ),
      ,
      `(Financing Activities) Impact of Stock Options and Other` = 
        coalesce(`(Financing Activities) Cash Flow from Financing Activities`,0) - 
        (coalesce(`(Financing Activities) Proceeds from Issuance of Stock`,0) + 
           coalesce(`(Financing Activities) Payment for Repurchase of Stock`,0) + 
           coalesce(`(Financing Activities) Proceeds from Issuance of Debt`,0) + 
           coalesce(`(Financing Activities) Payment of Debt`,0) + 
           coalesce(`(Financing Activities) Cash for Dividends`,0) 
        ),
      ,
    ) %>% 
    mutate_all(~round(., digits = 4))  # Adjust the number of digits as needed
  
  ## Step 4 - Order columns based on standardized_cashflow_label -----------------------------------
  custom_order <- unique(standardized_CF[,"standardized_label"])
  # Reorder the columns as per standardized_CF.xlsx
  df_std_CF <- df_std_CF[, c("end", custom_order)]
  # Add the columns with the metadata
  df_std_CF <- df_std_CF %>% 
    mutate(
      cik = df_Facts$cik[1],
      entityName = df_Facts$entityName[1],
      sic = df_Facts$sic[1],
      sicDescription = df_Facts$sicDescription[1],
      tickers = df_Facts$tickers[1]
    )
  
  
  return(df_std_CF)
}
