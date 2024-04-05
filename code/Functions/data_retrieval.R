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
  
  # Change format of start and end dates from characters to date
  df_std_BS <- df_std_BS %>%
    mutate(end = as.Date(end),
           start = as.Date(start))
  
  df_std_BS <- df_std_BS %>%
    # Filter out rows without standardized_balancesheet_label
    filter(!is.na(standardized_balancesheet_label) & !is.na(frame)) %>% 
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
    # Group by and arrange by descending filed date within each group
    group_by(label, end) %>%
    arrange(desc(filed)) %>%
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
  
  # 03 - Pivot df_std_BS in a dataframe format ------------------------------------------------------
  # This code transforms the data from a long format with multiple rows per observation to a wide format where each observation is represented by a single row with columns corresponding to different Concepts
  
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
  
  ## Step 1 - Check key financial Concepts -----------------------------------
  # It checks whether specific columns exist or are empty. If so it stops or remove corresponding rows
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
  
  # Remove rows where key financial Concepts are empty (or NA)
  df_std_BS <- df_std_BS %>%
    filter(  
      any(!is.na(`Total Liabilities & Stockholders Equity`) | `Total Liabilities & Stockholders Equity` != ""),
      any(!is.na(`Total Assets`) | `Total Assets` != ""),
      any(!is.na(`Total Liabilities`) | `Total Liabilities` != "")
    )
  
  ## Step 2 - Add missing columns -----------------------------------
  # It checks which columns from columns_to_add are not already present in df_std_BS
  columns_to_add <- setdiff(standardized_balancesheet$standardized_balancesheet_label,colnames(df_std_BS)) 
  
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
      `Total Current Liabilities` = pmax(0, case_when(
        is.na(`Total Current Liabilities`) ~ coalesce(`Total Liabilities`,0) - coalesce(`Total Non Current Liabilities`,0),
        TRUE ~ coalesce(`Total Current Liabilities`,0)
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
  custom_order <- unique(standardized_balancesheet[,1])
  
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
  # 01 - Join standardized_incomestatement ------------------------------------------------------
  # Define the standardized incomestatement file path
  incomestatement_path <- here(data_dir, "standardized_incomestatement.xlsx")
  
  # Read the standardized_incomestatement.xlsx file
  standardized_incomestatement <- read.xlsx(incomestatement_path, sheet = "Sheet1")
  
  # Rename standardized_incomestatement column df_Fact_Description to perform left_join
  standardized_incomestatement <- standardized_incomestatement %>%
    rename(description = df_Facts_Description)
  
  # Merge df_Facts with standardized_incomestatement based on description and period
  df_std_IS <- df_Facts %>%
    left_join(standardized_incomestatement, by = "description") %>%
    select(standardized_incomestatement_label, everything(), -df_Facts_us_gaap_references)
  
  # 02 - Data cleaning ------------------------------------------------------
  # This code filters rows in df_std_IS based on whether there's a "/A" in the 'form' column. Rows with "/A" are retained if any row in their group contains it. Relevant columns are selected, the data is arranged by descending 'end' date,  and for each unique 'val', the row with the most recent 'end' date is kept.
  
  # Change format of start and end dates from characters to date
  df_std_IS <- df_std_IS %>%
    mutate(end = as.Date(end),
           start = as.Date(start))
  
  df_std_IS <- df_std_IS %>%
    # Filter out rows without standardized_incomestatement_label and no frame e.g CY2023Q3 
    filter(!is.na(standardized_incomestatement_label) & !is.na(frame)) %>% 
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
    select(end, standardized_incomestatement_label, everything(),-has_form_A) %>%
    # Arrange by descending end date
    arrange(desc(end)) %>% 
    # Remove grouping
    ungroup() %>%
    # Group by and arrange by descending filed date within each group
    group_by(standardized_incomestatement_label, end) %>%
    arrange(desc(filed)) %>%
    # Retain only the first row within each group
    slice_head(n = 1) %>%
    # Remove grouping
    ungroup()
  
  # 03 - Handling missing quarters ------------------------------------------------------
  # This code generates estimates of the Income Statement Facts associated with quarters missing in the data downloaded.
  
  # Split the 'frame' column into 'frame_year' and 'frame_quarter'
  df_std_IS <- df_std_IS %>%
    mutate(
      frame_year = lubridate::year(end),
      frame_quarter = lubridate::quarter(end)
    )
  
  # Calculate the sum of all quarters values and the fiscal year values
  df_sum_quarters_years <- df_std_IS %>%
    group_by(standardized_incomestatement_label, frame_year) %>%
    summarise(
      # Calculate the total number of distinct quarters in the group, excluding NA values
      total_quarters = n_distinct(frame_quarter) - sum(is.na(frame_quarter)),
      # Calculate the sum of values for quarters without NA values
      total_val_quarters = sum(val[!is.na(frame_quarter)]),
      # Calculate the sum of values for quarters with NA values, representing the fiscal year total
      total_val_year = sum(val[is.na(frame_quarter)])
    ) %>% 
    ungroup()
  
  # Create the row_not_to_add column based on specified criteria. This column marks those years where there is no need to estimate the val of quarters since these are related to the quarters of the ongoing year or not yet filed.
  df_sum_quarters_years <- df_sum_quarters_years %>% 
    mutate(
      row_not_to_add = 
        total_val_year == 0 & 
        total_quarters == 3 & 
        as.integer(frame_year) >= (as.integer(format(Sys.Date(), "%Y")) - 1)
    )  
  
  # Calculate the difference between the sum of the val of the fiscal years and the sum of the val of the quarters. The two sums are different for those past years where only some quarters are included in the "df_std_IS" dataframe. The records marked with "quarter_not_to_add" as FALSE are related to those quarters to be added to the  dataframe df_std_IS. Those marked TRUE are removed since these are related to the quarters of the ongoing year or not yet filed.
  df_sum_quarters_years <- df_sum_quarters_years %>% 
    # Calculate the number of quarters to add
    mutate(total_quarters_to_add = 4 - total_quarters) %>%
    # Remove the rows that are not representative of missing quarters.
    filter(row_not_to_add == FALSE & total_quarters_to_add > 0) %>% 
    # Calculate the total and quarterly value of missing quarters
    mutate(
      total_val_of_missing_quarters = total_val_year - total_val_quarters,
      val_missing_quarter = total_val_of_missing_quarters / total_quarters_to_add
    )    
  
  # Extract the missing quarters
  missing_quarters <- df_sum_quarters_years %>%
    select(standardized_incomestatement_label,frame_year, val_missing_quarter) %>%
    # Duplicate each row four times to represent the four quarters in a year
    slice(rep(1:n(), each = 4)) %>%
    # Create a new column 'frame_quarter' based on the duplicated rows
    mutate(frame_quarter = as.integer(rep(c("1", "2", "3", "4"), n() / 4)))  
  
  # Create the missing rows for the missing quarters
  rows_to_add <- missing_quarters %>%
    anti_join(df_std_IS, by = c("standardized_incomestatement_label","frame_year","frame_quarter")) %>%
    mutate(
      # Set end and start dates based on frame_quarter
      end = as.Date(case_when(
        frame_quarter == "1" ~ paste(frame_year, "-03-31", sep = ""),
        frame_quarter == "2" ~ paste(frame_year, "-06-30", sep = ""),
        frame_quarter == "3" ~ paste(frame_year, "-09-30", sep = ""),
        frame_quarter == "4" ~ paste(frame_year, "-12-31", sep = "")
      )),
      start = as.Date(case_when(
        frame_quarter == "1" ~ paste(frame_year, "-01-01", sep = ""),
        frame_quarter == "2" ~ paste(frame_year, "-04-01", sep = ""),
        frame_quarter == "3" ~ paste(frame_year, "-07-01", sep = ""),
        frame_quarter == "4" ~ paste(frame_year, "-10-01", sep = "")
      )),
      estimated_val = "TRUE"
    ) %>% 
    rename(val = val_missing_quarter) %>% 
    # Ungroup missing_rows
    ungroup()
  
  # Bind the rows to df_std_IS and fill in NA in added columns
  df_std_IS <- df_std_IS %>% 
    bind_rows(rows_to_add) %>% 
    group_by(standardized_incomestatement_label) %>%
    mutate(
      # Fill in missing values in the added rows for description, label, and us_gaap_reference
      description = first(description),
      label = first(label),
      us_gaap_reference = first(us_gaap_reference),
      # Convert NA to FALSE in the estimated_val column
      estimated_val = ifelse(is.na(estimated_val), FALSE, estimated_val)
    ) %>%
    ungroup()  %>% 
    mutate(
      # Fill in missing values based on conditions for fy, fp, form, filed, and frame
      fy = ifelse(estimated_val == "TRUE", frame_year, df_std_IS$fy),
      fp = ifelse(estimated_val == "TRUE", paste0("Q", frame_quarter), df_std_IS$fp),
      form = ifelse(estimated_val == "TRUE", NA, df_std_IS$form),
      filed = ifelse(estimated_val == "TRUE", NA, df_std_IS$filed),
      frame = ifelse(estimated_val == "TRUE", paste0("CY", frame_year, "Q", frame_quarter), df_std_IS$frame)
    ) %>% 
    fill(
      # Fill missing values downward for selected columns
      accn, cik, entityName, sic, sicDescription, tickers,
      .direction = "down"
    ) %>% 
    arrange(desc(end), standardized_incomestatement_label) 
  
  # Filter all rows that include the result of the fiscal year, retaining the ones that include only the results of the fiscal quarters.
  df_std_IS <- df_std_IS %>% 
    filter(!is.na(frame_quarter))
  
  # 03 - Pivot df_std_IS in a dataframe format ------------------------------------------------------
  # This code transforms the data from a long format with multiple rows per observation to a wide format where each observation is represented by a single row with columns corresponding to different Concepts
  
  df_std_IS <- df_std_IS %>%
    select(end,standardized_incomestatement_label,val) %>% 
    # Pivot the data using standardized_incomestatement_label as column names
    pivot_wider(
      names_from = standardized_incomestatement_label,
      values_from = val
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
      any(!is.na(`Operating Income`) | `Operating Income` != ""),
      any(!is.na(`Gross Profit`) | `Gross Profit` != ""),
      any(!is.na(`Net Income (loss)`) | `Net Income (loss)` != "")
    )
  
  ## Step 2 - Add missing columns -----------------------------------
  # It checks which columns from columns_to_add are not already present in df_std_IS
  columns_to_add <- setdiff(standardized_incomestatement$standardized_incomestatement_label,colnames(df_std_IS)) 
  
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
  custom_order <- unique(standardized_incomestatement[,1])
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

# Function to rebuild the Cash Flow statement ---------------------------------------
# Function to create a dataframe representative of the quarterly Cash Flow statement of the entity. The basis for the dataframe is a standardized Cash Flow statement (standardized_cashflow.xlsx). In case of quarters that are missing the data (Facts) are estimated. The estimate of the data of the missing quarters is calculated based on the yearly data available. The difference between the yearly data and the data from the available quarter is then allocated equally to the missing quarters.

# "OTHER" TO BE CALCULATED
# (Operating Activities) Change in Other Working Capital = difference from (Operating Activities) Cash Flow from Operating Activities
# 
# (Investing Activities) Gain (Losses) in Other Investing Activities = difference from (Investing Activities) Cash Flow from Investing Activities
# 
# (Financing Activities) Impact of Stock Options and Other =difference from (Financing Activities) Cash Flow from Financing Activities
# 
# Other impact from Operating, Investing, Financing Activities = difference from Change in Cash, Cash Equivalents

CF_std <- function(df_Facts) {
  # 01 - Join standardized_cashflow ------------------------------------------------------
  # Define the standardized cashflow file path
  cashflow_path <- here(data_dir, "standardized_cashflow.xlsx")
  
  # Read the standardized_cashflow.xlsx file
  standardized_cashflow <- read.xlsx(cashflow_path, sheet = "Sheet1")
  
  # Rename standardized_cashflow column df_Fact_Description to perform left_join
  standardized_cashflow <- standardized_cashflow %>%
    rename(description = df_Facts_Description)
  
  # Merge df_Facts with standardized_incomestatement based on description and period
  df_std_CF <- df_Facts %>%
    left_join(standardized_cashflow, by = "description") %>%
    select(standardized_cashflow_label, everything(), -df_Facts_us_gaap_references)
  
  # 02 - Data cleaning ------------------------------------------------------
  # This code filters rows in df_std_CF based on whether there's a "/A" in the 'form' column. Rows with "/A" are retained if any row in their group contains it. Relevant columns are selected.
  
  # Change format of start and end dates from characters to date
  df_std_CF <- df_std_CF %>%
    mutate(end = as.Date(end),
           start = as.Date(start),
           filed = as.Date(filed))
  
  df_std_CF <- df_std_CF %>%
    # Filter out rows without standardized_cashflow_label and no frame e.g CY2023Q3 
    filter(!is.na(standardized_cashflow_label)) %>% 
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
    select(end, standardized_cashflow_label, everything(),-has_form_A) %>%
    # Arrange by descending end date
    arrange(desc(end)) %>% 
    # Remove grouping
    ungroup() 
  
  # Split the 'end' column into 'year_end/start' and 'quarter_end/start'
  df_std_CF <- df_std_CF %>%
    mutate(
      year_end = lubridate::year(end),
      quarter_end = lubridate::quarter(end),
      year_start = lubridate::year(start),
      quarter_start = lubridate::quarter(start)
    )
  
  # Add  fame_start_year and frame_start_quarter based on records e.g. "Cash & Cash Equivalent beginning of the period" from Balance Sheet.
  df_std_CF <- df_std_CF %>% 
    mutate(
      year_start = ifelse(is.na(year_start),year_end,year_start),
      quarter_start = ifelse(is.na(quarter_start) & 
                                     standardized_cashflow_label == "Cash & Cash Equivalent at the beginning of the period",
                                   quarter_end, quarter_start),
      quarter_start = ifelse(is.na(quarter_start) & 
                                     standardized_cashflow_label != "Cash & Cash Equivalent at the beginning of the period" & 
                                     form == "10-K", 1,quarter_start)
    )
  
  # 03 - Handling cumulative values and estimating missing quarters ------------------------------------------------------
     
  # Perform additional data processing to identify cumulative values and estimate values for missing quarters.
  # It then calculates the quarterly value by subtracting the lead value from the current value over the corresponding quarter.
  
 # Identify cumulative values in val
  df_std_CF_test <- df_std_CF %>%
    group_by(description, year_end) %>%
    arrange(desc(quarter_end), desc(quarter_start)) %>%
    mutate(
      Is_Cumulative = ifelse(
        year_end == year_start & quarter_end == quarter_start,
        "NO", "YES")
    )
  
  # Clean up duplicated val from multiple filings retaining the rows with the most recent "filed" date.
  df_std_CF_test <- df_std_CF_test %>%
    group_by(description, end) %>% 
    arrange(desc(filed)) %>%  # Arrange by descending "filed" date
    distinct(description, end, .keep_all = TRUE)  # Keep only the first occurrence of each unique combination of description and end, preserving the one with the most recent "filed" date
  
  
  # Summarize the number of distinct quarters represented for each description
  # This step calculates the number of distinct quarters and the missing ones represented by each description in the dataset.
  
  df_std_CF_quarter_summary <- df_std_CF %>%
    group_by(description) %>%
    summarise(total_quarters_end = n_distinct(quarter_end)) %>% 
    mutate(
      "No. Quarters Missing" = ifelse(total_quarters_end == 4, 0, 4 - total_quarters_end)
    )
  
  df_std_CF_test <- df_std_CF_test %>% 
    left_join(df_std_CF_quarter_summary, by = "description")
  
  df_std_CF_test <- df_std_CF_test %>% 
    group_by(description, year_end) %>% 
    mutate(
      Quarterly_val = case_when(
        Is_Cumulative == "NO" ~ as.numeric(val),
        Is_Cumulative == "YES" & quarter_end != quarter_start ~ as.numeric((val - dplyr::lead(val)) / (quarter_end - dplyr::lead(quarter_end))),
        Is_Cumulative == "YES" & quarter_end == quarter_start ~ NA_real_ ,TRUE ~ NA_real_)
    ) %>% 
    select(end, standardized_cashflow_label,val,Quarterly_val, description, everything())
  
   # 

  
  # >>>>>>----<<<<<<< TO CHECK WITH Missing quarters

  
  
  
  # >>>>>>----<<<<<<<
  
  
  # Calculate the sum of all quarters values and the fiscal year values
  df_sum_quarters_years <- df_std_CF %>%
    group_by(standardized_cashflow_label, frame_year) %>%
    summarise(
      # Calculate the total number of distinct quarters in the group, excluding NA values
      total_quarters = n_distinct(frame_quarter) - sum(is.na(frame_quarter)),
      # Calculate the sum of values for quarters without NA values
      total_val_quarters = sum(val[!is.na(frame_quarter)]),
      # Calculate the sum of values for quarters with NA values, representing the fiscal year total
      total_val_year = sum(val[is.na(frame_quarter)])
    ) %>% 
    ungroup()
  
  # Create the row_not_to_add column based on specified criteria. This column marks those years where there is no need to estimate the val of quarters since these are related to the quarters of the ongoing year or not yet filed.
  df_sum_quarters_years <- df_sum_quarters_years %>% 
    mutate(
      row_not_to_add = 
        total_val_year == 0 & 
        total_quarters == 3 & 
        as.integer(frame_year) >= (as.integer(format(Sys.Date(), "%Y")) - 1)
    )  
  
  # Calculate the difference between the sum of the val of the fiscal years and the sum of the val of the quarters. The two sums are different for those past years where only some quarters are included in the "df_std_CF" dataframe. The records marked with "quarter_not_to_add" as FALSE are related to those quarters to be added to the  dataframe df_std_CF. Those marked TRUE are removed since these are related to the quarters of the ongoing year or not yet filed.
  df_sum_quarters_years <- df_sum_quarters_years %>% 
    # Calculate the number of quarters to add
    mutate(total_quarters_to_add = 4 - total_quarters) %>%
    # Remove the rows that are not representative of missing quarters.
    filter(row_not_to_add == FALSE & total_quarters_to_add > 0) %>% 
    # Calculate the total and quarterly value of missing quarters
    mutate(
      total_val_of_missing_quarters = total_val_year - total_val_quarters,
      val_missing_quarter = total_val_of_missing_quarters / total_quarters_to_add
    )    
  
  # Extract the missing quarters
  missing_quarters <- df_sum_quarters_years %>%
    select(standardized_cashflow_label,frame_year, val_missing_quarter) %>%
    # Duplicate each row four times to represent the four quarters in a year
    slice(rep(1:n(), each = 4)) %>%
    # Create a new column 'frame_quarter' based on the duplicated rows
    mutate(frame_quarter = as.integer(rep(c("1", "2", "3", "4"), n() / 4)))  
  
  # Create the missing rows for the missing quarters
  rows_to_add <- missing_quarters %>%
    anti_join(df_std_CF, by = c("standardized_cashflow_label","frame_year","frame_quarter")) %>%
    mutate(
      # Set end and start dates based on frame_quarter
      end = as.Date(case_when(
        frame_quarter == "1" ~ paste(frame_year, "-03-31", sep = ""),
        frame_quarter == "2" ~ paste(frame_year, "-06-30", sep = ""),
        frame_quarter == "3" ~ paste(frame_year, "-09-30", sep = ""),
        frame_quarter == "4" ~ paste(frame_year, "-12-31", sep = "")
      )),
      start = as.Date(case_when(
        frame_quarter == "1" ~ paste(frame_year, "-01-01", sep = ""),
        frame_quarter == "2" ~ paste(frame_year, "-04-01", sep = ""),
        frame_quarter == "3" ~ paste(frame_year, "-07-01", sep = ""),
        frame_quarter == "4" ~ paste(frame_year, "-10-01", sep = "")
      )),
      estimated_val = "TRUE"
    ) %>% 
    rename(val = val_missing_quarter) %>% 
    # Ungroup missing_rows
    ungroup()
  
  # Bind the rows to df_std_CF and fill in NA in added columns
  df_std_CF <- df_std_CF %>% 
    bind_rows(rows_to_add) %>% 
    group_by(standardized_cashflow_label) %>%
    mutate(
      # Fill in missing values in the added rows for description, label, and us_gaap_reference
      description = first(description),
      label = first(label),
      us_gaap_reference = first(us_gaap_reference),
      # Convert NA to FALSE in the estimated_val column
      estimated_val = ifelse(is.na(estimated_val), FALSE, estimated_val)
    ) %>%
    ungroup()  %>% 
    mutate(
      # Fill in missing values based on conditions for fy, fp, form, filed, and frame
      fy = ifelse(estimated_val == "TRUE", frame_year, df_std_CF$fy),
      fp = ifelse(estimated_val == "TRUE", paste0("Q", frame_quarter), df_std_CF$fp),
      form = ifelse(estimated_val == "TRUE", NA, df_std_CF$form),
      filed = ifelse(estimated_val == "TRUE", NA, df_std_CF$filed),
      frame = ifelse(estimated_val == "TRUE", paste0("CY", frame_year, "Q", frame_quarter), df_std_CF$frame)
    ) %>% 
    fill(
      # Fill missing values downward for selected columns
      accn, cik, entityName, sic, sicDescription, tickers,
      .direction = "down"
    ) %>% 
    arrange(desc(end), standardized_cashflow_label) 
  
  # Filter all rows that include the result of the fiscal year, retaining the ones that include only the results of the fiscal quarters.
  df_std_CF <- df_std_CF %>% 
    filter(!is.na(frame_quarter))
  
  # 03 - Pivot df_std_CF in a dataframe format ------------------------------------------------------
  # This code transforms the data from a long format with multiple rows per observation to a wide format where each observation is represented by a single row with columns corresponding to different Concepts
  
  df_std_CF <- df_std_CF %>%
    select(end,standardized_cashflow_label,val) %>% 
    # Pivot the data using standardized_cashflow_label as column names
    pivot_wider(
      names_from = standardized_cashflow_label,
      values_from = val
    ) %>%
    # Arrange the dataframe in descending order based on the 'end' column
    arrange(desc(end))
  
  # 04 - Add new columns for standardization -----------------------------------
  # This code add the missing columns to the df_std_CF based on the standardized_cashflow.xls and perform checks
  
  ## Step 1 - Check key financial Concepts -----------------------------------
  # It checks whether specific columns exist or are empty. If so it stops or remove corresponding rows
  if (!("Net Income (loss)" %in% colnames(df_std_CF)) || !("(Operating Activities) Cash Flow from Operating Activities" %in% colnames(df_std_CF)) || !("(Investing Activities) Cash Flow from Investing Activities" %in% colnames(df_std_CF)) || !("(Financing Activities) Cash Flow from Financing Activities" %in% colnames(df_std_CF))) {
    stop("Cash Flow from Operating activities or Investing activities or Financing activities is missing. The entity is not adequate for financial analysis.")
  }
  
  # Remove rows where key financial Concepts are empty (or NA)
  df_std_CF <- df_std_CF %>%
    filter(
      any(!is.na(`Net Income (loss)`) | `Net Income (loss)` != ""),
      any(!is.na(`(Operating Activities) Cash Flow from Operating Activities`) | `(Operating Activities) Cash Flow from Operating Activities` != ""),
      any(!is.na(`(Investing Activities) Cash Flow from Investing Activities`) | `(Investing Activities) Cash Flow from Investing Activities` != ""),
      any(!is.na(`(Financing Activities) Cash Flow from Financing Activities`) | `(Financing Activities) Cash Flow from Financing Activities` != "")
    )
  
  ## Step 2 - Add missing columns -----------------------------------
  # It checks which columns from columns_to_add are not already present in df_std_CF
  columns_to_add <- setdiff(standardized_cashflow$standardized_cashflow_label,colnames(df_std_CF)) 
  
  #It then adds only the missing columns to df_std_CF and initializes them with NA.
  if (length(columns_to_add) > 0) {
    
    # Add columns to the dataframe
    df_std_CF[,columns_to_add] <- NA
  }
  
  ## Step 3 - Calculate newly added columns columns -----------------------------------
  # Evaluate expressions for key financial Concepts 
  
  df_std_CF <- df_std_CF %>%
    mutate(
      `(Operating Activities) Change in Other Working Capital` = case_when(
        is.na(`(Operating Activities) Change in Other Working Capital`) ~ 
          coalesce(`(Operating Activities) Cash Flow from Operating Activities`,0) - 
          (coalesce(`(Operating Activities) Cash Flow Depreciation, Depletion, Ammortization`,0) +
             coalesce(`(Operating Activities) Change in Accounts Receivable`,0) +
             coalesce(`(Operating Activities) Change in Inventory`,0) +
             coalesce(`(Operating Activities) Change in Prepaid expenses and other assets`,0) +
             coalesce(`(Operating Activities) Change in Accounts Payable`,0) +
             coalesce(`(Operating Activities) Change in Reserve for Sales Return and allowances`,0) +
             coalesce(`(Operating Activities) Deferred Income Tax`,0) +
             coalesce(`(Operating Activities) Stock-based Compensation`,0)
          ),
      ),
      
      `(Investing Activities) Gain (Losses) in Other Investing Activities` = case_when(
        is.na(`(Investing Activities) Gain (Losses) in Other Investing Activities`) ~ 
          coalesce(`(Investing Activities) Cash Flow from Investing Activities`,0) - 
          (coalesce(`(Investing Activities) Purchase of Property, Plant and Equipment`,0) + 
             coalesce(`(Investing Activities) Proceeds from Asset Sales`,0) +
             coalesce(`(Investing Activities) Purchase of Businesses`,0) +
             coalesce(`(Investing Activities) Purchase of Marketable Securities and Investment`,0) +
             coalesce(`(Investing Activities) Proceeds from sale of Marketable Securities and Investment`,0) +
             coalesce(`(Investing Activities) Proceeds from maturities of Marketable Securities and Investment`,0)
          ),
      ),
      
      `(Financing Activities) Impact of Stock Options and Other` = case_when(
        is.na(`(Financing Activities) Impact of Stock Options and Other`) ~ 
          coalesce(`(Financing Activities) Impact of Stock Options and Other`,0) - 
          (coalesce(`(Financing Activities) Proceeds from Issuance of Stock`,0) + 
             coalesce(`(Financing Activities) Payment for Repurchase of Stock`,0) + 
             coalesce(`(Financing Activities) Proceeds from Issuance of Debt`,0) + 
             coalesce(`(Financing Activities) Payment of Debt`,0) + 
             coalesce(`(Financing Activities) Cash for Dividends`,0) 
          ),
      ),
      
    ) %>% 
    mutate_all(~round(., digits = 4))  # Adjust the number of digits as needed
  
  ## Step 4 - Order columns based on standardized_cashflow_label -----------------------------------
  custom_order <- unique(standardized_cashflow[,1])
  # Reorder the columns as per standardized_cashflow.xlsx
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
