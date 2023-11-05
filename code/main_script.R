# Author: gp1981
# Purpose: The main script that orchestrates data retrieval and analysis.
# Disclaimer: This script is intended for educational purposes only and should not be used for investment decisions. Use at your own risk.

# Import required libraries (install if necessary)
library(tidyverse)
library(httr)
library(jsonlite)

# Sourcing required files
source("Functions/data_retrieval.R")
source("Functions/data_analysis.R")
source("Functions/data_visualization.R")
source("Functions/utils.R")


# Retrieve data from SEC --------------------------------------------------

# Define user headers
headers <- c('User-Agent' = 'email@address.com')

# Retrieve company list
company_List <- retrieve_Company_List(headers)

# Retrieve company information
cik <- company_List$cik[1]

company_Data <- retrieve_Company_Data(headers, cik)

# Explore data retrieved from SEC -----------------------------------------

# Assuming you have successfully retrieved company_Data
company_Metadata <- company_Data$company_Metadata
company_Facts <- company_Data$company_Facts
company_Concept <- company_Data$company_Concept

# Create a DataFrame with relevant data
company_df <- data.frame(
  CIK = cik,
  Name = company_Metadata$name,
  FiscalYearEnd = company_Metadata$fiscalYearEnd,
  AssetsLabel = company_Facts$facts$`us-gaap`$Assets$label,
  AssetsDescription = company_Facts$facts$`us-gaap`$Assets$description
  # Add more relevant fields here
)


## Company Metadata --------------------------------------------------------
# This includes general information about the company, such as its name and fiscal year end date.
company_Metadata <- company_Data$company_Metadata
company_Metadata$name        # Company name
company_Metadata$fiscalYearEnd  # Fiscal year end date
names(company_Metadata)


## Company Facts -----------------------------------------------------------
# These are specific financial facts reported by the company. You can access them based on their labels, e.g., "Assets."
company_Facts <- company_Data$company_Facts

assets_fact <- company_Facts$facts$`us-gaap`$Assets
names(assets_fact)
head(assets_fact)

assets_label <- assets_fact$label          # Label of the fact ("Assets")
assets_description <- assets_fact$description  # Description of the fact

names(company_Facts$facts$dei)
names(company_Facts$facts$`us-gaap`)

fact_df <-facts_to_dataframe(company_Facts)

## Company Concepts --------------------------------------------------------
# This contains detailed information related to a specific concept (e.g., "Assets"). You can access facts under this concept.

company_Concept <- company_Data$company_Concept
us_gaap_assets_facts <- company_Concept$tag  # Access the "Assets" concept


# A simplified balance sheet ------------------------------------------------------






