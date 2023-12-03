# Author: gp1981
# Purpose: Contains the script for processing, analyzing, and visualizing SEC data.
# Disclaimer: This script is intended for educational purposes only and should not be used for investment decisions. Use at your own risk.

# Function to unnest list company_Facts ----------------------------------

# Un-nest the company_Facts (and nested unit list)
FactsList_to_Dataframe <- function(company_Facts_us_gaap) {
  df_Facts <- company_Facts_us_gaap %>%
    tibble() %>%
    unnest_wider(col = everything()) %>%
    unnest(cols = c(units)) %>%
    unnest(cols = c(units))
  
  df_Facts <- as.data.frame(df_Facts)
  
  # Mutate to reduce values in millions by dividing by 1 million
  df_Facts <- df_Facts %>%
    mutate(val = val / 1e6)
  
  return(df_Facts)
}

