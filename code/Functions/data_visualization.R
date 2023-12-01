# Author: gp1981
# Purpose: Contains the script for processing, analyzing, and visualizing SEC data.
# Disclaimer: This script is intended for educational purposes only and should not be used for investment decisions. Use at your own risk.

# Function to unnest list company_Facts ----------------------------------

# Un-nest the company_Facts (and nested unit list)
FactsList_to_Dataframe <- function(company_Facts_us_gaap){
  df_Facts <- company_Facts_us_gaap %>%
    tibble() %>%
    unnest_wider(col = everything()) %>%
    unnest(cols = c(units)) %>%
    unnest(cols = c(units)) 
  
  # Format numbers in the val column as millions
  df_Facts$val <- scales::number(df_Facts$val / 1e6, accuracy = 0.1, suffix = "M")
  
  return(df_Facts)
}
