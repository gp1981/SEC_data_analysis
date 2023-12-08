# Author: gp1981
# Purpose: Contains the script of utility functions for processing, analyzing, and visualizing SEC data.
# Disclaimer: This script is intended for educational purposes only and should not be used for investment decisions. Use at your own risk.

# Import required libraries
library(tidyverse)
library(purrr)

# Function to unnest a list
unnest_list <- function(x) {
  purrr::map(x, ~{
    if (is.list(.x)) {
      unnest_list(.x)
    } else {
      .x
    }
  })
}

