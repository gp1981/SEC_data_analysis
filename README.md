# SEC Data Analysis

## Overview

This repository contains R scripts for retrieving and analyzing company data from the U.S. Securities and Exchange Commission (SEC) filings. It provides a framework to access information about publicly traded companies and perform data analysis based on their SEC filings.

## Purpose

The primary purpose of this project is to retrieve company data from SEC filings and analyze it for educational and research purposes. Please note that the scripts provided here are not intended for making investment decisions, and their use is at your own risk.

## Files

1. **main_script.R**: The main script that orchestrates data retrieval and analysis.
   - **Author**: gp1981
   - **Disclaimer**: This script is intended for educational purposes only and should not be used for investment decisions. Use at your own risk.
  
2. **code/Functions/data_retrieval.R**: Contains the script to retrieve company data from SEC filings.
   - **Author**: gp1981
   - **Disclaimer**: This script is intended for educational purposes only and should not be used for investment decisions. Use at your own risk.

3. **code/Functions/data_analysis.R**: Contains the script for processing and analyzing SEC data.
   - **Author**: gp1981
   - **Disclaimer**: This script is intended for educational purposes only and should not be used for investment decisions. Use at your own risk.

4. **code/Functions/data_visualization.R**: Contains the script for processing, analyzing and visualizing SEC data.
   - **Author**: gp1981
   - **Disclaimer**: This script is intended for educational purposes only and should not be used for investment decisions. Use at your own risk.
  
5. **code/Functions/utils.R**: Contains the script of utilities functions for processing, analyzing and visualizing SEC data.
   - **Author**: gp1981
   - **Disclaimer**: This script is intended for educational purposes only and should not be used for investment decisions. Use at your own risk.

## Getting Started

1. Clone the repository to your local machine:

   ```bash
   git clone https://github.com/gp1981/SEC_Data_Analysis.git

2. Install the required R libraries by running the following command:

   ```bash
   install.packages("httr")
   install.packages("jsonlite")
   install.packages("tidyverse")
   install.packages("dplyr")

3. Run the main_script.R script to retrieve and analyze SEC data.

## Quarto Website

We are developing a Quarto website as part of this project. You can access the book at the following link:

[Fundamental analysis of SEC data](#) - Link to your rendered book (will be replaced with the actual link)

## Rendering the Quarto Book Locally

If you want to render the Quarto book on your local machine, proceed with the following additional steps:


4.  **Install Quarto**: If you haven't already, you'll need to install Quarto. You can install it using the following command:

    ```bash 
    npm install -g quarto

5.  **Render the Quarto Book**: Navigate to the /quarto directory and use the Quarto CLI to render the book::

    ```bash 
    cd quarto quarto render 

The rendered book will be available in the [docs](./docs) directory. Alternatively, in the [quarto](./quarto) directory you can open the index.qmd file and click "Render"

## Contribution

Contributions are welcome! If you have any ideas for improvements or new features, please open an issue or submit a pull request. For major changes, please discuss the changes first by opening an issue.

## License

This project is licensed under the [MIT License](https://github.com/gp1981/SEC-Data-Analysis/blob/63ef4ecb1ef9a40ca64bdcb67ede14c0a7ab10c6/LICENSE). See the LICENSE file for details.

## Disclaimer

This project is for educational purposes only and should not be used for investment decisions. Use the scripts at your own risk.

## Contact

For questions or feedback, feel free to contact [gp1981](45032495+gp1981@users.noreply.github.com).

Enjoy analyzing SEC data!
