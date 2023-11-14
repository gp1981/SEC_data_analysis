---
editor_options: 
  markdown: 
    wrap: 72
---

# SEC Data Analysis

## Overview

Welcome to the SEC Data Analysis project! This repository contains R
scripts designed to retrieve and analyze company data from U.S.
Securities and Exchange Commission (SEC) filings. The primary goal is to
provide a framework for accessing information about publicly traded
companies and conducting data analysis based on their SEC filings.

**Note:** *the project is being developed. WIP indicates Work In
Progress*

## Purpose

The main purpose of this project is to retrieve company data from SEC
filings and analyze it for educational and research purposes. It's
important to note that the scripts provided here are intended for
educational use only and should not be used for making investment
decisions. Use these scripts at your own risk.

## Project Structure

This project has two main components:

1.  **R Scripts for Data Retrieval and Analysis:**
    -   `main_script.R`: Orchestrates data retrieval and analysis.
    -   `code/Functions/data_retrieval.R`: Retrieves company data from
        SEC filings.
    -   `code/Functions/data_analysis.R`: Processes and analyzes SEC
        data.
    -   `code/Functions/data_visualization.R`: Processes, analyzes, and
        visualizes SEC data.
    -   `code/Functions/utils.R`: Utility functions for processing,
        analyzing, and visualizing SEC data.
2.  **Quarto Website Development:**
    -   The `/quarto` directory contains Quarto (.qmd) files, forming a
        website that serves as a guide to SEC data analysis with R.
    -   `index.qmd`: Introduction and overview of the project.
    -   `01_data_retrieval.qmd`: Explains the process of retrieving
        financial data from SEC filings.
    -   `02_data_exploration.qmd`: Delves into the structure of the SEC
        data obtained from the API.
    -   `03_data_analysis.qmd`, `04_data_visualization.qmd`,
        `06_advanced_topics.qmd`: Currently placeholders for future
        content.
    -   `11_AAPL.qmd`: Placeholder for specific analysis on Apple Inc.
    -   `A1_main_script.qmd`, `A2_Functions.qmd`: Placeholder for
        detailed explanations of the main script and functions.

## Getting Started

1.  Clone the repository to your local machine:

    \`\`\`bash git clone
    <https://github.com/gp1981/SEC_Data_Analysis.git>

2.  Install the required R libraries by running the following command:

    \`\`\`bash install.packages("httr") install.packages("jsonlite")
    install.packages("tidyverse") install.packages("dplyr")

### Data analysis in R

Run the main_script.R script to retrieve and analyze SEC data.

### Quarto Website

Quarto website is part of this project. You can access the book at the
following link:

[Unlocking Financial Insights: A Guide to SEC Data Analysis with
R](https://gp1981.github.io/SEC_data_analysis/) - Link to the page
created

## Rendering the Quarto website Locally

To render the Quarto website on your local machine:

1.  **Install Quarto**: If not installed, run

\`\`\`bash npm install -g quarto

2.  **Render the Quarto website**: Navigate to the `/quarto` directory
    and use the Quarto CLI to render the book::

\`\`\`cd quarto \`\`\`quarto render

Alternatively, in the [quarto](./quarto) directory you can open the
index.qmd file and click "Render"

The rendered website will be available in the [docs](./docs) directory.

## Contribution

Contributions are welcome! If you have any ideas for improvements or new
features, please open an issue or submit a pull request. For major
changes, please discuss the changes first by opening an issue.

## License

This project is licensed under the [MIT
License](https://github.com/gp1981/SEC-Data-Analysis/blob/63ef4ecb1ef9a40ca64bdcb67ede14c0a7ab10c6/LICENSE).
See the LICENSE file for details.

## **Disclaimer**

*This script is intended for educational purposes only and should not be
used for investment decisions. Use at your own risk.*

## Contact

For questions or feedback, feel free to contact
[gp1981](45032495+gp1981@users.noreply.github.com).

Enjoy analyzing SEC data!
