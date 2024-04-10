# ADI HV Analysis

This repository contains the code and analysis for the ADI HV project. The main focus is on exploring distributions, creating pairwise plots, performing linear mixed regression modeling, handling missing data, and conducting sensitivity analyses related to imaging data quality control.

## Contents

- **data_11_20.csv**: This is the csv file for the included participants for all main analysis (N = 10114)
- **data_2_14_2.csv**: This is the csv file for all participants (including those that have missing data (N=11876) 

- **ADI_HV_Plots_EY.Rmd**: This R Markdown file includes code for generating distributions and pairwise plots of the data. These visualizations serve as a preliminary step before conducting linear mixed modeling to understand the relationships between variables and check for any potential issues or patterns.

- **ADI_HV_Missing_EY_0326.Rmd**: In this R Markdown file, we compare the excluded and included data to assess any differences or biases. We also check for missing values and handle them appropriately.

- **ADI_HV_regression_EY_0326.Rmd**: Contains the linear mixed regression model analysis. It investigates the relationships between the dependent variable and independent variables while accounting for random effects. The file also includes interaction analysis to examine potential moderating effects of school and family environment.

- **ADI_HV_Sensitivity.R**: Focuses on sensitivity analysis for imaging data quality control. It explores the analysis on only a subset of participants, assessing the robustness of our findings.

## Usage

To reproduce the analysis, follow these steps:

1. Clone the repository to your local machine.
2. Open the desired R Markdown file (.Rmd) in RStudio or your preferred IDE.
3. Ensure that the necessary dependencies and datasets are available.
4. Run the code chunks in the R Markdown file to generate the analysis output, including plots, tables, and model results.

## Dependencies

The analysis relies on the following R packages:

- `tidyverse` for data manipulation and visualization
- `lme4` and `lmerTest` for linear mixed modeling
- `ggplot2` and `GGally` for creating plots
- `knitr` for generating reports
- `dplyr` for data manipulation and filtering
- `interactions` for interaction analysis

Please ensure that these packages are installed before running the analysis.


## Contact

If you have any questions or suggestions regarding this project, please feel free to contact the repository owner or open an issue on GitHub.
