# Norepinephrine-Time-Dependent-Effects

This repository contains SQL, R, and Python code to analyze the time-dependent effects of initial norepinephrine (NE) infusion on mean arterial pressure (MAP) in ICU patients with shock.
Using data from seven ICUs in Japan (2013–2024), the scripts extract invasive MAP measurements from 30 minutes before to 120 minutes after NE initiation and apply statistical models to compare blood pressure trajectories across different starting NE doses.

---
## Table of Contents
- [Overview](#overview)
- [Repository Structure](#repository-structure)
- [Requirements](#requirements)
- [Usage](#usage)
  - [SQL Queries](#sqlqueries)
  - [R Scripts](#rscripts)
  - [Python Scripts](#pythonscripts)
- [Contact](#contact)
- [License](#license)

---
## Overview
The Norepinephrine-Time-Dependent-Effects repository includes:

1. SQL code to extract invasive MAP and relevant clinical data from the OneICU database.
2. R scripts to clean data, fit statistical models, and generate figures showing MAP changes before and after initial norepinephrine infusion.
3. Python code used specifically for applying a low-pass filter to MAP measurements.

By running these scripts, researchers can reproduce the analysis of time-dependent MAP trajectories across different starting NE doses and patient subgroups.

---
## Repository Structure
```
Norepinephrine-Time-Dependent-Effects
├── README.md
├── sql
├── R_scripts
└── python_scripts
└── ...
```
- sql
  - SQL scripts to extract invasive MAP measurements and clinical variables from the OneICU database in Google BigQuery.
- R_scripts
  - Contains R scripts for data cleaning, statistical modeling, and figure generation showing MAP changes before and after norepinephrine infusion.
- python_scripts
  - Contains Python scripts for applying a low-pass filter to MAP measurements.

---
## Requirements
1. Google BigQuery Access
    - To run the SQL scripts, you will need access to Google BigQuery and appropriate credentials to query the OneICU database.
2. R
    - R (version 4.0 or higher recommended) is required to run the R scripts for data cleaning, statistical modeling, and figure generation.figures.
3. R Packages
    - Common data analysis packages like tidyverse, ggplot2, etc.
    - Check the top of each R script for specific library requirements.
4. Python
    -  Python (version 3.8 or higher recommended) is required only for applying a low-pass filter to MAP measurements.

---
## Usage
### SQL Queries
1. Navigate to the `sql/OneICU` directory.
2. Open the SQL script of interest.
3. Copy the script into your BigQuery console.
4. Run the query
  - Ensure you have access to the OneICU database and that your [BigQuery billing project](#https://cloud.google.com/resource-manager/docs/creating-managing-projects?hl=ja) is configured correctly.

### R Scripts
1. Clone this repository or download the files locally.
2. Open your R environment (RStudio or equivalent).
3. Install any missing R packages with:
  ```r
  install.packages("<package_name>")
  ```
4. Run the scripts in the `R_scripts` folder in the recommended order to:
   -  Load query outputs.
   -  Perform data cleaning and statistical modeling.
   -  Generate figures showing MAP changes before and after norepinephrine infusion.

### Python Scripts
1. Install Python (3.8 or higher recommended).
2. Use the provided Python script to apply a low-pass filter to MAP measurements before running the R analysis.

---
## Contact
For questions or collaboration inquiries, please reach out to us by email:
 - [MeDiCU, Inc.](mailto:info@medicu.co.jp)

---
## License
This project is licensed under the GNU General Public License (GPL) - see the [LICENSE.md](#https://github.com/takapion/Norepinephrine-Time-Dependent-Effects/blob/main/LICENSE) file for details.

---
**Disclaimer:**
The code in this repository is provided for academic research and educational purposes. Individual patient data are not provided.
