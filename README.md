This repository contains all code and data used to build an interactive Shiny application that predicts chronological age 
using demographic and health variables from the NHANES dataset. The project includes:
Data cleaning + imputation pipelines, Multiple regression models, Preprocessed .rds datasets, The final deployed Shiny app (app.R), 
Additional scripts used during model development

**Repository Structure**
.
├── rsconnect/shinyapps.io/wlady/   # Deployment metadata
├── .gitignore
├── Data.RData                      # Saved R workspace
├── HANESATTEMPT.Rproj              # RStudio project file
├── WORKING MODEL.R                 # Data cleaning + model building
├── app.R                           # Final Shiny app
├── comp1.rds
├── comparison_table_round.rds
├── df_prep_clean.rds
├── fit_red_1.rds
├── fit_reduced_mids.rds
├── nhanes_comp1.rds
├── nhanes_imp_age_5m.rds
├── tidy_reduced_table.rds
All .rds files are required for the app to run locally.

**Required R Packages**
install.packages(c(
  "shiny", "dplyr", "ggplot2", "broom", "mice",
  "Metrics", "naniar", "tidyr", "tibble"
))

Optional (if your scripts use NHANES helpers):
install.packages("NHANES")

**Running the App Locally**

**1. Clone or Download**
Clone:
git clone https://github.com/wlady-sys/NHanesAttemptPredictor.git
cd NHanesAttemptPredictor
OR download as ZIP from GitHub → Code → Download ZIP and unzip.

**2. Open the RStudio Project**
Open:
HANESATTEMPT.Rproj
(This sets working paths correctly.)

**3. Install dependencies**
install.packages(c(
  "shiny", "dplyr", "ggplot2", "broom", "mice",
  "Metrics", "naniar", "tidyr", "tibble"
))
**4. Verify .rds files are present**
Ensure files like:
comp1.rds
df_prep_clean.rds
fit_reduced_mids.rds
remain in the project root next to app.R.

**5. Run the Shiny App**
Option A: From RStudio
Open app.R → click Run App
Option B: From console
shiny::runApp()
The app will launch in your browser.

**What the App Does**
Inside the app, users can:
Explore which predictors influence age
Input their own values and get predicted age
Compare demographic-only vs health-only models
Visualize model fit and predictor effects
Examine cleaned NHANES data and imputed values
The app supports both interactive prediction and exploratory analysis.

**Model Development**
Dataset preparation includes:
Dropping variables missing >50%
Age filters (8–79, matching NHANES structure)
Rule-based imputations for logical missingness
MICE imputation for random missing values (~5%)
Modeling workflow:
Linear models built on cleaned dataset
Redundant/collinear predictors removed

Comparison models saved as:
fit_red_1.rds
fit_reduced_mids.rds
comparison_table_round.rds

To reproduce the model pipeline:
source("WORKING MODEL.R")

**Troubleshooting**
App complains about missing files?
Check that all required .rds files are in the same folder as app.R.
“MICE” warnings?
These are expected during imputation and do not break the app.
Deployment metadata errors?
You can ignore or delete the rsconnect/ folder when running locally.
