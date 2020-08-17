
This repository contains the scripts for the CCA analysis.

# Data 
- Is stored in csv files and these files are currently ignored by git. 
## raw_data
- contains the raw data files extracted directly from the database
## scored_data
- contains all data processed using scoring or CCA scripts


# 1_CCA_data_scoring.R 
- This script scores the raw data, and then outputs a new csv file to the scored data folder

# CCA_descr_stats.Rmd 
- Creates the descriptive statistics and various plots for displaying them.

# CCA_func.R
- Contains functions to run the CCA used in 2/2a/2b.

# 2_basic_cca_script.Rmd 
- This is the basic script for conducting the cca. It computes the statistics important for write up of the the results. Covariates grouped with empathy variables.

# 2a and 2b 
 - Alternate models for covariates being grouped with internalising variables, or being regressed out before cca.

# 3, 3a and 3b 
- The permutation testing to get non-parametric p-values (a and b are the permutations associated with the alternative models run in 2a and 2b).

# 4, 4a and 4b
- The values produced from the permutations made in 3/3a/3b.

# reprotables folder
- Creates reproducible APA style tables for use in manuscript
