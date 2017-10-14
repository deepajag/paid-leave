# Balancing work and care: the effect of paid adult family leave policies on employment 

This repository contains all the code for the primary and senstivity analyses for the paper [Reference].

Instructions for reproducing our results
1. Data
a). Get access to the data at https://share-project.centerdata.nl/sharedatadissemination/users/login
b). Download the Job Episodes Panel from Wave 3 (Sharelife), which should be called something like "sharewX_rel5-0-0_gv_job_episodes_panel.dta"
c). Save this dataset in a new folder somewhere convenient and rename this file to "share_jobepisodes_condensed1.dta"

2. RStudio
a). Download R and RStudio (https://cran.r-project.org/bin/windows/base/) (https://www.rstudio.com/)

3. Get the files
*For Git Users*
a) Clone my repository to your computer
b) Place the dataset into the same directory as the cloned repository

*For non-git Users*
a) Download all the files as a zip file (option on top right)
b) Place in the same directory as the dataset

4. Run the results

**For the main analysis**
Run modelling_script.R
-The table of descriptives and the primary results will be saved as a text file in the same directory as 'table1' and 'all-results-final'

**For the main sensitivity analysis**
Run sensitivity_analysis.R
-A new directory called lead_lag will be created and all plots saved here

**For the supplemental sensitivity analysis**
Run supplemental_sensitivity_analyses.Rmd
-A new word document will be generated replicated our Supplemental File 2
