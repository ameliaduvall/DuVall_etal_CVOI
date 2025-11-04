## *Use of Constructed Value of Information to identify critical uncertainties in marine bird conservation* 

#### Amelia J DuVall, David Mazurkiewicz, Josh Adams, Yuliana Bedolla Guzmán, Russell W Bradley, Catherine A Carter, Kenneth Convery, Andrew J Dennhardt, Nick D Holmes, Jim A Howard, Michael E Johns, Annie Little, Stacey D Ostermann-Kelm, Michael W Parker, David Pereksta, Dan Robinette, Marc D Romano, T Scott Sillett, and Sarah J Converse

##### Please contact the first author for questions about the code or data: Amelia J DuVall (ajduvall@uw.edu)
##### Secondary contact: Sarah J Converse (sconver@usgs.gov)

_______________________________________________________________________________________

## Abstract

(Add Abstract here) 

### Table of Contents 


### [scripts](./scripts)

Contains scripts to run all analyses. 

1. [01_format_data](./scripts/01_format_data) 
2. [02_run_analyses](./scripts/02_run_analyses) 
3. [03_make_figures](./scripts/03_make_figures) 
 
### [data](./data) 

Contains final scoring forms (.xlsx) for 12 experts. 

### [results](./results)

Contains raw and processed results files (.rds and .csv).   

### [figures](./figures)

Contains pdf versions of all figures in manuscript. 

### Required Packages and Versions Used 

R version 4.4.1 (2024-06-14)  
Platform: aarch64-apple-darwin20  
Running under: macOS 15.6.1  

cowplot_1.2.0    
dplyr_1.1.4  
ggplot2_4.0.0  
ggrepel_0.9.6   
ggstance_0.3.7   
here_1.0.1   
janitor_2.2.0   
jcolors_0.0.5   
RColorBrewer_1.1-3   
readxl_1.4.3   
scales_1.4.0   
tidyverse_2.0.0   
viridis_0.6.5   

### Details of Article 

DuVall AJ, Mazurkiewicz DM, Adams J, Bedolla Guzman Y, Bradley RW, Carter CA, Convery K, Dennhardt AJ, Holmes ND, Howard JA, Johns ME, Little A, Ostermann-Kelm SD, Parker MW, Pereksta D, Robinette D, Romano MD, Sillett TS & Converse SJ. In prep. Use of constructed value of information to identify critical uncertainties in marine bird conservation. XXXXXXXX.  

### How to Use this Repository 

- Compile expert elicitation data by running [load_round_2.R](./scripts/01_format_data/load_round_2.R) in the [01_format_data](./scripts/01_format_data) subfolder in the [scripts](./scripts) folder. Compiled scores have also been saved in the [results](./results) folder as [results/round_2_all_results.RDS](./results/results/round_2_all_results.RDS).
- To calculate CVOI as well as mean scores across components for each species, run [calculate_cvoi.R](./scripts/02_run_analyses/calculate_cvoi.R) in the [02_run_analyses](./scripts/02_run_analyses) subfolder in the [scripts](./scripts) folder.
- To bootstrap results by species, run [bootstrap_by_spp.R](./scripts/02_run_analyses/bootstrap_by_spp.R) in the [02_run_analyses](./scripts/02_run_analyses) subfolder in the [scripts](./scripts) folder.
- To bootstrap result across species, run [bootstrap_across_spp.R](./scripts/02_run_analyses/bootstrap_across_spp.R) in the [02_run_analyses](./scripts/02_run_analyses) subfolder in the [scripts](./scripts) folder.

