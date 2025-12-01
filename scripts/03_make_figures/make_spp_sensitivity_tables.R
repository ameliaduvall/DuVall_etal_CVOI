## Sensitivity Tables

## load libraries
library(here)
library(tidyverse)

assp_sensitivity <- readRDS(here("results", "assp_sensitivity.rds"))
brpe_sensitivity <- readRDS(here("results", "brpe_sensitivity.rds"))
caau_sensitivity <- readRDS(here("results", "caau_sensitivity.rds"))
scmu_sensitivity <- readRDS(here("results", "scmu_sensitivity.rds"))
snpl_sensitivity <- readRDS(here("results", "snpl_sensitivity.rds"))
wegu_sensitivity <- readRDS(here("results", "wegu_sensitivity.rds"))

## hypotheses
h_number <- c("H1", "H2", "H3", "H4", "H5", "H6", "H7", "H8", "H9", "H10",
              "H11", "H12", "H13", "H14", "H15", "H16", "H17", "H18", "H19", "H20",
              "H21", "H22", "H23", "H24", "H25", "H26", "H27", "H28")

h_name <- c("Overflights", "Recreation", "Fishery Equipment", "Overfishing",            
                 "Native Predators", "Pinnipeds", "Seabird Colonization", "Oil Platforms",          
                 "Offshore Wind", "Shipping Traffic", "Artificial Light",       
                 "Invasive Plants", "Black Rats", "New Invasive Species",   
                 "Oil Spills", "Residual DDT", "Contaminants",          
                 "Sea Level Rise", "Marine Productivity", "Sea Surface Temperature",
                 "Precipitation", "Ambient Temperature", "Marine Heat Waves",     
                 "Storm Events", "Wind", "Harmful Algal Blooms",  
                 "Disease", "Parasites")

## assp table
colnames(assp_sensitivity) <- c("Highest Priority", "High Priority", "Medium Priority", "Low Priority")

assp_tbl <- bind_cols(h_number, h_name, assp_sensitivity) %>%
  rename("Hypothesis number" = "...1",
         "Hypothesis name" = "...2") 

write.csv(assp_tbl, here("results", "assp_sensitivity_tbl.csv")) 

## brpe table
colnames(brpe_sensitivity) <- c("Highest Priority", "High Priority", "Medium Priority", "Low Priority")

brpe_tbl <- bind_cols(h_number, h_name, brpe_sensitivity) %>%
  rename("Hypothesis number" = "...1",
         "Hypothesis name" = "...2")

write.csv(brpe_tbl, here("results", "brpe_sensitivity_tbl.csv")) 

## caau table
colnames(caau_sensitivity) <- c("Highest Priority", "High Priority", "Medium Priority", "Low Priority")

caau_tbl <- bind_cols(h_number, h_name, caau_sensitivity) %>%
  rename("Hypothesis number" = "...1",
         "Hypothesis name" = "...2")

write.csv(caau_tbl, here("results", "caau_sensitivity_tbl.csv")) 

## scmu table
colnames(scmu_sensitivity) <- c("Highest Priority", "High Priority", "Medium Priority", "Low Priority")

scmu_tbl <- bind_cols(h_number, h_name, scmu_sensitivity) %>%
  rename("Hypothesis number" = "...1",
         "Hypothesis name" = "...2")

write.csv(scmu_tbl, here("results", "scmu_sensitivity_tbl.csv")) 

## snpl table
colnames(snpl_sensitivity) <- c("Highest Priority", "High Priority", "Medium Priority", "Low Priority")

snpl_tbl <- bind_cols(h_number, h_name, snpl_sensitivity) %>%
  rename("Hypothesis number" = "...1",
         "Hypothesis name" = "...2")

write.csv(snpl_tbl, here("results", "snpl_sensitivity_tbl.csv")) 

## wegu table
colnames(wegu_sensitivity) <- c("Highest Priority", "High Priority", "Medium Priority", "Low Priority")

wegu_tbl <- bind_cols(h_number, h_name, wegu_sensitivity) %>%
  rename("Hypothesis number" = "...1",
         "Hypothesis name" = "...2")

write.csv(wegu_tbl, here("results", "wegu_sensitivity_tbl.csv")) 
