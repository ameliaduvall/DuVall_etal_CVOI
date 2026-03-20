## Calculate CVOI

## load libraries
library(here)
library(tidyverse)
library(janitor)

## load data
# source(here("scripts", "01_format_data", "load_round_2.R"))

## calculate cvoi and summarize
all_species_summary <- full_df %>%
  # filter experts that did not respond for a particular species
  filter(!is.na(rel_a)) %>%
  mutate(
    # if rel A is 0, subsequent components are 0 (rel B, mag) or NA (red)
    rel_b = ifelse(rel_a == 0, 0, rel_b),
    mag = ifelse(rel_a == 0, 0, mag),
    red = ifelse(rel_a == 0, NA, red),
    # rescale rel both
    rel_both = (rel_a * rel_b) / 4,
    # calculate CVOI
    cvoi = mag * rel_both
  ) %>%
  # group by metadata
  group_by(species, category, hypothesis, code, name, iucn_name) %>% 
  # calculate stats
  summarize(
    n_ex_total = n_distinct(expert),
    num_experts_rel = sum(rel_a > 0, na.rm = TRUE),
    # CVOI stats (denominator: total # experts)
    cvoi_mean = sum(cvoi, na.rm = TRUE) / n_ex_total,
    cvoi_lci  = quantile(cvoi, 0.05, na.rm = TRUE),
    cvoi_uci  = quantile(cvoi, 0.95, na.rm = TRUE),
    # reducibility stats (denominator: rel experts only)
    red_mean = sum(red, na.rm = TRUE) / num_experts_rel,
    red_lci  = quantile(red, 0.05, na.rm = TRUE),
    red_uci  = quantile(red, 0.95, na.rm = TRUE),
    # rel A stats (denominator: total # experts)
    rel_a_mean = sum(rel_a, na.rm = TRUE) / n_ex_total,
    rel_a_lci  = quantile(rel_a, 0.05, na.rm = TRUE),
    rel_a_uci  = quantile(rel_a, 0.95, na.rm = TRUE),
    # rel B stats (denominator: total # experts)
    rel_b_mean = sum(rel_b, na.rm = TRUE) / n_ex_total,
    rel_b_lci  = quantile(rel_b, 0.05, na.rm = TRUE),
    rel_b_uci  = quantile(rel_b, 0.95, na.rm = TRUE),
    # rel both stats (denominator: total # experts)
    rel_both_mean = sum(rel_both, na.rm = TRUE) / n_ex_total,
    rel_both_lci  = quantile(rel_both, 0.05, na.rm = TRUE),
    rel_both_uci  = quantile(rel_both, 0.95, na.rm = TRUE),
    # magnitude stats (denominator: total # experts)
    mag_mean = sum(mag, na.rm = TRUE) / n_ex_total,
    mag_lci  = quantile(mag, 0.05, na.rm = TRUE),
    mag_uci  = quantile(mag, 0.95, na.rm = TRUE),
    .groups = "drop"
  )

## split by species
species_list <- split(all_species_summary, all_species_summary$species)
# write.csv(all_species_summary, here("results", "round_2_summary_all_species.csv"))

# For Ashy Storm-Petrel
assp_results <- species_list[["ASSP"]]
write.csv(assp_results, here("results", "assp_summary.csv"))

# For California Brown Pelican
brpe_results <- species_list[["BRPE"]]
write.csv(brpe_results, here("results", "brpe_summary.csv"))

# For Cassin's Auklet
caau_results <- species_list[["CAAU"]]
write.csv(caau_results, here("results", "caau_summary.csv"))

# For Scripps's Murrelet
scmu_results <- species_list[["SCMU"]]
write.csv(scmu_results, here("results", "scmu_summary.csv"))

# For Snowy Plover
snpl_results <- species_list[["SNPL"]]
write.csv(snpl_results, here("results", "snpl_summary.csv"))

# For Western Gull
wegu_results <- species_list[["WEGU"]]
write.csv(wegu_results, here("results", "wegu_summary.csv"))
