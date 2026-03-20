## Bootstrapping - Linear Confidence Weighting

library(here)
library(tidyverse)
library(janitor)

## 1. Data prep
data_prepped <- full_df %>%
  # exclude experts who provided no data for a species 
  filter(!is.na(rel_a)) %>%
  mutate(
    # linear conf weights
    conf_weight = case_when(
      conf == 2 ~ 1.0,
      conf == 1 ~ 0.5,
      conf == 0 ~ 0.01,
      TRUE ~ 0.01
    ),
    # if rel a is 0, set other components
    rel_b = ifelse(rel_a == 0, 0, rel_b),
    mag = ifelse(rel_a == 0, 0, mag),
    red = ifelse(rel_a == 0, NA, red),
    # rescale rel_both 
    rel_both = (rel_a*rel_b)/4, 
    cvoi = mag * rel_both
  )

## bootstrap function
run_cvoi_boot_linear <- function(df, group_vars, mode = "standard", n_reps = 1000) {
  ## generate 1000 bootstrap replicates
  boot_reps <- map_df(1:n_reps, function(i) {
    ## resample experts with replacement w/in each species
    resampled_data <- df %>%
      group_by(species) %>%
      group_modify(~ {
        experts_in_spp <- unique(.x$expert)
        n_to_sample <- length(experts_in_spp)
        resampled_ids <- sample(experts_in_spp, n_to_sample, replace = TRUE)
        map_dfr(resampled_ids, function(ex) filter(.x, expert == ex))
      }) %>%
      ungroup()
    
    ## calculate weighted means 
    iteration_means <- resampled_data %>%
      group_by(species, hypothesis, code, name, iucn_name) %>%
      summarize(
        # use weighted.mean based on conf_weight
        spp_hypo_cvoi = weighted.mean(cvoi, w = conf_weight, na.rm = TRUE),
        spp_hypo_red  = weighted.mean(red, w = conf_weight, na.rm = TRUE),
        .groups = "drop"
      )
    
    ## diff forms of bootstrapping
    if (mode == "by_species") {
      iteration_means %>%
        rename(rep_cvoi = spp_hypo_cvoi, rep_red = spp_hypo_red) %>%
        mutate(rep_id = i)
    } else if (mode == "global_hypo") {
      ## mean of means across all 6 species
      iteration_means %>%
        group_by(hypothesis, code, name, iucn_name) %>%
        summarize(rep_cvoi = mean(spp_hypo_cvoi, na.rm = TRUE),
                  rep_red  = mean(spp_hypo_red, na.rm = TRUE),
                  .groups = "drop") %>%
        mutate(rep_id = i)
    } else if (mode == "global_iucn") {
      ## mean of means across all 6 species by IUCN
      iteration_means %>%
        group_by(iucn_name) %>%
        summarize(rep_cvoi = mean(spp_hypo_cvoi, na.rm = TRUE),
                  rep_red  = mean(spp_hypo_red, na.rm = TRUE),
                  .groups = "drop") %>%
        mutate(rep_id = i)
    }
  })
  
  ## calculate stats
  summary_stats <- boot_reps %>%
    group_by(across(all_of(group_vars))) %>%
    summarize(
      mean_cvoi = mean(rep_cvoi, na.rm = TRUE),
      mean_red  = mean(rep_red, na.rm = TRUE),
      cvoi_lci  = quantile(rep_cvoi, 0.05, na.rm = TRUE),
      cvoi_uci  = quantile(rep_cvoi, 0.95, na.rm = TRUE),
      red_lci   = quantile(rep_red, 0.05, na.rm = TRUE),
      red_uci   = quantile(rep_red, 0.95, na.rm = TRUE),
      .groups = "drop"
    )
  
  ## get quadrant boundaries
  med_cvoi <- median(summary_stats$mean_cvoi, na.rm = TRUE)
  med_red  <- median(summary_stats$mean_red, na.rm = TRUE)
  
  ## sensitivity analysis
  sensitivity <- boot_reps %>%
    left_join(summary_stats %>% select(all_of(group_vars), mean_cvoi, mean_red), by = group_vars) %>%
    mutate(
      target_quad = case_when(
        mean_cvoi >= med_cvoi & mean_red >= med_red ~ "Highest",
        mean_cvoi >= med_cvoi & mean_red <  med_red ~ "High",
        mean_cvoi <  med_cvoi & mean_red >= med_red ~ "Medium",
        TRUE ~ "Low"
      ),
      in_target = case_when(
        rep_cvoi >= med_cvoi & rep_red >= med_red & target_quad == "Highest" ~ 1,
        rep_cvoi >= med_cvoi & rep_red <  med_red & target_quad == "High"    ~ 1,
        rep_cvoi <  med_cvoi & rep_red >= med_red & target_quad == "Medium"  ~ 1,
        rep_cvoi <  med_cvoi & rep_red <  med_red & target_quad == "Low"     ~ 1,
        TRUE ~ 0
      )
    ) %>%
    group_by(across(all_of(group_vars))) %>%
    summarize(
      robustness_score = (sum(in_target, na.rm = TRUE) / n_reps) * 100,
      sensitivity_cat = case_when(
        robustness_score > 90 ~ "Robust",
        robustness_score > 75 ~ "Moderately Sensitive",
        TRUE ~ "Highly Sensitive"
      ),
      .groups = "drop"
    )
  
  ## combine and return
  left_join(summary_stats, sensitivity, by = group_vars)
}

## bootstrap by species
boot_by_species_linear <- run_cvoi_boot_linear(data_prepped, 
                                          group_vars = c("species", "hypothesis", "code", "name", "iucn_name"), 
                                          mode = "by_species")

## bootstrap across species by hypothesis
boot_global_hypo_linear <- run_cvoi_boot_linear(data_prepped, 
                                           group_vars = c("hypothesis", "code", "name", "iucn_name"), 
                                           mode = "global_hypo")

## bootstrap across species by IUCN threat category
boot_global_iucn_linear <- run_cvoi_boot_linear(data_prepped, 
                                           group_vars = "iucn_name", 
                                           mode = "global_iucn")

## save results
write.csv(boot_by_species_linear,  here("results", "linear_boot_by_species.csv"), row.names = FALSE)
write.csv(boot_global_hypo_linear, here("results", "linear_boot_global_hypo.csv"), row.names = FALSE)
write.csv(boot_global_iucn_linear, here("results", "linear_boot_global_iucn.csv"), row.names = FALSE)