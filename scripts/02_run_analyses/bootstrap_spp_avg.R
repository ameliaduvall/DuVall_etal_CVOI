## Bootstrap species average

## load libraries
library(tidyverse)
library(here)

## set seed
set.seed(16)

## load data
assp <- readRDS(here("results", "assp_raw_scores.RDS")) %>%
  mutate(sample_code = paste("assp", hypothesis, expert, sep = "-"))
brpe <- readRDS(here("results", "brpe_raw_scores.RDS")) %>%
  mutate(sample_code = paste("brpe", hypothesis, expert, sep = "-"))
caau <- readRDS(here("results", "caau_raw_scores.RDS")) %>%
  mutate(sample_code = paste("caau", hypothesis, expert, sep = "-"))
scmu <- readRDS(here("results", "scmu_raw_scores.RDS")) %>%
  mutate(sample_code = paste("scmu", hypothesis, expert, sep = "-"))
snpl <- readRDS(here("results", "snpl_raw_scores.RDS")) %>%
  mutate(sample_code = paste("snpl", hypothesis, expert, sep = "-"))
wegu <- readRDS(here("results", "wegu_raw_scores.RDS")) %>%
  mutate(sample_code = paste("wegu", hypothesis, expert, sep = "-"))

## create list
birds <- list(assp, brpe, caau, scmu, snpl, wegu)
names(birds) <- c('assp', 'brpe', 'caau', 'scmu', 'snpl', 'wegu')
nbirds <- length(birds)

## set parameters
S <- 1000

## get hypothesis info (all species have same hypotheses)
hypos <- unique(assp$hypothesis) 
name <- assp %>% distinct(hypothesis, name) %>% arrange(hypothesis) %>% pull(name)

## initialize storage
mu_cvoi_global <- matrix(nrow = S, ncol = length(hypos)) 
mu_red_global  <- matrix(nrow = S, ncol = length(hypos)) 

CVOI_bar <- numeric(length(hypos))
var_cvoi <- numeric(length(hypos))
Red_bar  <- numeric(length(hypos))
var_red  <- numeric(length(hypos))
e2e_sensitivity <- matrix(nrow = length(hypos), ncol = 4)

## bootstrap loop
for (h in 1:length(hypos)) {
  
  for (s in 1:S) {
    
    # temp vectors to hold mean score 
    species_means_cvoi <- numeric(nbirds)
    species_means_red  <- numeric(nbirds)
    
    # loop through each spp individually
    for (b in 1:nbirds) {
      
      sp_df <- birds[[b]]
      
      # filter for hypothesis
      hypo_data <- sp_df %>% filter(hypothesis == hypos[h])

      ## cvoi sampling  
      # number of experts for this specific spp
      n_experts_sp <- length(unique(hypo_data$expert))
      
      # sample experts with replacement
      sampled_indices <- sample(1:nrow(hypo_data), n_experts_sp, replace = TRUE)
      
      # calculate mean CVOI for this spp
      species_means_cvoi[b] <- mean(hypo_data$cvoi[sampled_indices])
      
      ## reducibility sampling
      hypo_red_data <- sp_df %>% filter(hypothesis == hypos[h], !is.na(red))
      n_red_experts <- nrow(hypo_red_data)
      
      if (n_red_experts > 0) {
        sampled_red_indices <- sample(1:nrow(hypo_red_data), n_red_experts, replace = TRUE)
        species_means_red[b] <- mean(hypo_red_data$red[sampled_red_indices])
      } else {
        species_means_red[b] <- NA # where a spp has no reducibility data
      }
    } # close spp loop
    
    
    # mean across spp
    mu_cvoi_global[s, h] <- mean(species_means_cvoi, na.rm = TRUE)
    mu_red_global[s, h]  <- mean(species_means_red, na.rm = TRUE)
    
  } # close bootstrap loop
  
  # summarize over bootstrap sets
  CVOI_bar[h] <- mean(mu_cvoi_global[,h], na.rm = TRUE)
  var_cvoi[h] <- var(mu_cvoi_global[,h], na.rm = TRUE)
  
  Red_bar[h]  <- mean(mu_red_global[,h], na.rm = TRUE)
  var_red[h]  <- var(mu_red_global[,h], na.rm = TRUE)
}

## make results df
df <- tibble(hypothesis = hypos,
  name = name,
  mean_cvoi = CVOI_bar,
  var_cvoi = var_cvoi,
  mean_red = Red_bar,
  var_red = var_red) 

saveRDS(df, here("results", "all_spp_avg_bootstrapped.RDS"))

## sensitivity
for (i in 1:length(hypos)) {
  
  if (all(is.na(mu_red_global[,i]))) next
  
  e2e_sensitivity[i,1] <- mean(mu_cvoi_global[,i] >= median(df$mean_cvoi, na.rm=T) & mu_red_global[,i] >= median(df$mean_red, na.rm=T), na.rm=T)
  e2e_sensitivity[i,2] <- mean(mu_cvoi_global[,i] >= median(df$mean_cvoi, na.rm=T) & mu_red_global[,i] <  median(df$mean_red, na.rm=T), na.rm=T)
  e2e_sensitivity[i,3] <- mean(mu_cvoi_global[,i] <  median(df$mean_cvoi, na.rm=T) & mu_red_global[,i] >= median(df$mean_red, na.rm=T), na.rm=T)
  e2e_sensitivity[i,4] <- mean(mu_cvoi_global[,i] <  median(df$mean_cvoi, na.rm=T) & mu_red_global[,i] <  median(df$mean_red, na.rm=T), na.rm=T)
}

saveRDS(e2e_sensitivity, here("results", "all_spp_avg_sensitivity.RDS"))