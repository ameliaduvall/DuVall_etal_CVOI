## Bootstrap species average - linear confidence

## load libraries
library(tidyverse)
library(here)

## set seed
set.seed(16)

## load data
assp <- readRDS(here("results", "assp_raw_scores.RDS")) %>%
  mutate(sample_code = paste(hypothesis, expert, sep = "-"),
         conf_weight = ifelse(conf == 2, 1, ifelse(conf == 1, 0.5, 0.01)))
brpe <- readRDS(here("results", "brpe_raw_scores.RDS")) %>%
  mutate(sample_code = paste(hypothesis, expert, sep = "-"),
         conf_weight = ifelse(conf == 2, 1, ifelse(conf == 1, 0.5, 0.01)))
caau <- readRDS(here("results", "caau_raw_scores.RDS")) %>%
  mutate(sample_code = paste(hypothesis, expert, sep = "-"),
         conf_weight = ifelse(conf == 2, 1, ifelse(conf == 1, 0.5, 0.01)))
scmu <- readRDS(here("results", "scmu_raw_scores.RDS")) %>%
  mutate(sample_code = paste(hypothesis, expert, sep = "-"),
         conf_weight = ifelse(conf == 2, 1, ifelse(conf == 1, 0.5, 0.01)))
snpl <- readRDS(here("results", "snpl_raw_scores.RDS")) %>%
  mutate(sample_code = paste(hypothesis, expert, sep = "-"),
         conf_weight = ifelse(conf == 2, 1, ifelse(conf == 1, 0.5, 0.01)))
wegu <- readRDS(here("results", "wegu_raw_scores.RDS")) %>%
  mutate(sample_code = paste(hypothesis, expert, sep = "-"),
         conf_weight = ifelse(conf == 2, 1, ifelse(conf == 1, 0.5, 0.01)))

## create list
birds <- list(assp, brpe, caau, scmu, snpl, wegu)
names(birds) <- c('assp', 'brpe', 'caau', 'scmu', 'snpl', 'wegu')
nbirds <- length(birds)

## set parameters
S <- 1000

## get hypothesis info
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
    
    for (b in 1:nbirds) {
      
      sp_df <- birds[[b]]
      
      # filter for hypothesis
      hypo_data <- sp_df %>% filter(hypothesis == hypos[h])
      
      ## cvoi sampling
      n_experts_sp <- length(unique(hypo_data$expert))
      
      # sample indices with replacement
      sampled_indices <- sample(1:nrow(hypo_data), n_experts_sp, replace = TRUE)
      
      # extract values
      sampled_cvoi <- hypo_data$cvoi[sampled_indices]
      sampled_conf <- hypo_data$conf_weight[sampled_indices]
      
      # weighted mean for this spp
      species_means_cvoi[b] <- sum(sampled_cvoi * sampled_conf) / sum(sampled_conf)
      
      ## reducibility sampling
      hypo_red_data <- sp_df %>% filter(hypothesis == hypos[h], !is.na(red))
      n_red_experts <- nrow(hypo_red_data)
      
      if (n_red_experts > 0) {
        sampled_red_indices <- sample(1:nrow(hypo_red_data), n_red_experts, replace = TRUE)
        
        sampled_red <- hypo_red_data$red[sampled_red_indices]
        sampled_red_conf <- hypo_red_data$conf_weight[sampled_red_indices]
        
        # weighted mean for this spp
        species_means_red[b] <- sum(sampled_red * sampled_red_conf) / sum(sampled_red_conf)
        
      } else {
        species_means_red[b] <- NA 
      }
    } # close spp loop
    
    # mean across spp
    mu_cvoi_global[s, h] <- mean(species_means_cvoi, na.rm = TRUE)
    mu_red_global[s, h]  <- mean(species_means_red, na.rm = TRUE)
    
  } # end bootstrap loop
  
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

saveRDS(df, here("results", "all_spp_avg_bootstrapped_linear_conf.RDS"))

## sensitivity
for (i in 1:length(hypos)) {
  
  if (all(is.na(mu_red_global[,i]))) next
  
  e2e_sensitivity[i,1] <- mean(mu_cvoi_global[,i] >= median(df$mean_cvoi, na.rm=T) & mu_red_global[,i] >= median(df$mean_red, na.rm=T), na.rm=T)
  e2e_sensitivity[i,2] <- mean(mu_cvoi_global[,i] >= median(df$mean_cvoi, na.rm=T) & mu_red_global[,i] <  median(df$mean_red, na.rm=T), na.rm=T)
  e2e_sensitivity[i,3] <- mean(mu_cvoi_global[,i] <  median(df$mean_cvoi, na.rm=T) & mu_red_global[,i] >= median(df$mean_red, na.rm=T), na.rm=T)
  e2e_sensitivity[i,4] <- mean(mu_cvoi_global[,i] <  median(df$mean_cvoi, na.rm=T) & mu_red_global[,i] <  median(df$mean_red, na.rm=T), na.rm=T)
}

saveRDS(e2e_sensitivity, here("results", "all_spp_avg_linear_conf_sensitivity.RDS"))
