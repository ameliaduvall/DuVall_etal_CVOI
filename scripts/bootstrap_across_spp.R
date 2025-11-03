## Bootstrap across all species 

## load libraries
library(tidyverse)
library(here)

## set seed
set.seed(16)

## load data (each species)
assp <- readRDS(here("results", "assp_raw_scores.RDS")) %>%
  mutate(species = "assp", sample_code = paste(hypothesis, expert, sep = "-"))
brpe <- readRDS(here("results", "brpe_raw_scores.RDS")) %>%
  mutate(species = "brpe", sample_code = paste(hypothesis, expert, sep = "-"))
caau <- readRDS(here("results", "caau_raw_scores.RDS")) %>%
  mutate(species = "caau", sample_code = paste(hypothesis, expert, sep = "-"))
scmu <- readRDS(here("results", "scmu_raw_scores.RDS")) %>%
  mutate(species = "scmu", sample_code = paste(hypothesis, expert, sep = "-"))
snpl <- readRDS(here("results", "snpl_raw_scores.RDS")) %>%
  mutate(species = "snpl", sample_code = paste(hypothesis, expert, sep = "-"))
wegu <- readRDS(here("results", "wegu_raw_scores.RDS")) %>%
  mutate(species = "wegu", sample_code = paste(hypothesis, expert, sep = "-"))

## combine all data
birds <- list(assp, brpe, caau, scmu, snpl, wegu)
names(birds) <- c('assp', 'brpe', 'caau', 'scmu', 'snpl', 'wegu')
nbirds <- length(birds)
all_birds <- bind_rows(birds)

## set parameters
S <- 1000
n_ex_total <- 12 # how to handle this? (11 for caau)
#n_ex_total <- max(all_birds %>% count(species, expert) %>% count(species) %>% pull(n))

## set equal species weights
species_list <- names(birds)
species_weights <- rep(1 / nbirds, nbirds) # could change this

## get hypothesis info
hypos <- unique(all_birds$hypothesis)
names_hypos <- all_birds %>%
  distinct(hypothesis, name) %>%
  arrange(hypothesis) %>%
  pull(name)

## initialize storage
expert_cvoi <- expert_red <- array(dim = c(S, n_ex_total, length(hypos)))
temp_cvoi <- temp_red <- array(dim = c(S, n_ex_total, length(hypos)))
X_cvoi <- X_red <- array(dim = c(S, n_ex_total, length(hypos)))
mu_cvoi <- mu_red <- matrix(nrow = S, ncol = length(hypos))
CVOI_bar <- Red_bar <- var_cvoi <- var_red <- numeric(length(hypos))
e2e_sensitivity <- matrix(nrow = length(hypos), ncol = 4)

## ================================
## MAIN BOOTSTRAP LOOP
## ================================
for (h in seq_along(hypos)) {
  
  ## skip if hypothesis missing everywhere (should not happen)
  hypo_data <- all_birds %>% filter(hypothesis == hypos[h])
  if (nrow(hypo_data) == 0) next
  
  for (s in 1:S) {
    
    ## sample species equally
    sampled_species <- sample(species_list, n_ex_total, replace = TRUE, prob = species_weights)
    
    ## sample one expert per selected species for this hypothesis
    expert_cvoi[s,,h] <- map_chr(sampled_species, function(sp) {
      sp_data <- hypo_data %>% filter(species == sp)
      if (nrow(sp_data) == 0) return(NA_character_)
      sample(sp_data$expert, 1, replace = TRUE)
    })
    
    ## combine hypothesis and expert
    temp_cvoi[s,,h] <- paste(hypos[h], expert_cvoi[s,,h], sep = "-")
    
    ## get corresponding cvoi scores
    X_cvoi[s,,h] <- map_dbl(temp_cvoi[s,,h], function(code) {
      all_birds$cvoi[which(all_birds$sample_code == code)] %>% first()
    })
    
    ## mean cvoi for this sample
    mu_cvoi[s,h] <- mean(X_cvoi[s,,h], na.rm = TRUE)
  }
  
  ## average and variance across samples
  CVOI_bar[h] <- mean(mu_cvoi[,h], na.rm = TRUE)
  var_cvoi[h] <- var(mu_cvoi[,h], na.rm = TRUE)
}

## ================================
## REDUCIBILITY BOOTSTRAP
## ================================
red_df <- all_birds %>% filter(!is.na(red))

for (h in seq_along(hypos)) {
  
  hypo_data <- red_df %>% filter(hypothesis == hypos[h])
  if (nrow(hypo_data) == 0) next
  
  for (s in 1:S) {
    
    ## sample species equally
    sampled_species <- sample(species_list, n_ex_total, replace = TRUE, prob = species_weights)
    
    expert_red[s,,h] <- map_chr(sampled_species, function(sp) {
      sp_data <- hypo_data %>% filter(species == sp)
      if (nrow(sp_data) == 0) return(NA_character_)
      sample(sp_data$expert, 1, replace = TRUE)
    })
    
    ## combine hypothesis and expert
    temp_red[s,,h] <- paste(hypos[h], expert_red[s,,h], sep = "-")
    
    ## extract reducibility scores
    X_red[s,,h] <- map_dbl(temp_red[s,,h], function(code) {
      red_df$red[which(red_df$sample_code == code)] %>% first()
    })
    
    mu_red[s,h] <- mean(X_red[s,,h], na.rm = TRUE)
  }
  
  Red_bar[h] <- mean(mu_red[,h], na.rm = TRUE)
  var_red[h] <- var(mu_red[,h], na.rm = TRUE)
}

## ================================
## MAKE FINAL DATAFRAME
## ================================
df <- tibble(
  hypothesis = hypos,
  name = names_hypos,
  mean_cvoi = CVOI_bar,
  var_cvoi = var_cvoi,
  mean_red = Red_bar,
  var_red = var_red
)

saveRDS(df, here("results", "combined_bootstrapped_equal_species.RDS"))

## ================================
## SENSITIVITY TABLE
## ================================
for (i in seq_along(hypos)) {
  if (all(is.na(mu_cvoi[,i])) | all(is.na(mu_red[,i]))) next
  
  e2e_sensitivity[i,1] <- mean(mu_cvoi[,i] >= median(df$mean_cvoi, na.rm = TRUE) &
                                 mu_red[,i] >= median(df$mean_red, na.rm = TRUE), na.rm = TRUE)
  e2e_sensitivity[i,2] <- mean(mu_cvoi[,i] >= median(df$mean_cvoi, na.rm = TRUE) &
                                 mu_red[,i] <  median(df$mean_red, na.rm = TRUE), na.rm = TRUE)
  e2e_sensitivity[i,3] <- mean(mu_cvoi[,i] <  median(df$mean_cvoi, na.rm = TRUE) &
                                 mu_red[,i] >= median(df$mean_red, na.rm = TRUE), na.rm = TRUE)
  e2e_sensitivity[i,4] <- mean(mu_cvoi[,i] <  median(df$mean_cvoi, na.rm = TRUE) &
                                 mu_red[,i] <  median(df$mean_red, na.rm = TRUE), na.rm = TRUE)
}

colnames(e2e_sensitivity) <- c("High_CVOI_High_RED", "High_CVOI_Low_RED",
                               "Low_CVOI_High_RED", "Low_CVOI_Low_RED")

saveRDS(e2e_sensitivity, here("results", "combined_sensitivity_equal_species.RDS"))
