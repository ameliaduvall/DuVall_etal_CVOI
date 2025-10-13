## Bootstrapping confidence
## Linear weight

## load libraries
library(tidyverse)
library(here)

## set seed
set.seed(16)

## load data
assp <- readRDS(here("data", "assp_round2.RDS")) %>%
  mutate(sample_code = paste(hypothesis, expert, sep = "-"),
         conf_weight = ifelse(conf == 2, 1, ifelse(conf == 1, 0.5, 0.01)))
brpe <- readRDS(here("data", "brpe_round2.RDS")) %>%
  mutate(sample_code = paste(hypothesis, expert, sep = "-"),
         conf_weight = ifelse(conf == 2, 1, ifelse(conf == 1, 0.5, 0.01)))
caau <- readRDS(here("data", "caau_round2.RDS")) %>%
  mutate(sample_code = paste(hypothesis, expert, sep = "-"),
         conf_weight = ifelse(conf == 2, 1, ifelse(conf == 1, 0.5, 0.01)))
scmu <- readRDS(here("data", "scmu_round2.RDS")) %>%
  mutate(sample_code = paste(hypothesis, expert, sep = "-"),
         conf_weight = ifelse(conf == 2, 1, ifelse(conf == 1, 0.5, 0.01)))
snpl <- readRDS(here("data", "snpl_round2.RDS")) %>%
  mutate(sample_code = paste(hypothesis, expert, sep = "-"),
         conf_weight = ifelse(conf == 2, 1, ifelse(conf == 1, 0.5, 0.01)))
wegu <- readRDS(here("data", "wegu_round2.RDS")) %>%
  mutate(sample_code = paste(hypothesis, expert, sep = "-"),
         conf_weight = ifelse(conf == 2, 1, ifelse(conf == 1, 0.5, 0.01)))

## set parameters
S <- 1000
birds <- list(assp, brpe, caau, scmu, snpl, wegu)
names(birds) <- c('assp', 'brpe', 'caau', 'scmu', 'snpl', 'wegu')
nbirds <- length(birds)

for (b in 1:nbirds) {
  
  n_ex_rel <- birds[[b]] %>%
    select(hypothesis, code, name, expert, rel_a, rel_b, mag, red) %>%
    pivot_longer(5:8, names_to = "var", values_to = "val") %>%
    group_by(hypothesis, code, name, var) %>%
    filter(var == "rel_a" ) %>%
    mutate(expert_count = ifelse(val == 0, 0, 1)) %>%
    group_by(hypothesis, code, name) %>%
    summarize(n_ex = sum(expert_count))
  n_ex <- as.vector(n_ex_rel$n_ex) # number of experts who thought hypothesis was relevant
  n_ex_total <- 12
  hypos <- unique(n_ex_rel$hypothesis) # vector of hypothesis number
  name <- unique(n_ex_rel$name) # vector of hypothesis name
  expert_cvoi <- expert_red <- array(dim = c(S, n_ex_total, length(hypos))) # store sampled expert numbers
  temp_cvoi <- temp_red <- array(dim = c(S, n_ex_total, length(hypos))) # store temp code that links hypotheses with experts
  X_cvoi <- X_red <- X_conf <- X_conf_red <- array(dim = c(S, n_ex_total, length(hypos))) # store score for sampled experts
  mu_cvoi <- mu_red <- sum_wgts <- sum_wgts_red <- matrix(nrow=S, ncol=length(hypos)) # matrix of avg score for each sample and hypothesis
  CVOI_bar<-Red_bar<-length(hypos) # vector of mean cvoi & reducibility for each hypothesis
  var_cvoi<-var_red<-length(hypos) # vector of cvoi & reducibility variance for each hypothesis
  e2e_sensitivity <- matrix(nrow = (length(hypos)), ncol = 4)
  
  for (h in 1:length(hypos)){
    for (s in 1:S){
      # randomly sample 12 expert numbers with replacement
      expert_cvoi[s,,h]<- (sample((birds[[b]]$expert[which(birds[[b]]$hypothesis==hypos[h])]), n_ex_total, replace=T))
      
      for (t in 1:dim(temp_cvoi)[2]) {
        # store sampled experts
        temp_cvoi[s,,h] <- paste(h,expert_cvoi[s,,h], sep = "-")
        # extract cvoi scores for sampled experts
        X_cvoi[s,t,h] <- birds[[b]]$cvoi[which(birds[[b]]$sample_code == temp_cvoi[s,t,h])]
        # extract conf scores for sampled experts
        X_conf[s,t,h] <- birds[[b]]$conf_weight[which(birds[[b]]$sample_code == temp_cvoi[s,t,h])]
        # sum conf weights across each sample
        sum_wgts[s,h] <- sum(X_conf[s,,h]) # denominator for weighted average
        # weighted average cvoi scores for sample and hypothesis
        mu_cvoi[s,h]<- sum((X_cvoi[s,,h] * X_conf[s,,h]))/(sum_wgts[s,h])
        
      }
    }
    # calculate average CVOI for each hypothesis
    CVOI_bar[h] <- sum(mu_cvoi[,h])/S
    
    # calculate variance for each hypothesis
    var_cvoi[h] <- (1/(S-1))*sum((mu_cvoi[,h]-CVOI_bar[h])^2)
  }
  
  red_df <- birds[[b]] %>%
    filter(!is.na(red))
  
  for (h in 1:length(hypos)){
    for (s in 1:S){
      
      #if (n_ex[h] != 0)  {
      if (n_ex[h] == 0) next 
      
      # randomly sample 12 expert numbers with replacement
      expert_red[s,1:n_ex[h],h] <- (sample((red_df$expert[which(red_df$hypothesis==hypos[h])]), n_ex[h], replace=T))
      
      for (t in 1:dim(temp_red)[2]) {
        #for (t in 1:n_ex[h]) {
        # store sampled experts
        temp_red[s,1:n_ex[h],h] <- paste(h,expert_red[s,1:n_ex[h],h], sep = "-")
        # extract reducibility scores for sampled experts
        if (is.na(temp_red[s,t,h])) next
        X_red[s,t,h] <- red_df$red[which(red_df$sample_code == temp_red[s,t,h])]
        # extract conf scores for sampled experts
        X_conf_red[s,t,h] <- red_df$conf_weight[which(red_df$sample_code == temp_red[s,t,h])]
        # sum conf weights across each sample
        sum_wgts_red[s,h] <- sum(X_conf_red[s,1:n_ex[h],h])#, na.rm = T) # denominator for weighted average
        # weighted average cvoi scores for sample and hypothesis
        mu_red[s,h]<- (sum(X_red[s,1:n_ex[h],h] * X_conf_red[s,1:n_ex[h],h])) /(sum_wgts_red[s,h]) #[s,1:n_ex[h],h]
        
      }
    }
    
    # calculate average red for each hypothesis
    Red_bar[h] <- sum(mu_red[,h])/S#, na.rm = T)/S # add na.rm = T?
    
    # calculate variance for each hypothesis
    var_red[h] <- (1/(S-1))*sum((mu_red[,h]-Red_bar[h])^2)
    
  }
  
  ## make dataframe
  df <- bind_cols(hypos, name, CVOI_bar, var_cvoi, Red_bar, var_red)
  colnames(df) <- c("hypothesis", "name", "mean_cvoi", "var_cvoi", "mean_red", "var_red")
  final_df <- df %>%
    mutate(species = c(paste(names(birds)[b]))) %>%
    dplyr::select(species, everything())
  
  saveRDS(final_df, here("results", paste(names(birds)[b], "bootstrapped_linear_conf.RDS", sep = "_")))
  #return(final_df)
  
  for (i in 1:length(hypos)) {
    if (n_ex[i] == 0) next 
    e2e_sensitivity[i,1]<-length(which(mu_cvoi[,i]>=median(df$mean_cvoi) & mu_red[,i]>=median(df$mean_red, na.rm = T)))/1000
    e2e_sensitivity[i,2]<-length(which(mu_cvoi[,i]>=median(df$mean_cvoi) & mu_red[,i]<median(df$mean_red, na.rm = T)))/1000
    e2e_sensitivity[i,3]<-length(which(mu_cvoi[,i]<median(df$mean_cvoi) & mu_red[,i]>=median(df$mean_red, na.rm = T)))/1000
    e2e_sensitivity[i,4]<-length(which(mu_cvoi[,i]<median(df$mean_cvoi) & mu_red[,i]<median(df$mean_red, na.rm = T)))/1000
  }
  
  saveRDS(e2e_sensitivity, here("results", paste(names(birds)[b], "linear_conf_sensitivity.RDS", sep = "_")))
  #return(e2e_sensitivity)
}
