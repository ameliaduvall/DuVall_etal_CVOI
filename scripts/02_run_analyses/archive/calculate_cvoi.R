## Calculate CVOI

## load libraries
library(here)
library(tidyverse)
library(janitor)

## create round 2 df 
source(here("scripts", "01_format_data","load_round_2.R"))

## Calculate scores by species

# Ashy Storm-Petrel (ASSP) ------------------------------------
assp <- full_df %>% 
  filter(species == "ASSP") %>%
  ## if rel_a is 0, make mag and rel_b a 0
  mutate(rel_b = ifelse(rel_a ==0, 0, rel_b),
         mag = ifelse(rel_a ==0, 0, mag))%>%
  arrange(hypothesis) %>%
  mutate(rel_both = ((rel_a*rel_b)-0/(4-0)),
         cvoi = (mag*rel_both),
         ## if experts scored a 0 for rel_a, they did not score for red
         red = ifelse((rel_a == 0 & rel_b == 0 & mag == 0), NA, red)) %>%
  select(expert, category, hypothesis, code, name, species, rel_a, rel_b, rel_both, mag, red, cvoi, conf)
# saveRDS(assp, here("results", "assp_raw_scores.rds"))

## calculate number of experts who thought hypothesis was relevant
n_ex_rel <- assp %>%
  filter(rel_a != 0) %>% 
  select(hypothesis, code, name, expert, species, rel_a, rel_b, mag, red) %>%
  pivot_longer(6:9, names_to = "var", values_to = "val") %>%
  group_by(hypothesis, code, name, species, var) %>%
  filter(var == "rel_a" ) %>%
  summarize(n_ex = n()) %>%
  distinct(hypothesis, .keep_all = T)
n_ex <- as.vector(n_ex_rel$n_ex) 
n_ex_total <- 12

hypothesis <- unique(n_ex_rel$hypothesis) # drop hypotheses that no one thought was relevant

assp_r2_means<- assp %>%
  filter(hypothesis %in% n_ex_rel$hypothesis) %>%
  distinct(hypothesis, .keep_all = TRUE) %>%
  arrange(hypothesis) %>%
  clean_names() %>%
  dplyr::select(species, category, hypothesis, code, name, expert, rel_a, rel_b, rel_both, mag, red, cvoi, conf)

assp_r2_means$expert <- ""
assp_r2_means$rel_a <- ""
assp_r2_means$rel_b <- ""
assp_r2_means$rel_both <- ""
assp_r2_means$mag <- ""
assp_r2_means$red <- ""
assp_r2_means$cvoi <- ""
assp_r2_means$conf <- ""

## create empty vectors
cvoi_lci <- cvoi_uci <- red_lci <- red_uci <- rel_both_lci <- rel_both_uci <- mag_lci <- mag_uci <- numeric(length(hypothesis))

for(i in 1:length(hypothesis)){
  assp_r2_means$mag[i]<-sum(assp$mag[which(assp$hypothesis==hypothesis[i])])/n_ex_total
  assp_r2_means$rel_a[i]<-sum(assp$rel_a[which(assp$hypothesis==hypothesis[i])])/n_ex_total
  assp_r2_means$rel_b[i]<-sum(assp$rel_b[which(assp$hypothesis==hypothesis[i])])/n_ex_total
  assp_r2_means$red[i]<-sum(assp$red[which(assp$hypothesis==hypothesis[i])], na.rm = T)/n_ex[i]
  assp_r2_means$rel_both[i]<-sum(assp$rel_both[which(assp$hypothesis==hypothesis[i])])/n_ex_total
  assp_r2_means$cvoi[i]<-sum(assp$cvoi[which(assp$hypothesis==hypothesis[i])])/n_ex_total
  assp_r2_means$conf[i]<-sum(assp$conf[which(assp$hypothesis==hypothesis[i])])/n_ex_total
  cvoi_lci[i]<-as.numeric(quantile((assp$cvoi[which(assp$hypothesis==hypothesis[i])]), 0.05))
  cvoi_uci[i]<-as.numeric(quantile((assp$cvoi[which(assp$hypothesis==hypothesis[i])]), 0.95))
  red_lci[i]<-as.numeric(quantile((assp$red[which(assp$hypothesis==hypothesis[i])]), 0.05, na.rm = T))
  red_uci[i]<-as.numeric(quantile((assp$red[which(assp$hypothesis==hypothesis[i])]), 0.95, na.rm = T))
  rel_both_lci[i]<-as.numeric(quantile((assp$rel_both[which(assp$hypothesis==hypothesis[i])]), 0.05, na.rm = T))
  rel_both_uci[i]<-as.numeric(quantile((assp$rel_both[which(assp$hypothesis==hypothesis[i])]), 0.95, na.rm = T))
  mag_lci[i]<-as.numeric(quantile((assp$mag[which(assp$hypothesis==hypothesis[i])]), 0.05, na.rm = T))
  mag_uci[i]<-as.numeric(quantile((assp$mag[which(assp$hypothesis==hypothesis[i])]), 0.95, na.rm = T))
}

assp_summary <- data.frame(Hypothesis = unique(n_ex_rel$hypothesis),
                              Name = unique(n_ex_rel$name), 
                              CVOI_mean = as.numeric(assp_r2_means$cvoi),
                              CVOI_lci = cvoi_lci,
                              CVOI_uci = cvoi_uci,
                              Red_mean = as.numeric(assp_r2_means$red),
                              Red_lci = red_lci,
                              Red_uci = red_uci,
                              Rel_a_mean = as.numeric(assp_r2_means$rel_a),
                              Rel_b_mean = as.numeric(assp_r2_means$rel_b),
                              Rel_both_mean = as.numeric(assp_r2_means$rel_both),
                              Rel_both_lci = rel_both_lci,
                              Rel_both_uci = rel_both_uci, 
                              Mag_mean = as.numeric(assp_r2_means$mag),
                              Mag_lci = mag_lci, 
                              Mag_uci = mag_uci,
                              Num_experts = n_ex_rel$n_ex)

#write.csv(assp_summary, here("results", "assp_summary.csv"))

# California Brown Pelican (BRPE) ------------------------------------
brpe <- full_df %>% 
  filter(species == "BRPE") %>%
  mutate(rel_b = ifelse(rel_a ==0, 0, rel_b),
         mag = ifelse(rel_a ==0, 0, mag))%>%
  arrange(hypothesis) %>%
  mutate(rel_both = ((rel_a*rel_b)-0/(4-0)),
         cvoi = (mag*rel_both),
         red = ifelse((rel_a == 0 & rel_b == 0 & mag == 0), NA, red)) %>%
  select(expert, category, hypothesis, code, name, species, rel_a, rel_b, rel_both, mag, red, cvoi, conf)
# saveRDS(brpe, here("results", "brpe_raw_scores.rds"))

## calculate number of experts who thought hypothesis was relevant
n_ex_rel <- brpe %>%
  filter(rel_a != 0) %>% 
  select(hypothesis, code, name, expert, species, rel_a, rel_b, mag, red) %>%
  pivot_longer(6:9, names_to = "var", values_to = "val") %>%
  group_by(hypothesis, code, name, species, var) %>%
  filter(var == "rel_a" ) %>%
  summarize(n_ex = n()) %>%
  distinct(hypothesis, .keep_all = T)
n_ex <- as.vector(n_ex_rel$n_ex) 
n_ex_total <- 12

hypothesis<-unique(n_ex_rel$hypothesis) # drop hypotheses that no one thought was relevant

brpe_r2_means<- brpe %>%
  filter(hypothesis %in% n_ex_rel$hypothesis) %>%
  distinct(hypothesis, .keep_all = TRUE) %>%
  arrange(hypothesis) %>%
  clean_names() %>%
  dplyr::select(species, category, hypothesis, code, name, expert, rel_a, rel_b, rel_both, mag, red, cvoi, conf)

brpe_r2_means$expert <- ""
brpe_r2_means$rel_a <- ""
brpe_r2_means$rel_b <- ""
brpe_r2_means$rel_both <- ""
brpe_r2_means$mag <- ""
brpe_r2_means$red <- ""
brpe_r2_means$cvoi <- ""
brpe_r2_means$conf <- ""

## create empty vectors
cvoi_lci <- cvoi_uci <- red_lci <- red_uci <- rel_both_lci <- rel_both_uci <- mag_lci <- mag_uci <- numeric(length(hypothesis))

for(i in 1:length(hypothesis)){
  brpe_r2_means$mag[i]<-sum(brpe$mag[which(brpe$hypothesis==hypothesis[i])])/n_ex_total
  brpe_r2_means$rel_a[i]<-sum(brpe$rel_a[which(brpe$hypothesis==hypothesis[i])])/n_ex_total
  brpe_r2_means$rel_b[i]<-sum(brpe$rel_b[which(brpe$hypothesis==hypothesis[i])])/n_ex_total
  brpe_r2_means$red[i]<-sum(brpe$red[which(brpe$hypothesis==hypothesis[i])], na.rm = T)/n_ex[i]
  brpe_r2_means$rel_both[i]<-sum(brpe$rel_both[which(brpe$hypothesis==hypothesis[i])])/n_ex_total
  brpe_r2_means$cvoi[i]<-sum(brpe$cvoi[which(brpe$hypothesis==hypothesis[i])])/n_ex_total
  brpe_r2_means$conf[i]<-sum(brpe$conf[which(brpe$hypothesis==hypothesis[i])])/n_ex_total
  cvoi_lci[i]<-as.numeric(quantile((brpe$cvoi[which(brpe$hypothesis==hypothesis[i])]), 0.05))
  cvoi_uci[i]<-as.numeric(quantile((brpe$cvoi[which(brpe$hypothesis==hypothesis[i])]), 0.95))
  red_lci[i]<-as.numeric(quantile((brpe$red[which(brpe$hypothesis==hypothesis[i])]), 0.05, na.rm = T))
  red_uci[i]<-as.numeric(quantile((brpe$red[which(brpe$hypothesis==hypothesis[i])]), 0.95, na.rm = T))
  rel_both_lci[i]<-as.numeric(quantile((brpe$rel_both[which(brpe$hypothesis==hypothesis[i])]), 0.05, na.rm = T))
  rel_both_uci[i]<-as.numeric(quantile((brpe$rel_both[which(brpe$hypothesis==hypothesis[i])]), 0.95, na.rm = T))
  mag_lci[i]<-as.numeric(quantile((brpe$mag[which(brpe$hypothesis==hypothesis[i])]), 0.05, na.rm = T))
  mag_uci[i]<-as.numeric(quantile((brpe$mag[which(brpe$hypothesis==hypothesis[i])]), 0.95, na.rm = T))
}

brpe_summary <- data.frame(Hypothesis = unique(n_ex_rel$hypothesis),
                           Name = unique(n_ex_rel$name), 
                           CVOI_mean = as.numeric(brpe_r2_means$cvoi),
                           CVOI_lci = cvoi_lci,
                           CVOI_uci = cvoi_uci,
                           Red_mean = as.numeric(brpe_r2_means$red),
                           Red_lci = red_lci,
                           Red_uci = red_uci,
                           Rel_a_mean = as.numeric(brpe_r2_means$rel_a),
                           Rel_b_mean = as.numeric(brpe_r2_means$rel_b),
                           Rel_both_mean = as.numeric(brpe_r2_means$rel_both),
                           Rel_both_lci = rel_both_lci,
                           Rel_both_uci = rel_both_uci, 
                           Mag_mean = as.numeric(brpe_r2_means$mag),
                           Mag_lci = mag_lci, 
                           Mag_uci = mag_uci,
                           Num_experts = n_ex_rel$n_ex)

#write.csv(brpe_summary, here("results", "brpe_summary.csv"))

# Cassin's Auklet (CAAU) ------------------------------------
caau <- full_df %>% 
  filter(species == "CAAU") %>%
  filter(!is.na(rel_a)) %>%
  mutate(rel_b = ifelse(rel_a ==0, 0, rel_b),
         mag = ifelse(rel_a ==0, 0, mag))%>%
  arrange(hypothesis) %>%
  mutate(rel_both = ((rel_a*rel_b)-0/(4-0)),
         cvoi = (mag*rel_both),
         red = ifelse((rel_a == 0 & rel_b == 0 & mag == 0), NA, red)) %>%
  select(expert, category, hypothesis, code, name, species, rel_a, rel_b, rel_both, mag, red, cvoi, conf)
# saveRDS(caau, here("results", "caau_raw_scores.rds"))

## calculate number of experts who thought hypothesis was relevant
n_ex_rel <- caau %>%
  filter(rel_a != 0) %>% 
  select(hypothesis, code, name, expert, species, rel_a, rel_b, mag, red) %>%
  pivot_longer(6:9, names_to = "var", values_to = "val") %>%
  group_by(hypothesis, code, name, species, var) %>%
  filter(var == "rel_a" ) %>%
  summarize(n_ex = n()) %>%
  distinct(hypothesis, .keep_all = T)
n_ex <- as.vector(n_ex_rel$n_ex) 
n_ex_total <- 11 # one did not respond for CAAU

hypothesis<-unique(n_ex_rel$hypothesis) # drop hypotheses that no one thought was relevant

caau_r2_means<- caau %>%
  filter(hypothesis %in% n_ex_rel$hypothesis) %>%
  distinct(hypothesis, .keep_all = TRUE) %>%
  arrange(hypothesis) %>%
  clean_names() %>%
  dplyr::select(species, category, hypothesis, code, name, expert, rel_a, rel_b, rel_both, mag, red, cvoi, conf)

caau_r2_means$expert <- ""
caau_r2_means$rel_a <- ""
caau_r2_means$rel_b <- ""
caau_r2_means$rel_both <- ""
caau_r2_means$mag <- ""
caau_r2_means$red <- ""
caau_r2_means$cvoi <- ""
caau_r2_means$conf <- ""

## create empty vectors
cvoi_lci <- cvoi_uci <- red_lci <- red_uci <- rel_both_lci <- rel_both_uci <- mag_lci <- mag_uci <- numeric(length(hypothesis))

for(i in 1:length(hypothesis)){
  caau_r2_means$mag[i]<-sum(caau$mag[which(caau$hypothesis==hypothesis[i])])/n_ex_total
  caau_r2_means$rel_a[i]<-sum(caau$rel_a[which(caau$hypothesis==hypothesis[i])])/n_ex_total
  caau_r2_means$rel_b[i]<-sum(caau$rel_b[which(caau$hypothesis==hypothesis[i])])/n_ex_total
  caau_r2_means$red[i]<-sum(caau$red[which(caau$hypothesis==hypothesis[i])], na.rm = T)/n_ex[i]
  caau_r2_means$rel_both[i]<-sum(caau$rel_both[which(caau$hypothesis==hypothesis[i])])/n_ex_total
  caau_r2_means$cvoi[i]<-sum(caau$cvoi[which(caau$hypothesis==hypothesis[i])])/n_ex_total
  caau_r2_means$conf[i]<-sum(caau$conf[which(caau$hypothesis==hypothesis[i])])/n_ex_total
  cvoi_lci[i]<-as.numeric(quantile((caau$cvoi[which(caau$hypothesis==hypothesis[i])]), 0.05))
  cvoi_uci[i]<-as.numeric(quantile((caau$cvoi[which(caau$hypothesis==hypothesis[i])]), 0.95))
  red_lci[i]<-as.numeric(quantile((caau$red[which(caau$hypothesis==hypothesis[i])]), 0.05, na.rm = T))
  red_uci[i]<-as.numeric(quantile((caau$red[which(caau$hypothesis==hypothesis[i])]), 0.95, na.rm = T))
  rel_both_lci[i]<-as.numeric(quantile((caau$rel_both[which(caau$hypothesis==hypothesis[i])]), 0.05, na.rm = T))
  rel_both_uci[i]<-as.numeric(quantile((caau$rel_both[which(caau$hypothesis==hypothesis[i])]), 0.95, na.rm = T))
  mag_lci[i]<-as.numeric(quantile((caau$mag[which(caau$hypothesis==hypothesis[i])]), 0.05, na.rm = T))
  mag_uci[i]<-as.numeric(quantile((caau$mag[which(caau$hypothesis==hypothesis[i])]), 0.95, na.rm = T))
}

caau_summary <- data.frame(Hypothesis = unique(n_ex_rel$hypothesis),
                           Name = unique(n_ex_rel$name), 
                           CVOI_mean = as.numeric(caau_r2_means$cvoi),
                           CVOI_lci = cvoi_lci,
                           CVOI_uci = cvoi_uci,
                           Red_mean = as.numeric(caau_r2_means$red),
                           Red_lci = red_lci,
                           Red_uci = red_uci,
                           Rel_a_mean = as.numeric(caau_r2_means$rel_a),
                           Rel_b_mean = as.numeric(caau_r2_means$rel_b),
                           Rel_both_mean = as.numeric(caau_r2_means$rel_both),
                           Rel_both_lci = rel_both_lci,
                           Rel_both_uci = rel_both_uci, 
                           Mag_mean = as.numeric(caau_r2_means$mag),
                           Mag_lci = mag_lci, 
                           Mag_uci = mag_uci,
                           Num_experts = n_ex_rel$n_ex)

#write.csv(caau_summary, here("results", "caau_summary.csv"))

# Scripps's Murrelet (SCMU) ------------------------------------
scmu <- full_df %>% 
  filter(species == "SCMU") %>%
  filter(!is.na(rel_a)) %>%
  mutate(rel_b = ifelse(rel_a ==0, 0, rel_b),
         mag = ifelse(rel_a ==0, 0, mag))%>%
  arrange(hypothesis) %>%
  mutate(rel_both = ((rel_a*rel_b)-0/(4-0)),
         cvoi = (mag*rel_both),
         red = ifelse((rel_a == 0 & rel_b == 0 & mag == 0), NA, red)) %>%
  select(expert, category, hypothesis, code, name, species, rel_a, rel_b, rel_both, mag, red, cvoi, conf)
# saveRDS(scmu, here("results", "scmu_raw_scores.rds"))

## calculate number of experts who thought hypothesis was relevant
n_ex_rel <- scmu %>%
  filter(rel_a != 0) %>% 
  select(hypothesis, code, name, expert, species, rel_a, rel_b, mag, red) %>%
  pivot_longer(6:9, names_to = "var", values_to = "val") %>%
  group_by(hypothesis, code, name, species, var) %>%
  filter(var == "rel_a" ) %>%
  summarize(n_ex = n()) %>%
  distinct(hypothesis, .keep_all = T)
n_ex <- as.vector(n_ex_rel$n_ex) 
n_ex_total <- 12

hypothesis<-unique(n_ex_rel$hypothesis) # drop hypotheses that no one thought was relevant

scmu_r2_means<- scmu %>%
  filter(hypothesis %in% n_ex_rel$hypothesis) %>%
  distinct(hypothesis, .keep_all = TRUE) %>%
  arrange(hypothesis) %>%
  clean_names() %>%
  dplyr::select(species, category, hypothesis, code, name, expert, rel_a, rel_b, rel_both, mag, red, cvoi, conf)

scmu_r2_means$expert <- ""
scmu_r2_means$rel_a <- ""
scmu_r2_means$rel_b <- ""
scmu_r2_means$rel_both <- ""
scmu_r2_means$mag <- ""
scmu_r2_means$red <- ""
scmu_r2_means$cvoi <- ""
scmu_r2_means$conf <- ""

## create empty vectors
cvoi_lci <- cvoi_uci <- red_lci <- red_uci <- rel_both_lci <- rel_both_uci <- mag_lci <- mag_uci <- numeric(length(hypothesis))

for(i in 1:length(hypothesis)){
  scmu_r2_means$mag[i]<-sum(scmu$mag[which(scmu$hypothesis==hypothesis[i])])/n_ex_total
  scmu_r2_means$rel_a[i]<-sum(scmu$rel_a[which(scmu$hypothesis==hypothesis[i])])/n_ex_total
  scmu_r2_means$rel_b[i]<-sum(scmu$rel_b[which(scmu$hypothesis==hypothesis[i])])/n_ex_total
  scmu_r2_means$red[i]<-sum(scmu$red[which(scmu$hypothesis==hypothesis[i])], na.rm = T)/n_ex[i]
  scmu_r2_means$rel_both[i]<-sum(scmu$rel_both[which(scmu$hypothesis==hypothesis[i])])/n_ex_total
  scmu_r2_means$cvoi[i]<-sum(scmu$cvoi[which(scmu$hypothesis==hypothesis[i])])/n_ex_total
  scmu_r2_means$conf[i]<-sum(scmu$conf[which(scmu$hypothesis==hypothesis[i])])/n_ex_total
  cvoi_lci[i]<-as.numeric(quantile((scmu$cvoi[which(scmu$hypothesis==hypothesis[i])]), 0.05))
  cvoi_uci[i]<-as.numeric(quantile((scmu$cvoi[which(scmu$hypothesis==hypothesis[i])]), 0.95))
  red_lci[i]<-as.numeric(quantile((scmu$red[which(scmu$hypothesis==hypothesis[i])]), 0.05, na.rm = T))
  red_uci[i]<-as.numeric(quantile((scmu$red[which(scmu$hypothesis==hypothesis[i])]), 0.95, na.rm = T))
  rel_both_lci[i]<-as.numeric(quantile((scmu$rel_both[which(scmu$hypothesis==hypothesis[i])]), 0.05, na.rm = T))
  rel_both_uci[i]<-as.numeric(quantile((scmu$rel_both[which(scmu$hypothesis==hypothesis[i])]), 0.95, na.rm = T))
  mag_lci[i]<-as.numeric(quantile((scmu$mag[which(scmu$hypothesis==hypothesis[i])]), 0.05, na.rm = T))
  mag_uci[i]<-as.numeric(quantile((scmu$mag[which(scmu$hypothesis==hypothesis[i])]), 0.95, na.rm = T))
}

scmu_summary <- data.frame(Hypothesis = unique(n_ex_rel$hypothesis),
                           Name = unique(n_ex_rel$name), 
                           CVOI_mean = as.numeric(scmu_r2_means$cvoi),
                           CVOI_lci = cvoi_lci,
                           CVOI_uci = cvoi_uci,
                           Red_mean = as.numeric(scmu_r2_means$red),
                           Red_lci = red_lci,
                           Red_uci = red_uci,
                           Rel_a_mean = as.numeric(scmu_r2_means$rel_a),
                           Rel_b_mean = as.numeric(scmu_r2_means$rel_b),
                           Rel_both_mean = as.numeric(scmu_r2_means$rel_both),
                           Rel_both_lci = rel_both_lci,
                           Rel_both_uci = rel_both_uci, 
                           Mag_mean = as.numeric(scmu_r2_means$mag),
                           Mag_lci = mag_lci, 
                           Mag_uci = mag_uci,
                           Num_experts = n_ex_rel$n_ex)

# write.csv(scmu_summary, here("results", "scmu_summary.csv"))

# Western Snowy Plover (SNPL) ------------------------------------
snpl <- full_df %>% 
  filter(species == "SNPL") %>%
  filter(!is.na(rel_a)) %>%
  mutate(rel_b = ifelse(rel_a ==0, 0, rel_b),
         mag = ifelse(rel_a ==0, 0, mag))%>%
  arrange(hypothesis) %>%
  mutate(rel_both = ((rel_a*rel_b)-0/(4-0)),
         cvoi = (mag*rel_both),
         red = ifelse((rel_a == 0 & rel_b == 0 & mag == 0), NA, red)) %>%
  select(expert, category, hypothesis, code, name, species, rel_a, rel_b, rel_both, mag, red, cvoi, conf)
# saveRDS(snpl, here("results", "snpl_raw_scores.rds"))

## calculate number of experts who thought hypothesis was relevant
n_ex_rel <- snpl %>%
  filter(rel_a != 0) %>% 
  select(hypothesis, code, name, expert, species, rel_a, rel_b, mag, red) %>%
  pivot_longer(6:9, names_to = "var", values_to = "val") %>%
  group_by(hypothesis, code, name, species, var) %>%
  filter(var == "rel_a" ) %>%
  summarize(n_ex = n()) %>%
  distinct(hypothesis, .keep_all = T)
n_ex <- as.vector(n_ex_rel$n_ex) 
n_ex_total <- 12

hypothesis<-unique(n_ex_rel$hypothesis) # drop hypotheses that no one thought was relevant

snpl_r2_means<- snpl %>%
  filter(hypothesis %in% n_ex_rel$hypothesis) %>%
  distinct(hypothesis, .keep_all = TRUE) %>%
  arrange(hypothesis) %>%
  clean_names() %>%
  dplyr::select(species, category, hypothesis, code, name, expert, rel_a, rel_b, rel_both, mag, red, cvoi, conf)

snpl_r2_means$expert <- ""
snpl_r2_means$rel_a <- ""
snpl_r2_means$rel_b <- ""
snpl_r2_means$rel_both <- ""
snpl_r2_means$mag <- ""
snpl_r2_means$red <- ""
snpl_r2_means$cvoi <- ""
snpl_r2_means$conf <- ""

## create empty vectors
cvoi_lci <- cvoi_uci <- red_lci <- red_uci <- rel_both_lci <- rel_both_uci <- mag_lci <- mag_uci <- numeric(length(hypothesis))

for(i in 1:length(hypothesis)){
  snpl_r2_means$mag[i]<-sum(snpl$mag[which(snpl$hypothesis==hypothesis[i])])/n_ex_total
  snpl_r2_means$rel_a[i]<-sum(snpl$rel_a[which(snpl$hypothesis==hypothesis[i])])/n_ex_total
  snpl_r2_means$rel_b[i]<-sum(snpl$rel_b[which(snpl$hypothesis==hypothesis[i])])/n_ex_total
  snpl_r2_means$red[i]<-sum(snpl$red[which(snpl$hypothesis==hypothesis[i])], na.rm = T)/n_ex[i]
  snpl_r2_means$rel_both[i]<-sum(snpl$rel_both[which(snpl$hypothesis==hypothesis[i])])/n_ex_total
  snpl_r2_means$cvoi[i]<-sum(snpl$cvoi[which(snpl$hypothesis==hypothesis[i])])/n_ex_total
  snpl_r2_means$conf[i]<-sum(snpl$conf[which(snpl$hypothesis==hypothesis[i])])/n_ex_total
  cvoi_lci[i]<-as.numeric(quantile((snpl$cvoi[which(snpl$hypothesis==hypothesis[i])]), 0.05))
  cvoi_uci[i]<-as.numeric(quantile((snpl$cvoi[which(snpl$hypothesis==hypothesis[i])]), 0.95))
  red_lci[i]<-as.numeric(quantile((snpl$red[which(snpl$hypothesis==hypothesis[i])]), 0.05, na.rm = T))
  red_uci[i]<-as.numeric(quantile((snpl$red[which(snpl$hypothesis==hypothesis[i])]), 0.95, na.rm = T))
  rel_both_lci[i]<-as.numeric(quantile((snpl$rel_both[which(snpl$hypothesis==hypothesis[i])]), 0.05, na.rm = T))
  rel_both_uci[i]<-as.numeric(quantile((snpl$rel_both[which(snpl$hypothesis==hypothesis[i])]), 0.95, na.rm = T))
  mag_lci[i]<-as.numeric(quantile((snpl$mag[which(snpl$hypothesis==hypothesis[i])]), 0.05, na.rm = T))
  mag_uci[i]<-as.numeric(quantile((snpl$mag[which(snpl$hypothesis==hypothesis[i])]), 0.95, na.rm = T))
}

snpl_summary <- data.frame(Hypothesis = unique(n_ex_rel$hypothesis),
                           Name = unique(n_ex_rel$name), 
                           CVOI_mean = as.numeric(snpl_r2_means$cvoi),
                           CVOI_lci = cvoi_lci,
                           CVOI_uci = cvoi_uci,
                           Red_mean = as.numeric(snpl_r2_means$red),
                           Red_lci = red_lci,
                           Red_uci = red_uci,
                           Rel_a_mean = as.numeric(snpl_r2_means$rel_a),
                           Rel_b_mean = as.numeric(snpl_r2_means$rel_b),
                           Rel_both_mean = as.numeric(snpl_r2_means$rel_both),
                           Rel_both_lci = rel_both_lci,
                           Rel_both_uci = rel_both_uci, 
                           Mag_mean = as.numeric(snpl_r2_means$mag),
                           Mag_lci = mag_lci, 
                           Mag_uci = mag_uci,
                           Num_experts = n_ex_rel$n_ex)

# write.csv(snpl_summary, here("results", "snpl_summary.csv"))

# Western Gull (WEGU) ------------------------------------
wegu <- full_df %>% 
  filter(species == "WEGU") %>%
  filter(!is.na(rel_a)) %>%
  mutate(rel_b = ifelse(rel_a ==0, 0, rel_b),
         mag = ifelse(rel_a ==0, 0, mag))%>%
  arrange(hypothesis) %>%
  mutate(rel_both = ((rel_a*rel_b)-0/(4-0)),
         cvoi = (mag*rel_both),
         red = ifelse((rel_a == 0 & rel_b == 0 & mag == 0), NA, red)) %>%
  select(expert, category, hypothesis, code, name, species, rel_a, rel_b, rel_both, mag, red, cvoi, conf)
# saveRDS(wegu, here("results", "wegu_raw_scores.rds"))

## calculate number of experts who thought hypothesis was relevant
n_ex_rel <- wegu %>%
  filter(rel_a != 0) %>% 
  select(hypothesis, code, name, expert, species, rel_a, rel_b, mag, red) %>%
  pivot_longer(6:9, names_to = "var", values_to = "val") %>%
  group_by(hypothesis, code, name, species, var) %>%
  filter(var == "rel_a" ) %>%
  summarize(n_ex = n()) %>%
  distinct(hypothesis, .keep_all = T)
n_ex <- as.vector(n_ex_rel$n_ex) 
n_ex_total <- 12

hypothesis<-unique(n_ex_rel$hypothesis) # drop hypotheses that no one thought was relevant

wegu_r2_means<- wegu %>%
  filter(hypothesis %in% n_ex_rel$hypothesis) %>%
  distinct(hypothesis, .keep_all = TRUE) %>%
  arrange(hypothesis) %>%
  clean_names() %>%
  dplyr::select(species, category, hypothesis, code, name, expert, rel_a, rel_b, rel_both, mag, red, cvoi, conf)

wegu_r2_means$expert <- ""
wegu_r2_means$rel_a <- ""
wegu_r2_means$rel_b <- ""
wegu_r2_means$rel_both <- ""
wegu_r2_means$mag <- ""
wegu_r2_means$red <- ""
wegu_r2_means$cvoi <- ""
wegu_r2_means$conf <- ""

## create empty vectors
cvoi_lci <- cvoi_uci <- red_lci <- red_uci <- rel_both_lci <- rel_both_uci <- mag_lci <- mag_uci <- numeric(length(hypothesis))

for(i in 1:length(hypothesis)){
  wegu_r2_means$mag[i]<-sum(wegu$mag[which(wegu$hypothesis==hypothesis[i])])/n_ex_total
  wegu_r2_means$rel_a[i]<-sum(wegu$rel_a[which(wegu$hypothesis==hypothesis[i])])/n_ex_total
  wegu_r2_means$rel_b[i]<-sum(wegu$rel_b[which(wegu$hypothesis==hypothesis[i])])/n_ex_total
  wegu_r2_means$red[i]<-sum(wegu$red[which(wegu$hypothesis==hypothesis[i])], na.rm = T)/n_ex[i]
  wegu_r2_means$rel_both[i]<-sum(wegu$rel_both[which(wegu$hypothesis==hypothesis[i])])/n_ex_total
  wegu_r2_means$cvoi[i]<-sum(wegu$cvoi[which(wegu$hypothesis==hypothesis[i])])/n_ex_total
  wegu_r2_means$conf[i]<-sum(wegu$conf[which(wegu$hypothesis==hypothesis[i])])/n_ex_total
  cvoi_lci[i]<-as.numeric(quantile((wegu$cvoi[which(wegu$hypothesis==hypothesis[i])]), 0.05))
  cvoi_uci[i]<-as.numeric(quantile((wegu$cvoi[which(wegu$hypothesis==hypothesis[i])]), 0.95))
  red_lci[i]<-as.numeric(quantile((wegu$red[which(wegu$hypothesis==hypothesis[i])]), 0.05, na.rm = T))
  red_uci[i]<-as.numeric(quantile((wegu$red[which(wegu$hypothesis==hypothesis[i])]), 0.95, na.rm = T))
  rel_both_lci[i]<-as.numeric(quantile((wegu$rel_both[which(wegu$hypothesis==hypothesis[i])]), 0.05, na.rm = T))
  rel_both_uci[i]<-as.numeric(quantile((wegu$rel_both[which(wegu$hypothesis==hypothesis[i])]), 0.95, na.rm = T))
  mag_lci[i]<-as.numeric(quantile((wegu$mag[which(wegu$hypothesis==hypothesis[i])]), 0.05, na.rm = T))
  mag_uci[i]<-as.numeric(quantile((wegu$mag[which(wegu$hypothesis==hypothesis[i])]), 0.95, na.rm = T))
}

wegu_summary <- data.frame(Hypothesis = unique(n_ex_rel$hypothesis),
                           Name = unique(n_ex_rel$name), 
                           CVOI_mean = as.numeric(wegu_r2_means$cvoi),
                           CVOI_lci = cvoi_lci,
                           CVOI_uci = cvoi_uci,
                           Red_mean = as.numeric(wegu_r2_means$red),
                           Red_lci = red_lci,
                           Red_uci = red_uci,
                           Rel_a_mean = as.numeric(wegu_r2_means$rel_a),
                           Rel_b_mean = as.numeric(wegu_r2_means$rel_b),
                           Rel_both_mean = as.numeric(wegu_r2_means$rel_both),
                           Rel_both_lci = rel_both_lci,
                           Rel_both_uci = rel_both_uci, 
                           Mag_mean = as.numeric(wegu_r2_means$mag),
                           Mag_lci = mag_lci, 
                           Mag_uci = mag_uci,
                           Num_experts = n_ex_rel$n_ex)

# write.csv(wegu_summary, here("results", "wegu_summary.csv"))

