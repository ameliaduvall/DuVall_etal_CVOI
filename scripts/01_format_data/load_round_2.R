## Load round 2 responses

## load libraries
library(here)
library(tidyverse)
library(readxl)
library(janitor)

## load responses
file.list <- list.files(path = here("data", "scores"), pattern = '*.xlsx')
rel_a <- lapply(here("data", "scores", file.list), read_excel, sheet = 1, na = c("NA"))
rel_b <- lapply(here("data", "scores", file.list), read_excel, sheet = 2, na = c("NA"))
mag <- lapply(here("data","scores", file.list), read_excel, sheet = 3, na = c("NA"))
red <- lapply(here("data", "scores", file.list), read_excel, sheet = 4, na = c("NA"))
conf <- lapply(here("data", "scores", file.list), read_excel, sheet = 5, na = c("NA"))

## load hypothesis names
names <- read_excel(here("data", "hypothesis_names.xlsx")) %>%
  mutate(hypothesis = c(1:28))

## relevance a
rel_a_df <- NULL
for (i in 1:length(rel_a)) {
  df <- rel_a[[i]] %>%
    rename(temp = "...2") %>%
    filter(!is.na(temp)) %>%
    dplyr::select("...3") %>%
    rename("score" = "...3") %>%
    mutate(var = c("rel_a"),
           hypothesis = rep(1:28, each = 6),
           species = rep(c("assp", "brpe", "scmu", "snpl", "wegu", "caau"), 28),
           expert = paste0("ex", i, sep = ""),
           score = as.numeric(score)) %>%
    dplyr::select(expert, var, hypothesis, species, score)
  
  rel_a_df <- rbind(rel_a_df, df)
  
}

## relevance b
rel_b_df <- NULL
for (i in 1:length(rel_b)) {
  df <- rel_b[[i]] %>%
    rename(temp = "...2") %>%
    filter(!is.na(temp)) %>%
    dplyr::select("...3") %>%
    rename("score" = "...3") %>%
    mutate(var = c("rel_b"),
           hypothesis = rep(1:28, each = 6),
           species = rep(c("assp", "brpe", "scmu", "snpl", "wegu", "caau"), 28),
           expert = paste0("ex", i, sep = ""),
           score = as.numeric(score)) %>%
    dplyr::select(expert, var, hypothesis, species, score)
  
  rel_b_df <- rbind(rel_b_df, df)
  
}

## magnitude of uncertainty
mag_df <- NULL
for (i in 1:length(mag)) {
  df <- mag[[i]] %>%
    rename(temp = "...2") %>%
    filter(!is.na(temp)) %>%
    dplyr::select("...3") %>%
    rename("score" = "...3") %>%
    mutate(var = c("mag"),
           hypothesis = rep(1:28, each = 6),
           species = rep(c("assp", "brpe", "scmu", "snpl", "wegu", "caau"), 28),
           expert = paste0("ex", i, sep = ""),
           score = as.numeric(score)) %>%
    dplyr::select(expert, var, hypothesis, species, score)
  
  mag_df <- rbind(mag_df, df)
  
}

## reducibility
red_df <- NULL
for (i in 1:length(red)) {
  df <- red[[i]] %>%
    rename(temp = "...2") %>%
    filter(!is.na(temp)) %>%
    dplyr::select("...3") %>%
    rename("score" = "...3") %>%
    mutate(var = c("red"),
           hypothesis = rep(1:28, each = 6),
           species = rep(c("assp", "brpe", "scmu", "snpl", "wegu", "caau"), 28),
           expert = paste0("ex", i, sep = ""),
           score = as.numeric(score)) %>%
    dplyr::select(expert, var, hypothesis, species, score)
  
  red_df <- rbind(red_df, df)
  
}

## personal confidence
conf_df <- NULL
for (i in 1:length(conf)) {
  df <- conf[[i]] %>%
    rename(temp = "...2") %>%
    filter(!is.na(temp)) %>%
    dplyr::select("...3") %>%
    rename("score" = "...3") %>%
    mutate(var = c("conf"),
           hypothesis = rep(1:28, each = 6),
           species = rep(c("assp", "brpe", "scmu", "snpl", "wegu", "caau"), 28),
           expert = paste0("ex", i, sep = ""),
           score = as.numeric(score)) %>%
    dplyr::select(expert, var, hypothesis, species, score)
  
  conf_df <- rbind(conf_df, df)
  
}

## combine
full_df <- bind_rows(rel_a_df, rel_b_df, mag_df, red_df, conf_df) %>%
  pivot_wider(names_from = var, values_from = score) %>%
  left_join(names, by = "hypothesis") %>%
  mutate(species = toupper(species)) %>%
  clean_names() %>%
  dplyr::select(expert, category, hypothesis, code, name, species, rel_a, rel_b, mag, red, conf)

# write.csv(full_df, here("results", "round_2_all_results.csv"))
# saveRDS(full_df, here("results", "round_2_all_results.RDS"))
