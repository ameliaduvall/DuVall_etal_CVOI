## CVOI across spp

## load libraries
library(here);library(ggplot2);library(ggrepel);library(ggstance);
library(scales); library(cowplot); library(viridis); library(RColorBrewer);
library(janitor); library(tidyverse); library(jcolors); library(RColorBrewer)

## load categories
full_df <- readRDS(here("data", "full_df.rds")) %>%
  dplyr::select(category, hypothesis, code, name) %>%
  distinct()

## load spp data ####
assp_boot <- readRDS(here("results", "assp_bootstrapped.RDS"))
brpe_boot <- readRDS(here("results", "brpe_bootstrapped.RDS"))
caau_boot <- readRDS(here("results", "caau_bootstrapped.RDS"))
scmu_boot <- readRDS(here("results", "scmu_bootstrapped.RDS"))
wegu_boot <- readRDS(here("results", "wegu_bootstrapped.RDS"))
snpl_boot <- readRDS(here("results", "snpl_bootstrapped.RDS"))

## combine
all_boot <- bind_rows(assp_boot, brpe_boot, caau_boot, scmu_boot, wegu_boot, snpl_boot)


all_boot$CVOI_scaled <- all_boot$mean_cvoi / 6
all_boot$reducibility_scaled <- all_boot$mean_red / 6

df_summary <- all_boot %>%
  group_by(hypothesis) %>%
  summarise(
    total_CVOI = sum(CVOI_scaled, na.rm = TRUE),
    total_reducibility = sum(reducibility_scaled, na.rm = TRUE)
  )

ggplot(df_summary) + 
  geom_point(aes(x = total_CVOI, y = total_reducibility, color = as.factor(hypothesis))) +
  geom_label(aes(x = total_CVOI, y = total_reducibility, label = as.factor(hypothesis)))
