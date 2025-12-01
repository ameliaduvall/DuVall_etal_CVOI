
## load libraries
library(here);library(ggplot2);library(ggrepel);library(ggstance);
library(scales); library(cowplot); library(RColorBrewer);
library(janitor); library(tidyverse); library(jcolors)

## load categories
cats <- readRDS(here("results", "round_2_all_results.rds")) %>%
  dplyr::select(category, hypothesis, code, name) %>%
  distinct()

## load data
df <- readRDS(here("results", "all_spp_avg_bootstrapped.RDS")) %>%
  mutate(cvoi_x_min = mean_cvoi-sqrt(var_cvoi),
         cvoi_x_max = mean_cvoi+sqrt(var_cvoi),
         red_y_min = mean_red-sqrt(var_red),
         red_y_max = mean_red+sqrt(var_red)) %>%
  left_join(., cats, by = c("hypothesis", "name"))

## set colors
cols <- c('mardi_gras', 'blue_yonder', 'deep_taupe', 'darkgrey', 'tiffany_blue', 
          'orioles_orange', 'vivid_yellow', 'moderate_red')

## plot
ggplot(df, aes(x = mean_cvoi, y = mean_red)) + 
  geom_point(aes(x = mean_cvoi, y = mean_red, color = category),
             size = 4, stroke = 0, shape = 16) +
  geom_vline(aes(xintercept = median(df$mean_cvoi)), linetype = 'dashed',color = 'black',linewidth = 0.75) +
  geom_hline(aes(yintercept = median(df$mean_red, na.rm = T)), linetype = 'dashed', color = 'black',linewidth = 0.75) +
  geom_label_repel(aes(label = code, fill = category), max.overlaps = 500, show.legend = F, size = 5) +
  scale_color_brewer(palette = 'Set2') +
  scale_fill_brewer(palette = 'Set2') +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = 'bottom') +
  scale_y_continuous(name = "Reducibility", breaks = seq(0,4,1), limits = c(-0.25,4.25)) +
  scale_x_continuous(name = "Constructed value of information", breaks = seq(-2,12,1), limits = c(-2,12)) +
  geom_text(x = -0.75, y = -0.25, label = "Low priority", color = "black", fontface = "bold", size = 8) +
  geom_text(x = -0.75, y = 4.3, label = "Medium priority", color = "black", fontface = "bold", size = 8) +
  geom_text(x = 7.0, y = -0.25, label = "High priority", color = "black", fontface = "bold", size = 8) +
  geom_text(x = 7.0, y = 4.3, label = "Highest priority", color = "black", fontface = "bold", size = 8) +
  theme(text = element_text(size = 20))

ggsave(here("figures", "all_spp_avg_cvoi.png"), width = 12, height = 8)

## plot w/ linear conf
df2<- readRDS(here("results", "all_spp_avg_bootstrapped_linear_conf.RDS")) %>%
  mutate(cvoi_x_min = mean_cvoi-sqrt(var_cvoi),
         cvoi_x_max = mean_cvoi+sqrt(var_cvoi),
         red_y_min = mean_red-sqrt(var_red),
         red_y_max = mean_red+sqrt(var_red)) %>%
  left_join(., cats, by = c("hypothesis", "name"))

ggplot(df2, aes(x = mean_cvoi, y = mean_red)) + 
  geom_point(aes(x = mean_cvoi, y = mean_red, color = category),
             size = 4, stroke = 0, shape = 16) +
  geom_vline(aes(xintercept = median(df2$mean_cvoi)), linetype = 'dashed',color = 'black',linewidth = 0.75) +
  geom_hline(aes(yintercept = median(df2$mean_red, na.rm = T)), linetype = 'dashed', color = 'black',linewidth = 0.75) +
  geom_label_repel(aes(label = code, fill = category), max.overlaps = 500, show.legend = F, size = 5) +
  scale_color_brewer(palette = 'Set2') +
  scale_fill_brewer(palette = 'Set2') +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = 'bottom') +
  scale_y_continuous(name = "Reducibility", breaks = seq(0,4,1), limits = c(-0.25,4.25)) +
  scale_x_continuous(name = "Constructed value of information", breaks = seq(-2,12,1), limits = c(-2,12)) +
  geom_text(x = -0.75, y = -0.25, label = "Low priority", color = "black", fontface = "bold", size = 8) +
  geom_text(x = -0.75, y = 4.3, label = "Medium priority", color = "black", fontface = "bold", size = 8) +
  geom_text(x = 7.0, y = -0.25, label = "High priority", color = "black", fontface = "bold", size = 8) +
  geom_text(x = 7.0, y = 4.3, label = "Highest priority", color = "black", fontface = "bold", size = 8) +
  theme(text = element_text(size = 20))

ggsave(here("figures", "all_spp_avg_cvoi_linear_conf.png"), width = 12, height = 8)
