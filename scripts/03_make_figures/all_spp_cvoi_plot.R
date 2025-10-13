## CVoI Plots
## A DuVall ajduvall@uw.edu

## load libraries
library(here);library(ggplot2);library(ggrepel);library(ggstance);
library(scales); library(cowplot); library(viridis); library(RColorBrewer);
library(janitor); library(tidyverse); library(jcolors); library(RColorBrewer)

cols <- c('mardi_gras', 'blue_yonder', 'deep_taupe', 'darkgrey', 'tiffany_blue', 
          'orioles_orange', 'vivid_yellow', 'moderate_red')

## load categories
full_df <- readRDS(here("data", "full_df.rds")) %>%
  dplyr::select(category, hypothesis, code, name) %>%
  distinct()

## snpl ####
assp_boot <- readRDS(here("results", "assp_bootstrapped.RDS"))
brpe_boot <- readRDS(here("results", "brpe_bootstrapped.RDS"))
caau_boot <- readRDS(here("results", "caau_bootstrapped.RDS"))
scmu_boot <- readRDS(here("results", "scmu_bootstrapped.RDS"))
snpl_boot <- readRDS(here("results", "snpl_bootstrapped.RDS"))
wegu_boot <- readRDS(here("results", "wegu_bootstrapped.RDS"))

df <- bind_rows(assp_boot, brpe_boot, caau_boot, scmu_boot, snpl_boot, wegu_boot) %>%
  mutate(cvoi_x_min = mean_cvoi-sqrt(var_cvoi),
         cvoi_x_max = mean_cvoi+sqrt(var_cvoi),
         red_y_min = mean_red-sqrt(var_red),
         red_y_max = mean_red+sqrt(var_red)) %>%
  left_join(., full_df, by = c("hypothesis", "name")) #%>%
  #filter(hypothesis %in% c(2, 11, 6, 19))


#pal <- c("#E78AC3", "#E5C494", "#8DA0CB", "#66C2A5")
pal <- c("#66C2A5", "#8DA0CB", "#E78AC3","#E5C494")

ggplot(df %>% filter(hypothesis %in% c(2, 11, 6, 19)), aes(x = mean_cvoi, y = mean_red)) + 
  geom_point(aes(x = mean_cvoi, y = mean_red, color = category),
             size = 4, stroke = 0, shape = 16) +
  # geom_linerange(aes(xmin = cvoi_x_min, xmax = cvoi_x_max, color = category), 
  #                alpha = 1, linetype = "solid") +
  # geom_linerange(aes(ymin = red_y_min, ymax = red_y_max, color = category), 
  #                alpha = 1, linetype = "solid") +
  geom_vline(aes(xintercept = median(df$mean_cvoi)), linetype = 'dashed',color = 'black',linewidth = 0.75) +
  geom_hline(aes(yintercept = median(df$mean_red, na.rm = T)), linetype = 'dashed', color = 'black',linewidth = 0.75) +
  geom_label_repel(aes(label = name, fill = category), max.overlaps = 500, show.legend = F, size = 5) +
  #scale_color_brewer(palette = 'Set2') +
  #scale_fill_brewer(palette = 'Set2') +
  scale_color_manual(values = pal) +
  scale_fill_manual(values = pal) +
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
        #plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave(here("figures", "final", "all_spp_test_cvoi.png"), width = 12, height = 8)

## all highest priority
pal2 <- c("#66C2A5", "#8DA0CB", "#A6D854", "#B3B3B3")

ggplot(df %>% filter(hypothesis %in% c(5, 13, 27, 2, 14, 12)), aes(x = mean_cvoi, y = mean_red)) + 
  geom_point(aes(x = mean_cvoi, y = mean_red, color = category),
             size = 4, stroke = 0, shape = 16) +
  # geom_linerange(aes(xmin = cvoi_x_min, xmax = cvoi_x_max, color = category), 
  #                alpha = 1, linetype = "solid") +
  # geom_linerange(aes(ymin = red_y_min, ymax = red_y_max, color = category), 
  #                alpha = 1, linetype = "solid") +
  geom_vline(aes(xintercept = median(df$mean_cvoi)), linetype = 'dashed',color = 'black',linewidth = 0.75) +
  geom_hline(aes(yintercept = median(df$mean_red, na.rm = T)), linetype = 'dashed', color = 'black',linewidth = 0.75) +
  geom_label_repel(aes(label = name, fill = category), max.overlaps = 500, show.legend = F, size = 5) +
  #scale_color_brewer(palette = 'Set2') +
  #scale_fill_brewer(palette = 'Set2') +
  scale_color_manual(values = pal2) +
  scale_fill_manual(values = pal2) +
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
#plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave(here("figures", "final", "all_spp_highest_cvoi.png"), width = 12, height = 8)

## all ####

ggplot(df, aes(x = mean_cvoi, y = mean_red)) + 
  geom_point(aes(x = mean_cvoi, y = mean_red, color = category),
             size = 4, stroke = 0, shape = 16) +
  # geom_linerange(aes(xmin = cvoi_x_min, xmax = cvoi_x_max, color = category), 
  #                alpha = 1, linetype = "solid") +
  # geom_linerange(aes(ymin = red_y_min, ymax = red_y_max, color = category), 
  #                alpha = 1, linetype = "solid") +
  geom_vline(aes(xintercept = median(df$mean_cvoi)), linetype = 'dashed',color = 'black',linewidth = 0.75) +
  geom_hline(aes(yintercept = median(df$mean_red, na.rm = T)), linetype = 'dashed', color = 'black',linewidth = 0.75) +
  geom_label_repel(aes(label = name, fill = category), max.overlaps = 500, show.legend = F, size = 5) +
  scale_color_brewer(palette = 'Set2') +
  scale_fill_brewer(palette = 'Set2') +
  # scale_color_manual(values = pal2) +
  # scale_fill_manual(values = pal2) +
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
#plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave(here("figures", "final", "all_spp_cvoi.png"), width = 12, height = 8)
