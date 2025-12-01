## CVoI Plots
## A DuVall ajduvall@uw.edu

## load libraries
library(here);library(ggplot2);library(ggrepel);library(ggstance);
library(scales); library(cowplot); library(viridis); library(RColorBrewer);
library(janitor); library(tidyverse); library(jcolors)

cols <- c('mardi_gras', 'blue_yonder', 'deep_taupe', 'darkgrey', 'tiffany_blue', 
          'orioles_orange', 'vivid_yellow', 'moderate_red')

## load categories
full_df <- readRDS(here("results", "round_2_all_results.RDS")) %>%
  dplyr::select(category, hypothesis, code, name) %>%
  distinct()

## assp ####
assp_boot <- readRDS(here("results", "assp_bootstrapped.RDS"))
assp_df <- assp_boot %>%
  mutate(cvoi_x_min = mean_cvoi-sqrt(var_cvoi),
         cvoi_x_max = mean_cvoi+sqrt(var_cvoi),
         red_y_min = mean_red-sqrt(var_red),
         red_y_max = mean_red+sqrt(var_red)) %>%
  left_join(., full_df, by = c("hypothesis", "name"))

ggplot(assp_df, aes(x = mean_cvoi, y = mean_red)) + 
  geom_point(aes(x = mean_cvoi, y = mean_red, color = category),
             size = 4, stroke = 0, shape = 16) +
  geom_linerange(aes(xmin = cvoi_x_min, xmax = cvoi_x_max, color = category), 
                 alpha = 1, linetype = "solid") +
  geom_linerange(aes(ymin = red_y_min, ymax = red_y_max, color = category), 
                 alpha = 1, linetype = "solid") +
  geom_vline(aes(xintercept = median(assp_df$mean_cvoi)), linetype = 'dashed',color = 'black',linewidth = 0.75) +
  geom_hline(aes(yintercept = median(assp_df$mean_red, na.rm = T)), linetype = 'dashed', color = 'black',linewidth = 0.75) +
  geom_label_repel(aes(label = name, fill = category), #size = mean_cvoi), 
                   max.overlaps = Inf, 
                   box.padding = 0.5,
                   point.padding = 0.5,
                   label.padding = 0.25,
                   force = 2, 
                   force_pull = 0.5,
                   segment.size = 0.2,
                   segment.color = "grey50",
                   #nudge_y = 0.2, nudge_x = 0.2,
                   show.legend = F, size = 5) +
  scale_color_brewer(palette = 'Set2') +
  scale_fill_brewer(palette = 'Set2') +
  #scale_size_continuous(range = c(2.5,6)) +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = 'bottom') +
  scale_y_continuous(name = "Reducibility", breaks = seq(0,4,1), limits = c(-0.25,4.25)) +
  scale_x_continuous(name = "Constructed value of information", breaks = seq(-2,13.1,1), limits = c(-2,13.1)) +
  geom_text(x = -0.75, y = -0.25, label = "Low priority", color = "black", fontface = "bold", size = 8) +
  geom_text(x = -0.75, y = 4.3, label = "Medium priority", color = "black", fontface = "bold", size = 8) +
  geom_text(x = 7.0, y = -0.25, label = "High priority", color = "black", fontface = "bold", size = 8) +
  geom_text(x = 7.0, y = 4.3, label = "Highest priority", color = "black", fontface = "bold", size = 8) +
  ggtitle("ASHY STORM-PETREL") +
  theme(text = element_text(size = 20),
        plot.title = element_text(hjust = 0.5, face = "bold"))

#ggsave(here("figures", "assp_cvoi.png"), width = 12, height = 8)

## try with code
ggplot(assp_df, aes(x = mean_cvoi, y = mean_red)) + 
  geom_point(aes(x = mean_cvoi, y = mean_red, color = category),
             size = 4, stroke = 0, shape = 16) +
  geom_linerange(aes(xmin = cvoi_x_min, xmax = cvoi_x_max, color = category), 
                 alpha = 1, linetype = "solid", linewidth = 1) +
  geom_linerange(aes(ymin = red_y_min, ymax = red_y_max, color = category), 
                 alpha = 1, linetype = "solid", linewidth = 1) +
  geom_vline(aes(xintercept = median(assp_df$mean_cvoi)), 
             linetype = 'dashed',color = 'black',linewidth = 0.75) +
  geom_hline(aes(yintercept = median(assp_df$mean_red, na.rm = T)), 
             linetype = 'dashed', color = 'black',linewidth = 0.75) +
  geom_label_repel(aes(label = code, fill = category), #size = mean_cvoi), 
                   max.overlaps = Inf, 
                   box.padding = 0.5,
                   point.padding = 0.5,
                   label.padding = 0.25,
                   force = 2, 
                   force_pull = 0.5,
                   segment.size = 0.2,
                   segment.color = "grey50",
                   nudge_y = 0.2, nudge_x = 0.2,
                   show.legend = F, size = 5) +
  scale_color_brewer(palette = 'Set2') +
  scale_fill_brewer(palette = 'Set2') +
  #scale_size_continuous(range = c(2.5,6)) +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = 'bottom') +
  scale_y_continuous(name = "Reducibility", breaks = seq(0,4,1), limits = c(-0.25,4.25)) +
  scale_x_continuous(name = "Constructed value of information", breaks = seq(-2,13.1,1), limits = c(-2,13.1)) +
  geom_text(x = -0.75, y = -0.25, label = "Low priority", color = "black", fontface = "bold", size = 8) +
  geom_text(x = -0.75, y = 4.3, label = "Medium priority", color = "black", fontface = "bold", size = 8) +
  geom_text(x = 7.0, y = -0.25, label = "High priority", color = "black", fontface = "bold", size = 8) +
  geom_text(x = 7.0, y = 4.3, label = "Highest priority", color = "black", fontface = "bold", size = 8) +
  ggtitle("ASHY STORM-PETREL") +
  theme(text = element_text(size = 20),
        plot.title = element_text(hjust = 0.5, face = "bold"))

#ggsave(here("figures", "assp_cvoi_hCode.png"), width = 12, height = 8)

## look at priority rankings
median_cvoi <- median(assp_df$mean_cvoi)
median_red <- median(assp_df$mean_red, na.rm = TRUE)

assp_rankings <- assp_df
assp_rankings$quadrat <- with(assp_df, ifelse(mean_cvoi >= median_cvoi & mean_red >= median_red, "Highest priority",
                                         ifelse(mean_cvoi < median_cvoi & mean_red >= median_red, "Medium priority",
                                                ifelse(mean_cvoi >= median_cvoi & mean_red < median_red, "High priority",
                                                       "Low priority")))) 
  


## another plot
ggplot(df, aes(x = mean_cvoi, y = mean_red)) + 
  geom_point(aes(x = mean_cvoi, y = mean_red, fill = category), color = 'grey', 
             size = 4, stroke = 0, shape = 16) +
  geom_linerange(aes(xmin = cvoi_x_min, xmax = cvoi_x_max), color = 'grey',# color = category), 
                 alpha = 1, linetype = "solid") +
  geom_linerange(aes(ymin = red_y_min, ymax = red_y_max), color = 'grey', #color = category), 
                 alpha = 1, linetype = "solid") +
  geom_vline(aes(xintercept = median(df$mean_cvoi)), linetype = 'dashed',color = 'black',linewidth = 0.75) +
  geom_hline(aes(yintercept = median(df$mean_red, na.rm = T)), linetype = 'dashed', color = 'black',linewidth = 0.75) +
  geom_label_repel(aes(label = name), fill = 'lightgrey', max.overlaps = 500, show.legend = F, size = 5) +
  #scale_color_brewer(palette = 'Set2') +
  #scale_fill_brewer(palette = 'Set2') +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = 'bottom') +
  scale_y_continuous(name = "Reducibility", breaks = seq(0,4,1), limits = c(-0.25,4.25)) +
  scale_x_continuous(name = "Constructed value of information", breaks = seq(-2,13.1,1), limits = c(-2,13.1)) +
  geom_text(x = -0.75, y = -0.25, label = "Low priority", color = "black", fontface = "bold", size = 8) +
  geom_text(x = -0.75, y = 4.3, label = "Medium priority", color = "black", fontface = "bold", size = 8) +
  geom_text(x = 7.0, y = -0.25, label = "High priority", color = "black", fontface = "bold", size = 8) +
  geom_text(x = 7.0, y = 4.3, label = "Highest priority", color = "black", fontface = "bold", size = 8) +
  ggtitle("ASHY STORM-PETREL") +
  theme(text = element_text(size = 20),
        plot.title = element_text(hjust = 0.5, face = "bold"))

ggsave(here("figures", "final", "assp_all_grey.png"), width = 12, height = 8)
