## Make plots that indicate how considering confidence changes results

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

## wegu ####
wegu_boot1 <- readRDS(here("results", "wegu_bootstrapped.rds")) %>%
  as.data.frame() %>%
  mutate(weight = c('full'))
wegu_boot2 <- readRDS(here("results", "wegu_bootstrapped_linear_conf.rds")) %>%
  as.data.frame() %>%
  mutate(weight = c('linear'))

wegu_df <- bind_rows(wegu_boot1, wegu_boot2) %>%
  left_join(., full_df, by = c("hypothesis", "name"))
wegu_conf <- bind_rows(wegu_boot1, wegu_boot2) %>%
  pivot_wider(names_from = "weight", values_from = c(mean_cvoi, var_cvoi, mean_red, var_red)) %>%
  left_join(., full_df, by = c("hypothesis", "name"))

ggplot() +
  geom_segment(data = wegu_conf, aes(x = mean_cvoi_full, y = mean_red_full,
                                    xend = mean_cvoi_linear, yend = mean_red_linear, color = category),
              arrow = arrow(length = unit(0.02, "npc")),
              size = 1.5) +
  geom_point(data = wegu_df %>% filter(weight == "full"),
             aes(x = mean_cvoi, y = mean_red, color = category),
             size = 4, stroke = 0, shape = 16) +
  geom_point(data = wegu_df %>% filter(weight == "linear"),
             aes(x = mean_cvoi, y = mean_red, color = category),
             shape = 4, size = 4) +
  geom_vline(aes(xintercept = median(wegu_df$mean_cvoi)), 
             linetype = 'dashed',color = 'black',linewidth = 0.75) +
  geom_hline(aes(yintercept = median(wegu_df$mean_red, na.rm = T)), 
             linetype = 'dashed', color = 'black',linewidth = 0.75) +
  geom_label_repel(data = wegu_df %>% filter(weight == "full"),
                   aes(x = mean_cvoi, y = mean_red, label = code, fill = category),
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
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = 'bottom') +
  scale_y_continuous(name = "Reducibility", breaks = seq(0,4,1), limits = c(-0.25,4.25)) +
  scale_x_continuous(name = "Constructed value of information", breaks = seq(-2,13.1,1), limits = c(-2,13.1)) +
  annotate("text", x = -0.75, y = -0.25, label = "Low priority", color = "black", fontface = "bold", size = 8) +
  annotate("text", x = -0.75, y = 4.25, label = "Medium priority", color = "black", fontface = "bold", size = 8) +
  annotate("text", x = 7.0, y = -0.25, label = "High priority", color = "black", fontface = "bold", size = 8) +
  annotate("text", x = 7.0, y = 4.25, label = "Highest priority", color = "black", fontface = "bold", size = 8) +
  ggtitle("WESTERN GULL",
          subtitle = "Change in hypothesis priority under different confidence weightings") +
  theme(text = element_text(size = 20),
        plot.title = element_text(hjust = 0.5, face = "bold"),
        plot.subtitle = element_text(hjust = 0.5))

#ggsave(here("figures", "final", "wegu_conf_cvoi.png"), width = 12, height = 8)  

## compare priority rankings ####
## look at priority rankings for full confidence
median_cvoi <- median(wegu_boot1$mean_cvoi)
median_red <- median(wegu_boot1$mean_red, na.rm = TRUE)

wegu_rankings <- wegu_boot1%>%
  mutate(quadrat = case_when(
    mean_cvoi >= median_cvoi & mean_red >= median_red ~ "Highest priority",
    mean_cvoi < median_cvoi & mean_red >= median_red ~ "Medium priority",
    mean_cvoi >= median_cvoi & mean_red < median_red ~ "High priority",
    TRUE ~ "Low priority"
  )) %>%
  dplyr::select(hypothesis, name, weight, quadrat)


## look at priority rankings for linear confidence
median_cvoi2 <- median(wegu_boot2$mean_cvoi)
median_red2 <- median(wegu_boot2$mean_red, na.rm = TRUE)

wegu_rankings2 <- wegu_boot2 %>%
  mutate(quadrat = case_when(
    mean_cvoi >= median_cvoi2 & mean_red >= median_red2 ~ "Highest priority",
    mean_cvoi < median_cvoi2 & mean_red >= median_red2 ~ "Medium priority",
    mean_cvoi >= median_cvoi2 & mean_red < median_red2 ~ "High priority",
    TRUE ~ "Low priority"
  )) %>%
  dplyr::select(hypothesis, name, weight, quadrat)

## compare df
chk <- full_join(wegu_rankings, wegu_rankings2, by = c("hypothesis", "name"))




