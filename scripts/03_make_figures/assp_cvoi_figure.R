## CVoI Plots
## A DuVall ajduvall@uw.edu

## load libraries
library(here);library(ggplot2);library(ggrepel);library(ggstance);
library(scales); library(cowplot); library(viridis); library(RColorBrewer);
library(janitor); library(tidyverse); library(jcolors)

# iucn_colors <- c(
#   "Biological resource use" = "#228B22",          # Forest Green
#   "Climate change & severe weather" = "#E31A1C",   # Strong Red
#   "Energy production & mining" = "#FFD700",        # Gold
#   "Human intrusions & disturbance" = "#FDBF6F",    # Sandy Orange
#   "Invasive & other problematic species, genes & diseases" = "#6A3D9A", # Deep Purple
#   "Pollution" = "#FB9A99",                         # Chemical Pink
#   "Transportation & service corridors" = "#1F78B4" # Steel Blue
# )
iucn_colors <- c(
  "Biological resource use" = "#4E79A7",          # Muted Steel Blue
  "Climate change & severe weather" = "#E15759",   # Muted Terracotta
  "Energy production & mining" = "#F28E2B",        # Ochre
  "Human intrusions & disturbance" = "#EDC948",    # Soft Brass
  "Invasive & other problematic species, genes & diseases" = "#B07AA1", # Dusty Plum
  "Pollution" = "#76B7B2",                         # Muted Teal/Sage
  "Transportation & service corridors" = "#59A14F" # Muted Leaf Green
)

## load categories
full_df <- readRDS(here("results", "round_2_all_results.RDS")) %>%
  dplyr::select(category, hypothesis, code, iucn_name) %>%
  distinct()

## assp ####
assp_boot <- read_csv(here("results", "boot_results_by_species.csv")) %>%
  filter(species == "ASSP") %>%
  mutate(iucn_wrapped = str_wrap(iucn_name, width = 25))

med_x <- median(assp_boot$mean_cvoi, na.rm = TRUE)
med_y <- median(assp_boot$mean_red, na.rm = TRUE)

names(iucn_colors) <- str_wrap(names(iucn_colors), width = 25)

ggplot(assp_boot, aes(x = mean_cvoi, y = mean_red)) + 
  # geom_linerange(aes(xmin = cvoi_lci, xmax = cvoi_uci, color = iucn_name), 
  #                alpha = 0.5, linewidth = 0.8) +
  # geom_linerange(aes(ymin = red_lci, ymax = red_uci, color = iucn_name), 
  #                alpha = 0.5, linewidth = 0.8) +
  geom_linerange(aes(xmin = cvoi_lci, xmax = cvoi_uci, color = iucn_wrapped), 
                 alpha = 0.5, linewidth = 0.6) +
  geom_linerange(aes(ymin = red_lci, ymax = red_uci, color = iucn_wrapped), 
                 alpha = 0.5, linewidth = 0.6) +
  geom_point(aes(color = iucn_wrapped), size = 5, shape = 16) +
  geom_vline(xintercept = med_x, linetype = 'dashed', color = 'black', linewidth = 0.75) +
  geom_hline(yintercept = med_y, linetype = 'dashed', color = 'black', linewidth = 0.75) +
  geom_label_repel(aes(label = code, fill = iucn_wrapped), 
                   color = "white", fontface = "bold", size = 5,
                   max.overlaps = Inf, 
                   box.padding = 0.5,
                   segment.size = 0.3,
                   segment.color = "grey50",
                   show.legend = FALSE) +
  scale_color_manual(values = iucn_colors) +
  scale_fill_manual(values = iucn_colors) +
  theme_bw() +
  theme(legend.title = element_blank(),
        legend.position = 'bottom') +
  scale_y_continuous(name = "Reducibility", breaks = seq(-2, 5, 1), limits = c(-2, 4.5)) +
  scale_x_continuous(name = "Constructed value of information", breaks = seq(-2, 5, 1), limits = c(-2, 4.5)) +
  geom_text(x = -1.8, y = -1.8, label = "Low priority", color = "black", size = 6) +
  geom_text(x = -1.8, y = 4.3, label = "Medium priority", color = "black", size = 6) +
  geom_text(x = 4.3, y = -1.8, label = "High priority", color = "black", size = 6) +
  geom_text(x = 4.3, y = 4.3, label = "Highest priority", color = "black", size = 6) +
  ggtitle("Ashy Storm-Petrel") +
  theme(text = element_text(size = 18),
        plot.title = element_text(hjust = 0.5, face = "bold", size = 22),
        legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size = 16),
        legend.spacing.y = unit(0.5, 'cm'),       
        legend.key.height = unit(1.2, "cm"),      
        legend.key.width = unit(0.8, "cm"),        
        panel.grid.minor = element_blank()) +
  guides(color = guide_legend(byrow = TRUE, ncol = 1))

#ggsave(here("figures", "assp_cvoi.png"), width = 16, height = 8)