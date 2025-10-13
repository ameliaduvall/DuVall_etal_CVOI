## Compare full versus linear confidence weighting

## load libraries
library(here);library(ggplot2);library(ggrepel);library(ggstance);
library(scales); library(cowplot); library(viridis); library(RColorBrewer);
library(janitor); library(tidyverse)

hypothesis_names <- c("H1: Overflights", "H2: Recreation", "H3: Fishery Equipment", "H4: Overfishing",            
                                     "H5: Native Predators", "H6: Pinnipeds", "H7: Seabird Colonization", "H8: Oil Platforms",          
                                     "H9: Offshore Wind", "H10: Shipping Traffic", "H11: Artificial Light",       
                                     "H12: Invasive Plants", "H13: Black Rats", "H14: New Invasive Species",   
                                     "H15: Oil Spills", "H16: Residual DDT", "H17: Contaminants",          
                                     "H18: Sea Level Rise", "H19: Marine Productivity", "H20: Sea Surface Temperature",
                                     "H21: Precipitation", "H22: Ambient Temperature", "H23: Marine Heat Waves",     
                                     "H24: Storm Events", "H25: Wind", "H26: Harmful Algal Blooms",  
                                     "H27: Disease", "H28: Parasites")
hypothesis_names2 <- c("Overflights", "Recreation", "Fishery Equipment", "Overfishing",            
                       "Native Predators", "Pinnipeds", "Seabird Colonization", "Oil Platforms",          
                       "Offshore Wind", "Shipping Traffic", "Artificial Light",       
                       "Invasive Plants", "Black Rats", "New Invasive Species",   
                       "Oil Spills", "Residual DDT", "Contaminants",          
                       "Sea Level Rise", "Marine Productivity", "Sea Surface Temperature",
                       "Precipitation", "Ambient Temperature", "Marine Heat Waves",     
                       "Storm Events", "Wind", "Harmful Algal Blooms",  
                       "Disease", "Parasites")

pal_col <- c('#009E73', '#56b4e9', '#f0e442', '#d55e00') # '#e69f00', 

## ASSP ####
assp1 <- readRDS(here('results', 'assp_sensitivity.rds')) %>%
  as.data.frame() %>%
  mutate(weight = c('full'))
assp2 <- readRDS(here('results', 'assp_linear_conf_sensitivity.rds')) %>%
  as.data.frame() %>%
  mutate(weight = c('linear'))
# assp3 <- readRDS(here('results', 'assp_exp_conf_sensitivity.rds')) %>%
#   as.data.frame() %>%
#   mutate(weight = c('exponential')) 
# highest, high, medium, low

assp_df <- bind_rows(assp1, assp2) %>%
  mutate(hypothesis = rep(1:28, 2),
         name = rep(hypothesis_names, 2)) %>%
  pivot_longer(cols = 1:4, names_to = 'priority', values_to = 'estimate') %>%
  mutate(priority = ifelse(priority == 'V1', 'highest',
                           ifelse(priority == 'V2', 'high',
                                  ifelse(priority == 'V3', 'medium', 'low'))))
assp_df$priority <- factor(assp_df$priority, levels = c("highest", "high", "medium", "low"),
                  labels = c("highest", "high", "medium", "low"))
assp_df$weight <- factor(assp_df$weight, levels = c("full", "linear", "exponential"),
                           labels = c("full", "linear", "exponential"))
assp_df$name <- factor(assp_df$name, levels = hypothesis_names,
                       labels = hypothesis_names2)

ggplot(assp_df) +
  geom_col(aes(x = weight, y = estimate, fill = priority)) +
  scale_fill_discrete(type = pal_col) +
  facet_wrap(~name, nrow = 7) +
  theme_minimal() +
  ggtitle("ASHY STORM-PETREL") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = 'bottom',
        legend.title = element_blank(),
        text = element_text(size = 40),
        plot.title = element_text(hjust = 0.5, face = "bold")) +
  xlab('Type of Confidence Weighting') + 
  ylab('% of samples in each quadrat') 

#ggsave(here('figures', 'final', 'assp_conf_wgts-test.jpg'), width = 30, height = 36)


## BRPE ####
brpe1 <- readRDS(here('results', 'brpe_sensitivity.rds')) %>%
  as.data.frame() %>%
  mutate(weight = c('full'))
brpe2 <- readRDS(here('results', 'brpe_linear_conf_sensitivity.rds')) %>%
  as.data.frame() %>%
  mutate(weight = c('linear'))
# brpe3 <- readRDS(here('results', 'brpe_exp_conf_sensitivity.rds')) %>%
#   as.data.frame() %>%
#   mutate(weight = c('exponential')) 
# highest, high, medium, low

brpe_df <- bind_rows(brpe1, brpe2) %>%
  mutate(hypothesis = rep(1:28, 2),
         name = rep(hypothesis_names, 2)) %>%
  pivot_longer(cols = 1:4, names_to = 'priority', values_to = 'estimate') %>%
  mutate(priority = ifelse(priority == 'V1', 'highest',
                           ifelse(priority == 'V2', 'high',
                                  ifelse(priority == 'V3', 'medium', 'low'))))
brpe_df$priority <- factor(brpe_df$priority, levels = c("highest", "high", "medium", "low"),
                           labels = c("highest", "high", "medium", "low"))
brpe_df$weight <- factor(brpe_df$weight, levels = c("full", "linear", "exponential"),
                         labels = c("full", "linear", "exponential"))
brpe_df$name <- factor(brpe_df$name, levels = hypothesis_names,
                       labels = hypothesis_names2)

ggplot(brpe_df) +
  geom_col(aes(x = weight, y = estimate, fill = priority)) +
  scale_fill_discrete(type = pal_col) +
  facet_wrap(~name, nrow = 4) +
  theme_minimal() +
  ggtitle("CALIFORNIA BROWN PELICAN") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = 'bottom',
        legend.title = element_blank(),
        text = element_text(size = 16),
        plot.title = element_text(hjust = 0.5, face = "bold")) +
  xlab('Type of Confidence Weighting') + 
  ylab('% of samples in each quadrat') 

ggsave(here('figures', 'final', 'brpe_conf_wgts.jpg'), width = 16, height = 12)

## CAAU ####
caau1 <- readRDS(here('results', 'caau_sensitivity.rds')) %>%
  as.data.frame() %>%
  mutate(weight = c('full'))
caau2 <- readRDS(here('results', 'caau_linear_conf_sensitivity.rds')) %>%
  as.data.frame() %>%
  mutate(weight = c('linear'))
# caau3 <- readRDS(here('results', 'caau_exp_conf_sensitivity.rds')) %>%
#   as.data.frame() %>%
#   mutate(weight = c('exponential')) 
# highest, high, medium, low

caau_df <- bind_rows(caau1, caau2) %>%
  mutate(hypothesis = rep(1:28, 2),
         name = rep(hypothesis_names, 2)) %>%
  pivot_longer(cols = 1:4, names_to = 'priority', values_to = 'estimate') %>%
  mutate(priority = ifelse(priority == 'V1', 'highest',
                           ifelse(priority == 'V2', 'high',
                                  ifelse(priority == 'V3', 'medium', 'low'))))
caau_df$priority <- factor(caau_df$priority, levels = c("highest", "high", "medium", "low"),
                           labels = c("highest", "high", "medium", "low"))
caau_df$weight <- factor(caau_df$weight, levels = c("full", "linear", "exponential"),
                         labels = c("full", "linear", "exponential"))
caau_df$name <- factor(caau_df$name, levels = hypothesis_names,
                       labels = hypothesis_names2)

ggplot(caau_df) +
  geom_col(aes(x = weight, y = estimate, fill = priority)) +
  scale_fill_discrete(type = pal_col) +
  facet_wrap(~name, nrow = 4) +
  theme_minimal() +
  ggtitle("CASSIN'S AUKLET") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = 'bottom',
        legend.title = element_blank(),
        text = element_text(size = 16),
        plot.title = element_text(hjust = 0.5, face = "bold")) +
  xlab('Type of Confidence Weighting') + 
  ylab('% of samples in each quadrat') 

ggsave(here('figures', 'final', 'caau_conf_wgts.jpg'), width = 16, height = 12)

## SCMU ####
scmu1 <- readRDS(here('results', 'scmu_sensitivity.rds')) %>%
  as.data.frame() %>%
  mutate(weight = c('full'))
scmu2 <- readRDS(here('results', 'scmu_linear_conf_sensitivity.rds')) %>%
  as.data.frame() %>%
  mutate(weight = c('linear'))
# scmu3 <- readRDS(here('results', 'scmu_exp_conf_sensitivity.rds')) %>%
#   as.data.frame() %>%
#   mutate(weight = c('exponential')) 
# highest, high, medium, low

scmu_df <- bind_rows(scmu1, scmu2) %>%
  mutate(hypothesis = rep(1:28, 2),
         name = rep(hypothesis_names, 2)) %>%
  pivot_longer(cols = 1:4, names_to = 'priority', values_to = 'estimate') %>%
  mutate(priority = ifelse(priority == 'V1', 'highest',
                           ifelse(priority == 'V2', 'high',
                                  ifelse(priority == 'V3', 'medium', 'low'))))
scmu_df$priority <- factor(scmu_df$priority, levels = c("highest", "high", "medium", "low"),
                           labels = c("highest", "high", "medium", "low"))
scmu_df$weight <- factor(scmu_df$weight, levels = c("full", "linear", "exponential"),
                         labels = c("full", "linear", "exponential"))
scmu_df$name <- factor(scmu_df$name, levels = hypothesis_names,
                       labels = hypothesis_names2)

ggplot(scmu_df) +
  geom_col(aes(x = weight, y = estimate, fill = priority)) +
  scale_fill_discrete(type = pal_col) +
  facet_wrap(~name, nrow = 4) +
  theme_minimal() +
  ggtitle("SCRIPPS'S MURRELET") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = 'bottom',
        legend.title = element_blank(),
        text = element_text(size = 16),
        plot.title = element_text(hjust = 0.5, face = "bold")) +
  xlab('Type of Confidence Weighting') + 
  ylab('% of samples in each quadrat') 

ggsave(here('figures', 'final', 'scmu_conf_wgts.jpg'), width = 16, height = 12)

## SNPL ####
snpl1 <- readRDS(here('results', 'snpl_sensitivity.rds')) %>%
  as.data.frame() %>%
  mutate(weight = c('full'))
snpl2 <- readRDS(here('results', 'snpl_linear_conf_sensitivity.rds')) %>%
  as.data.frame() %>%
  mutate(weight = c('linear'))
# snpl3 <- readRDS(here('results', 'snpl_exp_conf_sensitivity.rds')) %>%
#   as.data.frame() %>%
#   mutate(weight = c('exponential')) 
# highest, high, medium, low

snpl_df <- bind_rows(snpl1, snpl2) %>%
  mutate(hypothesis = rep(1:28, 2),
         name = rep(hypothesis_names, 2)) %>%
  pivot_longer(cols = 1:4, names_to = 'priority', values_to = 'estimate') %>%
  mutate(priority = ifelse(priority == 'V1', 'highest',
                           ifelse(priority == 'V2', 'high',
                                  ifelse(priority == 'V3', 'medium', 'low'))))
snpl_df$priority <- factor(snpl_df$priority, levels = c("highest", "high", "medium", "low"),
                           labels = c("highest", "high", "medium", "low"))
snpl_df$weight <- factor(snpl_df$weight, levels = c("full", "linear", "exponential"),
                         labels = c("full", "linear", "exponential"))
snpl_df$name <- factor(snpl_df$name, levels = hypothesis_names,
                       labels = hypothesis_names2)

ggplot(snpl_df) +
  geom_col(aes(x = weight, y = estimate, fill = priority)) +
  scale_fill_discrete(type = pal_col) +
  facet_wrap(~name, nrow = 4) +
  theme_minimal() +
  ggtitle("WESTERN SNOWY PLOVER") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = 'bottom',
        legend.title = element_blank(),
        text = element_text(size = 16),
        plot.title = element_text(hjust = 0.5, face = "bold")) +
  xlab('Type of Confidence Weighting') + 
  ylab('% of samples in each quadrat') 

ggsave(here('figures', 'final', 'snpl_conf_wgts.jpg'), width = 16, height = 12)

## WEGU ####
wegu1 <- readRDS(here('results', 'wegu_sensitivity.rds')) %>%
  as.data.frame() %>%
  mutate(weight = c('full'))
wegu2 <- readRDS(here('results', 'wegu_linear_conf_sensitivity.rds')) %>%
  as.data.frame() %>%
  mutate(weight = c('linear'))
# wegu3 <- readRDS(here('results', 'wegu_exp_conf_sensitivity.rds')) %>%
#   as.data.frame() %>%
#   mutate(weight = c('exponential')) 
# highest, high, medium, low

wegu_df <- bind_rows(wegu1, wegu2) %>%
  mutate(hypothesis = rep(1:28, 2),
         name = rep(hypothesis_names, 2)) %>%
  pivot_longer(cols = 1:4, names_to = 'priority', values_to = 'estimate') %>%
  mutate(priority = ifelse(priority == 'V1', 'highest',
                           ifelse(priority == 'V2', 'high',
                                  ifelse(priority == 'V3', 'medium', 'low')))) %>%
  mutate(estimate = estimate*100)
wegu_df$priority <- factor(wegu_df$priority, levels = c("highest", "high", "medium", "low"),
                           labels = c("highest", "high", "medium", "low"))
wegu_df$weight <- factor(wegu_df$weight, levels = c("full", "linear"),
                         labels = c("full", "linear"))
wegu_df$name <- factor(wegu_df$name, levels = hypothesis_names,
                       labels = hypothesis_names2)

ggplot(wegu_df) +
  geom_col(aes(x = weight, y = estimate, fill = priority)) +
  scale_fill_discrete(type = pal_col) +
  facet_wrap(~name, nrow = 4) +
  theme_minimal() +
  ggtitle("WESTERN GULL") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1),
        legend.position = 'bottom',
        legend.title = element_blank(),
        text = element_text(size = 16),
        plot.title = element_text(hjust = 0.5, face = "bold")) +
  xlab('Type of Confidence Weighting') + 
  ylab('% of samples in each quadrat') 


ggsave(here('figures', 'final', 'wegu_conf_wgts.jpg'), width = 16, height = 12)

