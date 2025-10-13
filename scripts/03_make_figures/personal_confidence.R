library(here)
library(tidyverse)
library(ggplot2)

full_df <- readRDS(here("data", "full_df.RDS"))

h_name <- c("Overflights", "Recreation", "Fishery Equipment", "Overfishing",            
            "Native Predators", "Pinnipeds", "Seabird Colonization", "Oil Platforms",          
            "Offshore Wind", "Shipping Traffic", "Artificial Light",       
            "Invasive Plants", "Black Rats", "New Invasive Species",   
            "Oil Spills", "Residual DDT", "Contaminants",          
            "Sea Level Rise", "Marine Productivity", "Sea Surface Temperature",
            "Precipitation", "Ambient Temperature", "Marine Heat Waves",     
            "Storm Events", "Wind", "Harmful Algal Blooms",  
            "Disease", "Parasites")



conf_by_spp <- full_df %>%
  group_by(species) %>%
  summarize(mean = mean(conf, na.rm = T))

conf_by_hypothesis <- full_df %>%
  group_by(hypothesis, name) %>%
  summarize(mean = mean(conf, na.rm = T))


ggplot(full_df, aes(x = name, y = conf, fill = category)) +
  scale_fill_brewer(palette = 'Set2') +
  geom_violin() +
  stat_summary(fun.y=mean, geom="point", fill = "black", size=2) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust=0.95, vjust=0.2),
        legend.position = "bottom",
        text = element_text(size = 20),
        legend.title = element_blank()) +
  scale_x_discrete(limits = c("Overflights", "Recreation", "Fishery Equipment", "Overfishing",            
                              "Native Predators", "Pinnipeds", "Seabird Colonization", "Oil Platforms",          
                              "Offshore Wind", "Shipping Traffic", "Artificial Light",       
                              "Invasive Plants", "Black Rats", "New Invasive Species",   
                              "Oil Spills", "Residual DDT", "Contaminants",          
                              "Sea Level Rise", "Marine Productivity", "Sea Surface Temperature",
                              "Precipitation", "Ambient Temperature", "Marine Heat Waves",     
                              "Storm Events", "Wind", "Harmful Algal Blooms",  
                              "Disease", "Parasites")) +
  xlab("Hypothesis") + ylab("Personal Confidence \n (Low to High)")

#ggsave(here("figures", "personal_conf_by_hypothesis.png"), width = 14, height = 8)  

full_df_fix <- full_df %>%
  mutate(species = ifelse(species == "SNPL", "WSPL", species))

ggplot(full_df_fix, aes(x = species, y = conf, fill = species)) +
  scale_fill_brewer(palette = 'Set3') +
  geom_violin() +
  stat_summary(fun.y=mean, geom="point", fill = "black", size=2) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, 
                                   hjust=0.95, vjust=0.2),
        legend.position = "none",
        text = element_text(size = 20),
        legend.title = element_blank()) +
  scale_x_discrete(limits = c("ASSP", "BRPE", "CAAU", "SCMU",
                              "WEGU", "WSPL")) +
  xlab("Species") + ylab("Personal Confidence \n (Low to High)")

ggsave(here("figures", "personal_conf_by_spp.png"), width = 10, height = 8)  


