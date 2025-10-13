## Relevance % Plots
## A DuVall ajduvall@uw.edu

## load libraries
library(here);library(ggplot2);library(ggrepel);library(ggstance);
library(scales); library(cowplot); library(viridis); library(RColorBrewer)

#full_df <- bind_rows(full_df, full_df_caau)
full_df <- readRDS(here("data", "full_df.rds"))

## expert opinion on relevance by species
rel <- full_df %>%
  filter(rel_a != 0) %>%
  select(category, hypothesis, code, name, expert, species, rel_a, rel_b, mag, red) %>%
  pivot_longer(7:10, names_to = "var", values_to = "val") %>%
  group_by(category, hypothesis, code, name, species, var) %>%
  filter(var == "rel_a") %>%
  reframe(Num_responses = n(),
            Per_responses = ifelse(species == "CAAU", ((Num_responses/11)*100), ((Num_responses/12)*100))) %>%
            #Per_responses = (Num_responses/12)*100) %>%
  distinct() %>% # duplicate rows created for some reason (line 18)
  unite(c(code, name), col = hypothesis_name, sep = ": ")

pal1 <- rev(viridis(6))
pal2<- viridis(6)

level_order <- c("H2: Recreation", "H3: Fishery Equipment", "H4: Overfishing",            
                 "H5: Native Predators", "H7: Seabird Colonization", "H8: Oil Platforms",          
                 "H9: Offshore Wind", "H10: Shipping Traffic", "H11: Artificial Light",       
                 "H12: Invasive Plants", "H13: Black Rats", "H14: New Invasive Species",   
                 "H15: Oil Spills", "H16: Residual DDT", "H17: Contaminants",          
                 "H18: Sea Level Rise", "H19: Marine Productivity", "H20: Sea Surface Temperature",
                 "H21: Precipitation", "H22: Ambient Temperature", "H23: Marine Heat Waves",     
                 "H24: Storm Events", "H25: Wind", "H26: Harmful Algal Blooms",  
                 "H27: Disease", "H28: Parasites")

ggplot(rel %>% filter(category == "Disturbance")) +
  geom_col(aes(x = hypothesis_name, y = Per_responses, 
               group = species, fill = species),
           #position = "dodge") +
           position = position_dodge2(preserve = "single")) +
  ylim(c(0,100)) + xlab("") + ylab("% of experts who think it is relevant") +
  ggtitle("Disturbance", subtitle = "Relevance of Hypotheses by Species") +
  scale_fill_discrete(type = pal1) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "bottom",
        text = element_text(size = 20))

ggsave(here("figures", "disturbance_relevance.jpg"), width = 12, height = 7)

ggplot(rel %>% filter(category == "Fisheries")) +
  geom_col(aes(x = hypothesis_name, y = Per_responses, group = species, fill = species),
           position = position_dodge2(preserve = "single")) +
  ylim(c(0,100)) + xlab("") + ylab("% of experts who think it is relevant") +
  ggtitle("Fisheries", subtitle = "Relevance of Hypotheses by Species") +
  scale_fill_discrete(type = pal1) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "bottom",
        text = element_text(size = 20))

ggsave(here("figures", "fisheries_relevance.jpg"), width = 12, height = 7)

ggplot(rel %>% filter(category == "Harmful native species interactions")) +
  geom_col(aes(x = hypothesis_name, y = Per_responses, group = species, fill = species),
           position = position_dodge2(preserve = "single")) +
  ylim(c(0,100)) + xlab("") + ylab("% of experts who think it is relevant") +
  ggtitle("Harmful native species interactions", subtitle = "Relevance of Hypotheses by Species") +
  scale_fill_discrete(type = pal1) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "bottom",
        text = element_text(size = 20))

ggsave(here("figures", "native_species_relevance.jpg"), width = 12, height = 7)

ggplot(rel %>% filter(category == "Human infrastructure")) +
  geom_col(aes(x = hypothesis_name, y = Per_responses, group = species, fill = species),
           position = position_dodge2(preserve = "single")) +
  ylim(c(0,100)) + xlab("") + ylab("% of experts who think it is relevant") +
  ggtitle("Human infrastructure", subtitle = "Relevance of Hypotheses by Species") +
  scale_fill_discrete(type = pal1) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "bottom",
        text = element_text(size = 20))

ggsave(here("figures", "human_infrastructure_relevance.jpg"), width = 12, height = 7)

ggplot(rel %>% filter(category == "Invasive species")) +
  geom_col(aes(x = hypothesis_name, y = Per_responses, group = species, fill = species),
           position = position_dodge2(preserve = "single")) +
  ylim(c(0,100)) + xlab("") + ylab("% of experts who think it is relevant") +
  ggtitle("Invasive species", subtitle = "Relevance of Hypotheses by Species") +
  scale_fill_discrete(type = pal1) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "bottom",
        text = element_text(size = 20))

ggsave(here("figures", "invasive_species_relevance.jpg"), width = 12, height = 7)

ggplot(rel %>% filter(category == "Pollution")) +
  geom_col(aes(x = hypothesis_name, y = Per_responses, group = species, fill = species),
           position = position_dodge2(preserve = "single")) +
  ylim(c(0,100)) + xlab("") + ylab("% of experts who think it is relevant") +
  ggtitle("Pollution", subtitle = "Relevance of Hypotheses by Species") +
  scale_fill_discrete(type = pal1) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "bottom",
        text = element_text(size = 20))

ggsave(here("figures", "pollution_relevance.jpg"), width = 12, height = 7)

ggplot(rel %>% filter(category == "Projected climate change impacts")) +
  geom_col(aes(x = hypothesis_name, y = Per_responses, group = species, fill = species),
           position = position_dodge2(preserve = "single")) +
  ylim(c(0,100)) + xlab("") + ylab("% of experts who think it is relevant") +
  ggtitle("Projected climate change impacts", subtitle = "Relevance of Hypotheses by Species") +
  scale_fill_discrete(type = pal1) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 0.9),
        legend.position = "bottom",
        text = element_text(size = 20))

ggsave(here("figures", "climate_change_relevance.jpg"), width = 12, height = 7)

ggplot(rel %>% filter(category == "Toxins/diseases/parasites")) +
  geom_col(aes(x = hypothesis_name, y = Per_responses, group = species, fill = species),
           position = position_dodge2(preserve = "single")) +
  ylim(c(0,100)) + xlab("") + ylab("% of experts who think it is relevant") +
  ggtitle("Toxins/diseases/parasites", subtitle = "Relevance of Hypotheses by Species") +
  scale_fill_discrete(type = pal1) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.title = element_blank(),
        #axis.text.x = element_text(angle = 90),
        legend.position = "bottom",
        text = element_text(size = 20))

ggsave(here("figures", "toxins-disease-parasites_relevance.jpg"), width = 12, height = 7)


## number of relevant threats by species
by_spp <- rel %>% 
  group_by(species) %>%
  summarize(n = n())

ggplot(rel) %>%
  