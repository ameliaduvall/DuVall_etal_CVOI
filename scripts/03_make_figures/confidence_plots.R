## Confidence sensitivity analysis

## expert opinion on confidence
avg_conf <- full_df %>%
  select(category, hypothesis, code, name, expert, species, rel_a, rel_b, mag, red, conf) %>%
  pivot_longer(7:11, names_to = "var", values_to = "val") %>%
  group_by(category, hypothesis, code, name, species, var) %>%
  filter(var == "conf") %>%
  summarize(Per_responses = mean(val, na.rm = T)) %>%
  unite(c(code, name), col = hypothesis_name, sep = ": ")

pal1 <- rev(viridis(6))
pal2<- viridis(6)

## missing pinnipeds below?

level_order <- c("H2: Recreation", "H3: Fishery Equipment", "H4: Overfishing",            
                 "H5: Native Predators", "H7: Seabird Colonization", "H8: Oil Platforms",          
                 "H9: Offshore Wind", "H10: Shipping Traffic", "H11: Artificial Light",       
                 "H12: Invasive Plants", "H13: Black Rats", "H14: New Invasive Species",   
                 "H15: Oil Spills", "H16: Residual DDT", "H17: Contaminants",          
                 "H18: Sea Level Rise", "H19: Marine Productivity", "H20: Sea Surface Temperature",
                 "H21: Precipitation", "H22: Ambient Temperature", "H23: Marine Heat Waves",     
                 "H24: Storm Events", "H25: Wind", "H26: Harmful Algal Blooms",  
                 "H27: Disease", "H28: Parasites")

ggplot(avg_conf %>% filter(category == "Disturbance")) +
  geom_col(aes(x = hypothesis_name, y = Per_responses, 
               group = species, fill = species),
           #position = "dodge") +
           position = position_dodge2(preserve = "single")) +
  ylim(c(0,2)) + xlab("") + ylab("Mean expert confidence") +
  ggtitle("Disturbance") +
  scale_fill_discrete(type = pal1) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "bottom",
        text = element_text(size = 20))

ggsave(here("figures", "disturbance_confidence.jpg"), width = 12, height = 7)

ggplot(avg_conf %>% filter(category == "Fisheries")) +
  geom_col(aes(x = hypothesis_name, y = Per_responses, 
               group = species, fill = species),
           #position = "dodge") +
           position = position_dodge2(preserve = "single")) +
  ylim(c(0,2)) + xlab("") + ylab("Mean expert confidence") +
  ggtitle("Fisheries") +
  scale_fill_discrete(type = pal1) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "bottom",
        text = element_text(size = 20))

ggsave(here("figures", "fisheries_confidence.jpg"), width = 12, height = 7)

ggplot(avg_conf %>% filter(category == "Harmful native species interactions")) +
  geom_col(aes(x = hypothesis_name, y = Per_responses, 
               group = species, fill = species),
           #position = "dodge") +
           position = position_dodge2(preserve = "single")) +
  ylim(c(0,2)) + xlab("") + ylab("Mean expert confidence") +
  ggtitle("Harmful native species interactions") +
  scale_fill_discrete(type = pal1) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "bottom",
        text = element_text(size = 20))

ggsave(here("figures", "native_species_confidence.jpg"), width = 12, height = 7)

ggplot(avg_conf %>% filter(category == "Human infrastructure")) +
  geom_col(aes(x = hypothesis_name, y = Per_responses, 
               group = species, fill = species),
           #position = "dodge") +
           position = position_dodge2(preserve = "single")) +
  ylim(c(0,2)) + xlab("") + ylab("Mean expert confidence") +
  ggtitle("Human infrastructure") +
  scale_fill_discrete(type = pal1) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "bottom",
        text = element_text(size = 20))

ggsave(here("figures", "human_infrastructure_confidence.jpg"), width = 12, height = 7)

ggplot(avg_conf %>% filter(category == "Invasive species")) +
  geom_col(aes(x = hypothesis_name, y = Per_responses, 
               group = species, fill = species),
           #position = "dodge") +
           position = position_dodge2(preserve = "single")) +
  ylim(c(0,2)) + xlab("") + ylab("Mean expert confidence") +
  ggtitle("Invasive species") +
  scale_fill_discrete(type = pal1) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "bottom",
        text = element_text(size = 20))

ggsave(here("figures", "invasive_species_confidence.jpg"), width = 12, height = 7)

ggplot(avg_conf %>% filter(category == "Pollution")) +
  geom_col(aes(x = hypothesis_name, y = Per_responses, 
               group = species, fill = species),
           #position = "dodge") +
           position = position_dodge2(preserve = "single")) +
  ylim(c(0,2)) + xlab("") + ylab("Mean expert confidence") +
  ggtitle("Pollution") +
  scale_fill_discrete(type = pal1) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "bottom",
        text = element_text(size = 20))

ggsave(here("figures", "pollution_confidence.jpg"), width = 12, height = 7)

ggplot(avg_conf %>% filter(category == "Projected climate change impacts")) +
  geom_col(aes(x = hypothesis_name, y = Per_responses, 
               group = species, fill = species),
           #position = "dodge") +
           position = position_dodge2(preserve = "single")) +
  ylim(c(0,2)) + xlab("") + ylab("Mean expert confidence") +
  ggtitle("Projected climate change impacts") +
  scale_fill_discrete(type = pal1) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 0.9),
        legend.position = "bottom",
        text = element_text(size = 20))

ggsave(here("figures", "climate_change_confidence.jpg"), width = 12, height = 7)

ggplot(avg_conf %>% filter(category == "Toxins/diseases/parasites")) +
  geom_col(aes(x = hypothesis_name, y = Per_responses, 
               group = species, fill = species),
           #position = "dodge") +
           position = position_dodge2(preserve = "single")) +
  ylim(c(0,2)) + xlab("") + ylab("Mean expert confidence") +
  ggtitle("Toxins/diseases/parasites") +
  scale_fill_discrete(type = pal1) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "bottom",
        text = element_text(size = 20))

ggsave(here("figures", "toxins-disease-parasites_confidence.jpg"), width = 12, height = 7)
=======
## Confidence sensitivity analysis

## expert opinion on confidence
avg_conf <- full_df %>%
  select(category, hypothesis, code, name, expert, species, rel_a, rel_b, mag, red, conf) %>%
  pivot_longer(7:11, names_to = "var", values_to = "val") %>%
  group_by(category, hypothesis, code, name, species, var) %>%
  filter(var == "conf") %>%
  summarize(Per_responses = mean(val, na.rm = T)) %>%
  unite(c(code, name), col = hypothesis_name, sep = ": ")

pal1 <- rev(viridis(6))
pal2<- viridis(6)

## missing pinnipeds below?

level_order <- c("H2: Recreation", "H3: Fishery Equipment", "H4: Overfishing",            
                 "H5: Native Predators", "H7: Seabird Colonization", "H8: Oil Platforms",          
                 "H9: Offshore Wind", "H10: Shipping Traffic", "H11: Artificial Light",       
                 "H12: Invasive Plants", "H13: Black Rats", "H14: New Invasive Species",   
                 "H15: Oil Spills", "H16: Residual DDT", "H17: Contaminants",          
                 "H18: Sea Level Rise", "H19: Marine Productivity", "H20: Sea Surface Temperature",
                 "H21: Precipitation", "H22: Ambient Temperature", "H23: Marine Heat Waves",     
                 "H24: Storm Events", "H25: Wind", "H26: Harmful Algal Blooms",  
                 "H27: Disease", "H28: Parasites")

ggplot(avg_conf %>% filter(category == "Disturbance")) +
  geom_col(aes(x = hypothesis_name, y = Per_responses, 
               group = species, fill = species),
           #position = "dodge") +
           position = position_dodge2(preserve = "single")) +
  ylim(c(0,2)) + xlab("") + ylab("Mean expert confidence") +
  ggtitle("Disturbance") +
  scale_fill_discrete(type = pal1) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "bottom",
        text = element_text(size = 20))

ggsave(here("figures", "disturbance_confidence.jpg"), width = 12, height = 7)

ggplot(avg_conf %>% filter(category == "Fisheries")) +
  geom_col(aes(x = hypothesis_name, y = Per_responses, 
               group = species, fill = species),
           #position = "dodge") +
           position = position_dodge2(preserve = "single")) +
  ylim(c(0,2)) + xlab("") + ylab("Mean expert confidence") +
  ggtitle("Fisheries") +
  scale_fill_discrete(type = pal1) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "bottom",
        text = element_text(size = 20))

ggsave(here("figures", "fisheries_confidence.jpg"), width = 12, height = 7)

ggplot(avg_conf %>% filter(category == "Harmful native species interactions")) +
  geom_col(aes(x = hypothesis_name, y = Per_responses, 
               group = species, fill = species),
           #position = "dodge") +
           position = position_dodge2(preserve = "single")) +
  ylim(c(0,2)) + xlab("") + ylab("Mean expert confidence") +
  ggtitle("Harmful native species interactions") +
  scale_fill_discrete(type = pal1) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "bottom",
        text = element_text(size = 20))

ggsave(here("figures", "native_species_confidence.jpg"), width = 12, height = 7)

ggplot(avg_conf %>% filter(category == "Human infrastructure")) +
  geom_col(aes(x = hypothesis_name, y = Per_responses, 
               group = species, fill = species),
           #position = "dodge") +
           position = position_dodge2(preserve = "single")) +
  ylim(c(0,2)) + xlab("") + ylab("Mean expert confidence") +
  ggtitle("Human infrastructure") +
  scale_fill_discrete(type = pal1) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "bottom",
        text = element_text(size = 20))

ggsave(here("figures", "human_infrastructure_confidence.jpg"), width = 12, height = 7)

ggplot(avg_conf %>% filter(category == "Invasive species")) +
  geom_col(aes(x = hypothesis_name, y = Per_responses, 
               group = species, fill = species),
           #position = "dodge") +
           position = position_dodge2(preserve = "single")) +
  ylim(c(0,2)) + xlab("") + ylab("Mean expert confidence") +
  ggtitle("Invasive species") +
  scale_fill_discrete(type = pal1) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "bottom",
        text = element_text(size = 20))

ggsave(here("figures", "invasive_species_confidence.jpg"), width = 12, height = 7)

ggplot(avg_conf %>% filter(category == "Pollution")) +
  geom_col(aes(x = hypothesis_name, y = Per_responses, 
               group = species, fill = species),
           #position = "dodge") +
           position = position_dodge2(preserve = "single")) +
  ylim(c(0,2)) + xlab("") + ylab("Mean expert confidence") +
  ggtitle("Pollution") +
  scale_fill_discrete(type = pal1) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "bottom",
        text = element_text(size = 20))

ggsave(here("figures", "pollution_confidence.jpg"), width = 12, height = 7)

ggplot(avg_conf %>% filter(category == "Projected climate change impacts")) +
  geom_col(aes(x = hypothesis_name, y = Per_responses, 
               group = species, fill = species),
           #position = "dodge") +
           position = position_dodge2(preserve = "single")) +
  ylim(c(0,2)) + xlab("") + ylab("Mean expert confidence") +
  ggtitle("Projected climate change impacts") +
  scale_fill_discrete(type = pal1) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.title = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 0.9),
        legend.position = "bottom",
        text = element_text(size = 20))

ggsave(here("figures", "climate_change_confidence.jpg"), width = 12, height = 7)

ggplot(avg_conf %>% filter(category == "Toxins/diseases/parasites")) +
  geom_col(aes(x = hypothesis_name, y = Per_responses, 
               group = species, fill = species),
           #position = "dodge") +
           position = position_dodge2(preserve = "single")) +
  ylim(c(0,2)) + xlab("") + ylab("Mean expert confidence") +
  ggtitle("Toxins/diseases/parasites") +
  scale_fill_discrete(type = pal1) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5),
        legend.title = element_blank(),
        legend.position = "bottom",
        text = element_text(size = 20))

ggsave(here("figures", "toxins-disease-parasites_confidence.jpg"), width = 12, height = 7)
