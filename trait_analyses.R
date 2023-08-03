library(googlesheets4)
library(tidyverse)
library(cowplot)
library(lme4)
library(lmerTest)
library(ggeffects)
library(effects)
library(emmeans)
library(multcomp)


# First, loading data:

data <- read_sheet("https://docs.google.com/spreadsheets/d/1jaxMxXIJAjN9hq9pF6PaG1_v6-3HE1K58bbUWnbNTIQ/edit#gid=413886912", sheet = "Trait Measurement", na = "NA") %>% 
  separate(id,into = c("population","rep"),remove=FALSE)

data2<- read_sheet("https://docs.google.com/spreadsheets/d/1jaxMxXIJAjN9hq9pF6PaG1_v6-3HE1K58bbUWnbNTIQ/edit#gid=413886912", sheet = "Trait measurement2", na = "NA") %>% 
  separate(id,into = c("population","rep"), remove = FALSE) %>% 
  mutate(measurement_date_2 = as.POSIXct(measurement_date_2, format = "%d-%m-%Y"))

germ <- read_sheet("https://docs.google.com/spreadsheets/d/1jaxMxXIJAjN9hq9pF6PaG1_v6-3HE1K58bbUWnbNTIQ/edit#gid=413886912", sheet = "germination data", na = "NA", "germination data!A1:G480" )

climate <- read_sheet("https://docs.google.com/spreadsheets/d/1v3_JhdpfEbMju2BVWWZYQzKhFTQ6arreWW5JFkD7064/edit#gid=0") %>% 
  dplyr::select(population, CMD_sm, CMD_sp)

all_data <- left_join(data, germ) %>% 
  left_join(.,data2) %>% 
  left_join(., climate) %>% 
  mutate(treatment_ord = factor(treatment, levels =c( "well watered", "drought level 1", "drought level 2", "drought level 3", "drought level 4"))) %>% 
  mutate(treatment_numeric = if_else(treatment == "well watered", 35, if_else(treatment == "drought level 1", 20, if_else(treatment == "drought level 4", 5, 10)))) %>% 
  mutate(mating_system = ifelse(species %in% c("C. rhomboidea", "C. quadrivulnera"), "selfing", "outcrossing")) %>% 
  filter(species != "C. dudleyana") %>% 
  # mutate(measurement_date_2 = as.POSIXct(measurement_date_2, format = "%d-%m-%Y"),
         # measurement_date_1 = as.POSIXct(measurement_date_1, format = "%d-%m-%Y")) %>% 
  mutate(age_measurement_1 = as.numeric(round(measurement_date_1 - germination_date, 0))) %>% 
  mutate(age_measurement_2 = as.numeric(round(measurement_date_2 - germination_date, 0))) %>% 
  mutate(overall_height_per_day = overall_height_2/age_measurement_2) %>% 
  mutate(num_leaves_per_day = num_leaves_over_2mm_2/age_measurement_2) 



# Data checking
summary(data)
table(data$species)
table(data$id )
table(data$treatment)
table(data$location)
summary(data$height_meristem_mm_1)


# Trait differences among species

ggplot(data = all_data) +
  geom_histogram(aes(x = height_meristem_mm_1)) +
  facet_wrap(.~species)

ggplot(data = all_data) +
  geom_histogram(aes(x = num_leaves_over_2mm_1)) +
  facet_wrap(.~species)

ggplot(data = all_data) +
  geom_histogram(aes(x = height_to_meristem_2)) +
  facet_wrap(.~species)

ggplot(data = all_data) +
  geom_histogram(aes(x = num_leaves_over_2mm_2)) +
  facet_wrap(.~species)

ggplot(data=all_data,aes(x=species,y=height_meristem_mm_1, fill = population))+
  geom_boxplot()
ggplot(data=all_data,aes(x=length_largest_leaf_1,y=height_meristem_mm_1, color=species, shape=population))+
  geom_point()
ggplot(data=all_data,aes(x=length_largest_leaf_1,y=height_meristem_mm_1, color=population, shape=species))+
  geom_point()+
  geom_smooth(method = "lm") +
  facet_wrap(.~species) +
  theme_cowplot() +
  labs(x = "Length in mm")
ggsave("figures/height_by_length_paneled.jpg", height = 5, width = 5)


m1 <- lmer(height_meristem_mm_1 ~ species + (1|population), data = all_data)
summary(m1)


m2 <- lmer(height_meristem_mm_1 ~ length_largest_leaf_1*species + (1|population), data = all_data)
summary(m2)

plot(ggeffect(m2, terms = c("length_largest_leaf_1", "species")))

# integrating germination date Measurement data 1 

ggplot(data=all_data,aes(y=germination_date,x=height_meristem_mm_1, fill=species))+
  geom_point() 

ggplot(data=all_data,aes(x=germination_date,y=length_largest_leaf_1, color=population))+
  geom_point()+
  geom_smooth(method = "lm") +
  facet_wrap(.~species) 

ggplot(data=all_data,aes(x=germination_date,y=height_meristem_mm_1, color=population))+
  geom_point()+
  geom_smooth(method = "lm") +
  facet_wrap(.~species) 

# Measurment Data 2 

ggplot(data=all_data,aes(x=treatment_ord,y=overall_height_2, color=population))+
  geom_boxplot()+
  facet_wrap(.~species, scales = "free_y")

ggplot(data=all_data,aes(x=treatment_ord,y=num_leaves_over_2mm_2, color=population))+
  geom_boxplot()+
  facet_wrap(.~species, scales = "free_y") 

ggplot(data=all_data,aes(x=treatment_ord,y=length_largest_leaf_2, fill=population))+
  geom_boxplot()+
  facet_wrap(.~species, scales = "free_y") 

# Can also try with treatment as a continuous predictor (mL water/week)
ggplot(data=all_data,aes(x=treatment_numeric,y=overall_height_2, color=population))+
  geom_smooth(method = "lm") +
  facet_wrap(.~species)

ggplot(data=all_data,aes(x=treatment_numeric,y=num_leaves_over_2mm_2, color=population))+
  geom_smooth(method = "lm") +
  facet_wrap(.~species)

ggplot(data=all_data,aes(x=treatment_numeric,y=num_leaves_over_2mm_2, color=population))+
  geom_smooth(method = "lm") +
  facet_wrap(.~species)



#integrating germination date Measurment data 2

ggplot(data=all_data,aes(y=germination_date,x=height_to_meristem_2, fill=species))+
  geom_point() 

ggplot(data=all_data,aes(x=germination_date,y=length_largest_leaf_2, color=population))+
  geom_point()+
  geom_smooth(method = "lm") +
  facet_wrap(.~species) 

ggplot(data=all_data,aes(x=germination_date,y=height_to_meristem_2, color=population))+
  geom_point()+
  geom_smooth(method = "lm") +
  facet_wrap(.~species) 


#/////////////////////////////////////////////////////////////////////////////////////////////////////
#ANOVAs

# Revamping this one to include the Tukey test letters

# Pulchella 

pulchella = all_data %>% 
  filter(species == "C. pulchella")

pul_leaf_size = lm(length_largest_leaf_2 ~ treatment_ord*population + length_largest_leaf_1, data = pulchella)
summary(pul_leaf_size)
# Interaction is not significant so we can drop it

pul_leaf_size = lm(length_largest_leaf_2 ~ treatment_ord+population, data = pulchella)
summary(pul_leaf_size)

# Calculate estimated marginal means with pairwise tukey contrasts.
mod_means_pul <- emmeans(object = pul_leaf_size,
                                  pairwise ~ "treatment_ord",
                                  adjust = "tukey")

# Then get the letters to go with the contrasts
letters_pul <- cld(object = mod_means_pul$emmeans,
                            Letters = letters)

# Plot these with raw data

ggplot() +
  geom_boxplot(data = pulchella, aes(x = treatment_ord, y = length_largest_leaf_2, fill = population)) +
  theme(axis.text.x = element_text(angle = 90)) + # Optionally rotate labels
  geom_text(data = letters_pul, aes(x = treatment_ord, y = emmean, label = gsub(" ", "", .group)),
            position = position_nudge(x = 0, y = 6)) # Can adjust these numbers to move letters relative to means in each group
# When showing this plot, you can note on the slide that there is also a significant difference between pops in size, but that there is no significant interaction, so responses to drought do not depend on population.



# Rhomboidea

rhomboidea = all_data %>% 
  filter(species == "C. rhomboidea")

rhom_leaf_size = lm(length_largest_leaf_2 ~ treatment_ord*population + length_largest_leaf_1, data = rhomboidea)
summary(rhom_leaf_size)

rhom_leaf_size = lm(length_largest_leaf_2 ~ treatment_ord+population, data = rhomboidea)
summary(rhom_leaf_size)

# Calculate estimated marginal means with pairwise tukey contrasts.
mod_means_rhom <- emmeans(object = rhom_leaf_size,
                         pairwise ~ "treatment_ord",
                         adjust = "tukey")

# Then get the letters to go with the contrasts
letters_rhom <- cld(object = mod_means_rhom$emmeans,
                   Letters = letters)

# Plot these with raw data

ggplot() +
  geom_boxplot(data = rhomboidea, aes(x = treatment_ord, y = length_largest_leaf_2, fill = population)) +
  theme(axis.text.x = element_text(angle = 90)) + # Optionally rotate labels
  geom_text(data = letters_rhom, aes(x = treatment_ord, y = emmean, label = gsub(" ", "", .group)),
            position = position_nudge(x = 0, y = 6)) # Can adjust these numbers to move letters relative to means in each group



# arcuata

arcuata = all_data %>% 
  filter(species == "C. arcuata")

arc_leaf_size = lm(length_largest_leaf_2 ~ treatment_ord, data = arcuata)
summary(arc_leaf_size)

# Calculate estimated marginal means with pairwise tukey contrasts.
mod_means_arc <- emmeans(object = arc_leaf_size,
                          pairwise ~ "treatment_ord",
                          adjust = "tukey")

# Then get the letters to go with the contrasts
letters_arc <- cld(object = mod_means_arc$emmeans,
                    Letters = letters)

# Plot these with raw data

ggplot() +
  geom_boxplot(data = arcuata, aes(x = treatment_ord, y = length_largest_leaf_2)) +
  theme(axis.text.x = element_text(angle = 90)) + # Optionally rotate labels
  geom_text(data = letters_arc, aes(x = treatment_ord, y = emmean, label = gsub(" ", "", .group)),
            position = position_nudge(x = 0.2, y = 6)) # Can adjust these numbers to move letters relative to means in each group




  #quadrivulnera

quadrivulnera = all_data %>% 
  filter(species == "C. quadrivulnera")

quad_leaf_size = lm(length_largest_leaf_2 ~ treatment, data = quadrivulnera)
summary(quad_leaf_size)

quad_leaf_size_aov = aov(length_largest_leaf_2 ~ treatment, data = quadrivulnera)
summary(quad_leaf_size_aov)

TukeyHSD((quad_leaf_size_aov))

  #purpurea

purpurea = all_data %>% 
  filter(species == "C. purpurea")

purp_leaf_size = lm(length_largest_leaf_2 ~ treatment, data = purpurea)
summary(purp_leaf_size)

purp_leaf_size_aov = aov(length_largest_leaf_2 ~ treatment, data = purpurea)
summary(purp_leaf_size_aov)

TukeyHSD((purp_leaf_size_aov))


#/////////////////////////////////////////////////////////////////////////////////

  # Flowering date 

ggplot(data=all_data,aes(x=treatment_ord,y=flowered_2, fill=population))+
   geom_bar()+
   facet_wrap(.~species, scales = "free_y")


flower_counts = all_data %>% 
  group_by(species, population, treatment_ord) %>% 
  filter(!is.na(flowered_2)) %>% 
  summarize(n = n(), num_flower = sum(flowered_2 == "Y")) %>% 
  mutate(prop_flowered = num_flower/n)%>%
filter(species != "C. rhomboidea", species != "C.amoena") 

ggplot(data=flower_counts,aes(x=treatment_ord,y=prop_flowered, fill=population))+
  geom_col(stat="identity")+
  facet_wrap(.~species,)

  
flower_date = all_data %>% 
filter(flowered_2 == "Y")%>%
  filter(species != "C. rhomboidea") 

ggplot(data=flower_date,aes(x=treatment_ord,y=length_largest_leaf_2, color=population))+
  geom_boxplot()+
facet_wrap(.~species, scales = "free_y")




##create plot that shows growth rate using both data sets and germination date 

ggplot(data=all_data,aes(x=treatment_ord,y=overall_height_per_day, color=population))+
  geom_boxplot()+
  facet_wrap(.~species, scales = "free_y")



 #//////////////////////////////////////////////////////////////////////////////
  
  
#Research Question: How do different species of Clarkia vary in their response to varying amounts of water in terms of growth rate, flowering time, and final size?

  ## Growth Rate vs. Watering Level

ggplot(data=all_data,aes(x=treatment_ord,y=overall_height_per_day, color=population))+
  geom_boxplot()+
  facet_wrap(.~species, scales = "free_y")

ggplot(data=all_data,aes(x=treatment_ord,y=num_leaves_per_day, color=population))+
  geom_boxplot()+
  facet_wrap(.~species, scales = "free_y")


  ## Final Size vs. Watering Level
  
  ## Flowering Time vs. Watering Level




#Questions below require climate data integration ////////////////////////////////


#Research Question: Can the growth rate, flowering time, and final size of Clarkia populations be accurately predicted based on the climatic data of their collection sites?

  ## Growth Rate vs. Climatic Variables (e.g., Temperature, Precipitation, Humidity)

  ## Flowering Time vs. Climatic Variables

  ## Final Size vs. Climatic Variables

#Research Question: Do certain Clarkia species exhibit higher plasticity or adaptive capacity in response to changes in water availability compared to others, and can this be attributed to their climatic origins?

  ##Watering Level vs. Performance Metric
    
    ##Integrate climate data: how well does climate data predict performance and plasticity? 

#Research Question: Can the observed variation in performance curves of Clarkia populations be explained by a combination of climatic variables rather than a single variable alone?

  ##Performance Metric vs. Combination of Climatic Variables

#Research Question: Are there any interactions or trade-offs between the measured metrics (growth rate, flowering time, final size) in response to different levels of water availability, and how do these interactions differ across Clarkia populations from various climatic sources?

  ##Watering Level vs. Interaction Term (e.g., Growth Rate x Flowering Time)
  
  

