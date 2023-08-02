library(googlesheets4)
library(tidyverse)
library(cowplot)
library(lme4)
library(lmerTest)
library(ggeffects)
library(effects)


# First, loading data:


data <- read_sheet("https://docs.google.com/spreadsheets/d/1jaxMxXIJAjN9hq9pF6PaG1_v6-3HE1K58bbUWnbNTIQ/edit#gid=413886912", sheet = "Trait Measurement", na = "NA") %>% 
  separate(id,into = c("population","rep"),remove=FALSE)

data2<- read_sheet("https://docs.google.com/spreadsheets/d/1jaxMxXIJAjN9hq9pF6PaG1_v6-3HE1K58bbUWnbNTIQ/edit#gid=413886912", sheet = "Trait measurement2", na = "NA") %>% 
  separate(id,into = c("population","rep"),remove=FALSE)

germ <- read_sheet("https://docs.google.com/spreadsheets/d/1jaxMxXIJAjN9hq9pF6PaG1_v6-3HE1K58bbUWnbNTIQ/edit#gid=413886912", sheet = "germination data", na = "NA", "germination data!A1:G480" )


table(data2$measurement_date_2)
table(data2$length_largest_leaf_2)
hist(data2$length_largest_leaf_2)

all_data <- left_join(data, germ) %>% left_join(.,data2) %>% 
  mutate(treatment_ord = factor(treatment, levels =c( "well watered", "drought level 1", "drought level 2", "drought level 3", "drought level 4")))%>% 
  filter(species != "C. dudleyana") %>% 
  mutate(measurement_date_2 = as.POSIXct(measurement_date_2, format = "%d-%m-%Y"),
         measurement_date_1 = as.POSIXct(measurement_date_1, format = "%d-%m-%Y")) %>% 
  mutate(age_measurement_2 = as.numeric(round(measurement_date_2 - germination_date, 0))) %>% 
  mutate(overall_height_per_day = overall_height_2/age_measurement_2) %>% 
  mutate(num_leaves_per_day = num_leaves_over_2mm_2/age_measurement_2) 

# Come back to fixing measurement date 1 read-in 


# Data checking
summary(data)
table(data$species)
table(data$id )
table(data$treatment)
table(data$location)
summary(data$height_meristem_mm_1)
summary(data$'')

summary(data2)



# Trait differences among species

ggplot(data = data) +
  geom_histogram(aes(x = height_meristem_mm_1)) +
  facet_wrap(.~species)


ggplot(data = data) +
  geom_histogram(aes(x = num_leaves_over_2mm_1)) +
  facet_wrap(.~species)


ggplot(data=data,aes(x=species,y=height_meristem_mm_1, fill=population))+
  geom_boxplot()
ggplot(data=data,aes(x=length_largest_leaf_1,y=height_meristem_mm_1, color=species, shape=population))+
  geom_point()
ggplot(data=data,aes(x=length_largest_leaf_1,y=height_meristem_mm_1, color=population, shape=species))+
  geom_point()+
  geom_smooth(method = "lm") +
  facet_wrap(.~species) +
  theme_cowplot() +
  labs(x = "Length in mm")
ggsave("figures/height_by_length_paneled.jpg", height = 5, width = 5)


m1 <- lmer(height_meristem_mm_1 ~ species + (1|population), data = data)
summary(m1)


m2 <- lmer(height_meristem_mm_1 ~ length_largest_leaf_1*species + (1|population), data = data)
summary(m2)

plot(ggeffect(m2, terms = c("length_largest_leaf_1", "species")))

#integrating germination date Measurement data 1 

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

#Measurment Data 2 

ggplot(data=all_data,aes(x=treatment_ord,y=overall_height_2, color=population))+
  geom_boxplot()+
  facet_wrap(.~species, scales = "free_y")

ggplot(data=all_data,aes(x=treatment_ord,y=num_leaves_over_2mm_2, color=population))+
  geom_boxplot()+
  facet_wrap(.~species, scales = "free_y") 

ggplot(data=all_data,aes(x=treatment_ord,y=length_largest_leaf_2, fill=population))+
  geom_boxplot()+
  facet_wrap(.~species, scales = "free_y") +
  labs(x = "t")

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

  #Pulchella 
pulchella = all_data %>% 
  filter(species == "C. pulchella")

pul_leaf_size = lm(length_largest_leaf_2 ~ treatment*population + length_largest_leaf_1, data = pulchella)
summary(pul_leaf_size)

pul_leaf_size = lm(length_largest_leaf_2 ~ treatment+population, data = pulchella)
summary(pul_leaf_size)

pul_leaf_size_aov = aov(length_largest_leaf_2 ~ treatment+population, data = pulchella)
summary(pul_leaf_size_aov)

TukeyHSD((pul_leaf_size_aov))

  #Rhomboidea

rhomboidea = all_data %>% 
  filter(species == "C. rhomboidea")

rhom_leaf_size = lm(length_largest_leaf_2 ~ treatment*population + length_largest_leaf_1, data = rhomboidea)
summary(rhom_leaf_size)

rhom_leaf_size = lm(length_largest_leaf_2 ~ treatment+population, data = rhomboidea)
summary(rhom_leaf_size)

rhom_leaf_size_aov = aov(length_largest_leaf_2 ~ treatment+population, data = rhomboidea)
summary(rhom_leaf_size_aov)

TukeyHSD((rhom_leaf_size_aov))

  #arcuata

arcuata = all_data %>% 
  filter(species == "C. arcuata")

arc_leaf_size = lm(length_largest_leaf_2 ~ treatment, data = arcuata)
summary(arc_leaf_size)

arc_leaf_size_aov = aov(length_largest_leaf_2 ~ treatment, data = arcuata)
summary(arc_leaf_size_aov)

TukeyHSD((arc_leaf_size_aov))

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
  
  

