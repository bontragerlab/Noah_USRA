library(googlesheets4)
library(tidyverse)
library(cowplot)
library(lme4)
library(lmerTest)
library(ggeffects)


# First, loading data:


data <- read_sheet("https://docs.google.com/spreadsheets/d/1jaxMxXIJAjN9hq9pF6PaG1_v6-3HE1K58bbUWnbNTIQ/edit#gid=413886912", sheet = "Trait Measurement", na = "NA") %>% 
  separate(id,into = c("population","rep"),remove=FALSE)

germ <- read_sheet("https://docs.google.com/spreadsheets/d/1jaxMxXIJAjN9hq9pF6PaG1_v6-3HE1K58bbUWnbNTIQ/edit#gid=413886912", sheet = "germination data", na = "NA", "Copy of thinning!A1:G480" )

all_data <- left_join(data, germ)

# Data checking
summary(data)
table(data$species)
table(data$id )
table(data$treatment)
table(data$location)
summary(data$height_meristem_mm)
summary(data$'')



# Trait differences among species

ggplot(data = data) +
  geom_histogram(aes(x = height_meristem_mm)) +
  facet_wrap(.~species)


ggplot(data = data) +
  geom_histogram(aes(x = `#_leaves_over_2mm`)) +
  facet_wrap(.~species)


ggplot(data=data,aes(x=species,y=height_meristem_mm, fill=population))+
  geom_boxplot()
ggplot(data=data,aes(x=length_largest_leaf_mm,y=height_meristem_mm, color=species, shape=population))+
  geom_point()
ggplot(data=data,aes(x=length_largest_leaf_mm,y=height_meristem_mm, color=population, shape=species))+
  geom_point()+
  geom_smooth(method = "lm") +
  facet_wrap(.~species) +
  theme_cowplot() +
  labs(x = "Length in mm")
ggsave("figures/height_by_length_paneled.jpg", height = 5, width = 5)


m1 <- lmer(height_meristem_mm ~ species + (1|population), data = data)
summary(m1)


m2 <- lmer(height_meristem_mm ~ length_largest_leaf_mm*species + (1|population), data = data)
summary(m2)

plot(ggeffect(m2, terms = c("length_largest_leaf_mm", "species")))

#integrating germination date 

ggplot(data=all_data,aes(y=germination_date,x=height_meristem_mm, fill=species))+
  geom_point()

ggplot(data=all_data,aes(x=germination_date,y=length_largest_leaf_mm, color=population))+
  geom_point()+
  geom_smooth(method = "lm") +
  facet_wrap(.~species) 

ggplot(data=all_data,aes(x=germination_date,y=height_meristem_mm, color=population))+
  geom_point()+
  geom_smooth(method = "lm") +
  facet_wrap(.~species) 
