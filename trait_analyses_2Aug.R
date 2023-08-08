library(googlesheets4)
library(tidyverse)
library(cowplot)
library(lme4)
library(lmerTest)
library(ggeffects)
library(effects)
library(emmeans)
library(multcomp)


# First, loading data:-------------------------

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



# Data checking----------------
summary(data)
table(data$species)
table(data$id )
table(data$treatment)
table(data$location)
summary(data$height_meristem_mm_1)


# Trait differences among species-------------

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

# integrating germination date Measurement data 1 --------------------

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

# Measurment Data 2 -------------------

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

#Histogram of germination--------------------------------------
data=all_data%>%
  filter(species!="C.amoena")
ggplot(data =data) +
  geom_histogram(aes(x = germination_date)) +
  facet_wrap(.~species)+
  theme_cowplot()+
  labs(x = "Germination date", y="Count")
ggsave("Germination_Hist_panelled_5.jpg", height = 5, width = 7)

# Pulchella plots ------------------

  ##Pul_LL_TK----------------
pulchella = all_data %>% 
  filter(species == "C. pulchella")

pul_leaf_size = lm(length_largest_leaf_2 ~ treatment_ord*population + length_largest_leaf_1, data = pulchella)
summary(pul_leaf_size)

pulchella = all_data %>% 
  filter(species == "C. pulchella")

pul_leaf_size= lm(length_largest_leaf_2 ~ treatment_ord + length_largest_leaf_1, data = pulchella)
summary(pul_leaf_size)

mod_means_pul <- emmeans(object = pul_leaf_size,
                                  pairwise ~ "treatment_ord",
                                  adjust = "tukey")

letters_pul <- cld(object = mod_means_pul$emmeans,
                            Letters = letters)

ggplot() +
  geom_boxplot(data = pulchella, aes(x = treatment_ord, y = length_largest_leaf_2, fill= population)) +
  theme_cowplot()+
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(.~population)+# Optionally rotate labels
  geom_text(data = letters_pul, aes(x = treatment_ord, y = emmean, label = gsub(" ", "", .group)),
            position = position_nudge(x = 0.2, y = 6))+
  labs(x = "Watering treatment", y="Length of largest leaf (mm)")
ggsave("Pul_LL_TK.jpg", height = 5, width = 7)


  

  ##Pul_OH_TK-------------------------------------

pulchella = all_data %>% 
  filter(species == "C. pulchella")

pul_overall_height = lm(overall_height_2 ~ treatment_ord*population + height_meristem_mm_1, data = pulchella)
summary(pul_overall_height)

pulchella = all_data %>% 
  filter(species == "C. pulchella")

pul_overall_height= lm(overall_height_2 ~ treatment_ord + height_meristem_mm_1, data = pulchella)
summary(pul_overall_height)

mod_means_pul <- emmeans(object = pul_overall_height,
                         pairwise ~ "treatment_ord",
                         adjust = "tukey")

letters_pul <- cld(object = mod_means_pul$emmeans,
                   Letters = letters)

ggplot() +
  geom_boxplot(data = pulchella, aes(x = treatment_ord, y = overall_height_2, fill= population)) +
  theme_cowplot()+
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(.~population)+# Optionally rotate labels
  geom_text(data = letters_pul, aes(x = treatment_ord, y = emmean, label = gsub(" ", "", .group)),
            position = position_nudge(x = 0.2, y = 16))+
  labs(x = "Watering treatment", y="Overall height (mm)")
ggsave("Pul_OH_TK.jpg", height = 5, width = 7)

##Pul_NL_TK-------------------------------------

pulchella = all_data %>% 
  filter(species == "C. pulchella")

pul_num_leaves = lm(num_leaves_over_2mm_2 ~ treatment_ord*population + num_leaves_over_2mm_1, data = pulchella)
summary(pul_overall_height)

pulchella = all_data %>% 
  filter(species == "C. pulchella")

pul_num_leaves= lm(num_leaves_over_2mm_2 ~ treatment_ord + num_leaves_over_2mm_1, data = pulchella)
summary(pul_num_leaves)

mod_means_pul <- emmeans(object = pul_num_leaves,
                         pairwise ~ "treatment_ord",
                         adjust = "tukey")

letters_pul <- cld(object = mod_means_pul$emmeans,
                   Letters = letters)

ggplot() +
  geom_boxplot(data = pulchella, aes(x = treatment_ord, y = num_leaves_over_2mm_2, fill= population)) +
  theme_cowplot()+
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(.~population)+# Optionally rotate labels
  geom_text(data = letters_pul, aes(x = treatment_ord, y = emmean, label = gsub(" ", "", .group)),
            position = position_nudge(x = 0.2, y = 7))+
  labs(x = "Watering treatment", y="Number of leaves over 2mm)")
ggsave("Pul_NL_TK.jpg", height = 5, width = 7)

# Rhomboidea plots ------------------

  ##Rho_LL_TK----------------
rhomboidea = all_data %>% 
  filter(species == "C. rhomboidea")

rho_leaf_size = lm(length_largest_leaf_2 ~ treatment_ord*population + length_largest_leaf_1, data = rhomboidea)
summary(rho_leaf_size)

rhomboidea = all_data %>% 
  filter(species == "C. rhomboidea")

rho_leaf_size= lm(length_largest_leaf_2 ~ treatment_ord + length_largest_leaf_1, data = rhomboidea)
summary(rho_leaf_size)

mod_means_rho <- emmeans(object = rho_leaf_size,
                                  pairwise ~ "treatment_ord",
                                  adjust = "tukey")

letters_rho <- cld(object = mod_means_rho$emmeans,
                            Letters = letters)

ggplot() +
  geom_boxplot(data = rhomboidea, aes(x = treatment_ord, y = length_largest_leaf_2, fill= population)) +
  theme_cowplot()+
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(.~population)+# Optionally rotate labels
  geom_text(data = letters_pul, aes(x = treatment_ord, y = emmean, label = gsub(" ", "", .group)),
            position = position_nudge(x = 0.2, y = 4))+
  labs(x = "Watering treatment", y="Length of largest leaf (mm)")
ggsave("Rho_LL_TK.jpg", height = 5, width = 7)


  

  ##Rho_OH_TK-------------------------------------

rhomboidea = all_data %>% 
  filter(species == "C. rhomboidea")

rho_OH = lm(overall_height_2 ~ treatment_ord*population + height_meristem_mm_1, data = rhomboidea)
summary(rho_OH)

rhomboidea = all_data %>% 
  filter(species == "C. rhomboidea")

rho_OH= lm(overall_height_2 ~ treatment_ord + height_meristem_mm_1, data = rhomboidea)
summary(rho_OH)

mod_means_rho <- emmeans(object = rho_OH,
                         pairwise ~ "treatment_ord",
                         adjust = "tukey")

letters_rho <- cld(object = mod_means_rho$emmeans,
                   Letters = letters)

ggplot() +
  geom_boxplot(data = rhomboidea, aes(x = treatment_ord, y = overall_height_2, fill= population)) +
  theme_cowplot()+
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(.~population)+# Optionally rotate labels
  geom_text(data = letters_pul, aes(x = treatment_ord, y = emmean, label = gsub(" ", "", .group)),
            position = position_nudge(x = 0.2, y = 7))+
  labs(x = "Watering treatment", y="Overall height (mm)")
ggsave("Rho_OH_TK.jpg", height = 5, width = 7)

##Rho_NL_TK-------------------------------------

rhomboidea = all_data %>% 
  filter(species == "C. rhomboidea")

rho_NL = lm(num_leaves_over_2mm_2 ~ treatment_ord*population + num_leaves_over_2mm_1, data = rhomboidea)
summary(rho_NL)

rhomboidea = all_data %>% 
  filter(species == "C. rhomboidea")

rho_OH= lm(num_leaves_over_2mm_2 ~ treatment_ord + num_leaves_over_2mm_1, data = rhomboidea)
summary(rho_NL)

mod_means_rho <- emmeans(object = rho_OH,
                         pairwise ~ "treatment_ord",
                         adjust = "tukey")

letters_rho <- cld(object = mod_means_rho$emmeans,
                   Letters = letters)

ggplot() +
  geom_boxplot(data = rhomboidea, aes(x = treatment_ord, y = num_leaves_over_2mm_2, fill= population)) +
  theme_cowplot()+
  theme(axis.text.x = element_text(angle = 90)) +
  facet_wrap(.~population)+# Optionally rotate labels
  geom_text(data = letters_pul, aes(x = treatment_ord, y = emmean, label = gsub(" ", "", .group)),
            position = position_nudge(x = 0.2, y = 1))+
  labs(x = "Watering treatment", y="Number of leaves over 2mm)")
ggsave("Rho_NL_TK.jpg", height = 5, width = 7)


# Arcuata Plots--------------------------------------------
  
  ##Arc_LL_TK-------------------------------------
arcuata = all_data %>% 
  filter(species == "C. arcuata")

arc_leaf_size = lm(length_largest_leaf_2 ~ treatment_ord, data = arcuata)
summary(arc_leaf_size)

mod_means_arc <- emmeans(object = arc_leaf_size,
                          pairwise ~ "treatment_ord",
                          adjust = "tukey")

letters_arc <- cld(object = mod_means_arc$emmeans,
                    Letters = letters)

ggplot() +
  geom_boxplot(data = arcuata, aes(x = treatment_ord, y = length_largest_leaf_2)) +
  theme_cowplot()+
  theme(axis.text.x = element_text(angle = 90)) +# Optionally rotate labels
  geom_text(data = letters_arc, aes(x = treatment_ord, y = emmean, label = gsub(" ", "", .group)),
            position = position_nudge(x = 0.2, y = 6))+
  labs(x = "Watering treatment", y="Length of largest leaf (mm)")
ggsave("Arc_LL_TK.jpg", height = 5, width = 7)
  

##Arc_OH_TK-------------------------------------
arcuata = all_data %>% 
  filter(species == "C. arcuata")

arc_overall_height = lm(overall_height_2 ~ treatment_ord, data = arcuata)
summary(arc_overall_height)

mod_means_arc <- emmeans(object = arc_overall_height,
                         pairwise ~ "treatment_ord",
                         adjust = "tukey")

letters_arc <- cld(object = mod_means_arc$emmeans,
                   Letters = letters)

ggplot() +
  geom_boxplot(data = arcuata, aes(x = treatment_ord, y = overall_height_2)) +
  theme_cowplot()+
  theme(axis.text.x = element_text(angle = 90)) +# Optionally rotate labels
  geom_text(data = letters_arc, aes(x = treatment_ord, y = emmean, label = gsub(" ", "", .group)),
            position = position_nudge(x = 0.2, y = 60))+
  labs(x = "Watering treatment", y="Overall height (mm)")
ggsave("Arc_OH_TK.jpg", height = 5, width = 7)

##Arc_NL_TK-------------------------------------
arcuata = all_data %>% 
  filter(species == "C. arcuata")

arc_num_leaves = lm(num_leaves_over_2mm_2 ~ treatment_ord, data = arcuata)
summary(arc_num_leaves)

mod_means_arc <- emmeans(object = arc_num_leaves,
                         pairwise ~ "treatment_ord",
                         adjust = "tukey")

letters_arc <- cld(object = mod_means_arc$emmeans,
                   Letters = letters)

ggplot() +
  geom_boxplot(data = arcuata, aes(x = treatment_ord, y = num_leaves_over_2mm_2)) +
  theme_cowplot()+
  theme(axis.text.x = element_text(angle = 90)) +# Optionally rotate labels
  geom_text(data = letters_arc, aes(x = treatment_ord, y = emmean, label = gsub(" ", "", .group)),
            position = position_nudge(x = 0.2, y = 2.5))+
  labs(x = "Watering treatment", y="Number of leaves over 2mm")
ggsave("Arc_NL_TK.jpg", height = 5, width = 7)


#Quadrivulnera Plots--------------------------------------------------
  
  ##Quad_LL_TK---------------------------------------
quadrivulnera = all_data %>% 
  filter(species == "C. quadrivulnera")

quad_leaf_size = lm(length_largest_leaf_2 ~ treatment_ord, data = quadrivulnera)
summary(quad_leaf_size)

mod_means_quad <- emmeans(object = quad_leaf_size,
                         pairwise ~ "treatment_ord",
                         adjust = "tukey")

letters_quad <- cld(object = mod_means_quad$emmeans,
                   Letters = letters)

ggplot() +
  geom_boxplot(data = quadrivulnera, aes(x = treatment_ord, y = length_largest_leaf_2)) +
  theme_cowplot()+
  theme(axis.text.x = element_text(angle = 90)) + # Optionally rotate labels
  geom_text(data = letters_quad, aes(x = treatment_ord, y = emmean, label = gsub(" ", "", .group)),
            position = position_nudge(x = 0.1, y = 7))+
  labs(x = "Watering treatment", y="Length of largest leaf (mm)")
ggsave("Quad_LL_TK.jpg", height = 5, width = 7)

##Quad_OH_TK---------------------------------------
quadrivulnera = all_data %>% 
  filter(species == "C. quadrivulnera")

quad_overall_height = lm(overall_height_2 ~ treatment_ord, data = quadrivulnera)
summary(quad_overall_height)

mod_means_quad <- emmeans(object = quad_overall_height,
                          pairwise ~ "treatment_ord",
                          adjust = "tukey")

letters_quad <- cld(object = mod_means_quad$emmeans,
                    Letters = letters)

ggplot() +
  geom_boxplot(data = quadrivulnera, aes(x = treatment_ord, y = overall_height_2)) +
  theme_cowplot()+
  theme(axis.text.x = element_text(angle = 90)) + # Optionally rotate labels
  geom_text(data = letters_quad, aes(x = treatment_ord, y = emmean, label = gsub(" ", "", .group)),
            position = position_nudge(x = 0.1, y = 112))+
  labs(x = "Watering treatment", y="Overall height (mm)")
ggsave("Quad_OH_TK.jpg", height = 5, width = 7)

  ##Quad_NL_TK--------------------------------------
quadrivulnera = all_data %>% 
  filter(species == "C. quadrivulnera")

quad_num_leaves = lm(num_leaves_over_2mm_2 ~ treatment_ord, data = quadrivulnera)
summary(quad_num_leaves)

mod_means_quad <- emmeans(object = quad_num_leaves,
                          pairwise ~ "treatment_ord",
                          adjust = "tukey")

letters_quad <- cld(object = mod_means_quad$emmeans,
                    Letters = letters)

ggplot() +
  geom_boxplot(data = quadrivulnera, aes(x = treatment_ord, y = num_leaves_over_2mm_2)) +
  theme_cowplot()+
  theme(axis.text.x = element_text(angle = 90)) + # Optionally rotate labels
  geom_text(data = letters_quad, aes(x = treatment_ord, y = emmean, label = gsub(" ", "", .group)),
            position = position_nudge(x = 0.1, y = 5))+
labs(x = "Watering treatment", y="Number of leaves over 2mm")
ggsave("Quad_NL_TK.jpg", height = 5, width = 7)


#Purpurea Plots-------------------------------------------

  ##Purp_LL_TK----------------------------------

purpurea = all_data %>% 
  filter(species == "C. purpurea")

purp_leaf_size = lm(length_largest_leaf_2 ~ treatment_ord, data = purpurea)
summary(purp_leaf_size)

mod_means_purp <- emmeans(object = purp_leaf_size,
                         pairwise ~ "treatment_ord",
                         adjust = "tukey")

letters_purp <- cld(object = mod_means_purp$emmeans,
                   Letters = letters)

ggplot() +
  geom_boxplot(data = purpurea, aes(x = treatment_ord, y = length_largest_leaf_2)) +
  theme_cowplot()+
  theme(axis.text.x = element_text(angle = 90)) + # Optionally rotate labels
  geom_text(data = letters_purp, aes(x = treatment_ord, y = emmean, label = gsub(" ", "", .group)),
            position = position_nudge(x = 0.2, y = 6))+
  labs(x = "Watering treatment", y="Length of largest leaf (mm)")
ggsave("Purp_LL_TK.jpg", height = 5, width = 7)

  ##Purp_OH_TK-----------------------------------------------------


purpurea = all_data %>% 
  filter(species == "C. purpurea")

purp_OH = lm(overall_height_2 ~ treatment_ord, data = purpurea)
summary(purp_OH)

mod_means_purp <- emmeans(object = purp_OH,
                          pairwise ~ "treatment_ord",
                          adjust = "tukey")

letters_purp <- cld(object = mod_means_purp$emmeans,
                    Letters = letters)

ggplot() +
  geom_boxplot(data = purpurea, aes(x = treatment_ord, y = overall_height_2)) +
  theme_cowplot()+
  theme(axis.text.x = element_text(angle = 90)) + # Optionally rotate labels
  geom_text(data = letters_purp, aes(x = treatment_ord, y = emmean, label = gsub(" ", "", .group)),
            position = position_nudge(x = 0.2, y = 65))+
  labs(x = "Watering treatment", y="Overall height")
ggsave("Purp_OH_TK.jpg", height = 5, width = 7)

  ##Purp_NL_TK-------------------------------

purpurea = all_data %>% 
  filter(species == "C. purpurea")

purp_NL = lm(num_leaves_over_2mm_2 ~ treatment_ord, data = purpurea)
summary(purp_NL)

mod_means_purp <- emmeans(object = purp_NL,
                          pairwise ~ "treatment_ord",
                          adjust = "tukey")

letters_purp <- cld(object = mod_means_purp$emmeans,
                    Letters = letters)

ggplot() +
  geom_boxplot(data = purpurea, aes(x = treatment_ord, y =num_leaves_over_2mm_2)) +
  theme_cowplot()+
  theme(axis.text.x = element_text(angle = 90)) + # Optionally rotate labels
  geom_text(data = letters_purp, aes(x = treatment_ord, y = emmean, label = gsub(" ", "", .group)),
            position = position_nudge(x = 0.2, y = 4))+
  labs(x = "Watering treatment", y="Number of leaves over 2mm")
ggsave("Purp_NL_TK.jpg", height = 5, width = 7)


#Amoena Plots--------------------------------------------------

##Amoena_LL_TK---------------------------------------
amoena = all_data %>% 
  filter(species == "C.amoena")

amo_LS = lm(length_largest_leaf_2 ~ treatment_ord, data = amoena)
summary(amo_LS)

mod_means_amo <- emmeans(object = amo_LS,
                          pairwise ~ "treatment_ord",
                          adjust = "tukey")

letters_amo <- cld(object = mod_means_amo$emmeans,
                    Letters = letters)

ggplot() +
  geom_boxplot(data = amoena, aes(x = treatment_ord, y = length_largest_leaf_2)) +
  theme_cowplot()+
  theme(axis.text.x = element_text(angle = 90)) + # Optionally rotate labels
  geom_text(data = letters_quad, aes(x = treatment_ord, y = emmean, label = gsub(" ", "", .group)),
            position = position_nudge(x = 0.1, y = 7))+
  labs(x = "Watering treatment", y="Length of largest leaf (mm)")
ggsave("Amo_LL_TK.jpg", height = 5, width = 7)

##Quad_OH_TK---------------------------------------
amoena = all_data %>% 
  filter(species == "C.amoena")

amo_OH = lm(overall_height_2 ~ treatment_ord, data = amoena)
summary(amo_OH)

mod_means_amo <- emmeans(object = amo_OH,
                         pairwise ~ "treatment_ord",
                         adjust = "tukey")

letters_amo <- cld(object = mod_means_amo$emmeans,
                   Letters = letters)

ggplot() +
  geom_boxplot(data = amoena, aes(x = treatment_ord, y = overall_height_2)) +
  theme_cowplot()+
  theme(axis.text.x = element_text(angle = 90)) + # Optionally rotate labels
  geom_text(data = letters_quad, aes(x = treatment_ord, y = emmean, label = gsub(" ", "", .group)),
            position = position_nudge(x = 0.1, y = 105))+
  labs(x = "Watering treatment", y="Overall height (mm)")
ggsave("Amo_OH_TK.jpg", height = 5, width = 7)

  ##Amoena_NL_TK--------------------------------------
amoena = all_data %>% 
  filter(species == "C.amoena")

amoena_num_leaves = lm(num_leaves_over_2mm_2 ~ treatment_ord, data = quadrivulnera)
summary(amoena_num_leaves)

mod_means_amo <- emmeans(object = amoena_num_leaves,
                          pairwise ~ "treatment_ord",
                          adjust = "tukey")

letters_amo <- cld(object = mod_means_amo$emmeans,
                    Letters = letters)

ggplot() +
  geom_boxplot(data = amoena, aes(x = treatment_ord, y = num_leaves_over_2mm_2)) +
  theme_cowplot()+
  theme(axis.text.x = element_text(angle = 90)) + # Optionally rotate labels
  geom_text(data = letters_quad, aes(x = treatment_ord, y = emmean, label = gsub(" ", "", .group)),
            position = position_nudge(x = 0.1, y = 5))+
  labs(x = "Watering treatment", y="Number of Leaves over 2mm")
ggsave("Amo_NL_TK.jpg", height = 5, width = 7)


#Flowering date plots---------------------------

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
  
    ##Proportion flowered plot--------------------
  
ggplot(data=flower_counts,aes(x=treatment_ord,y=prop_flowered, fill=population))+
  geom_col(stat="identity")+
  facet_wrap(.~species,)+
  theme_cowplot()+
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = "Watering treatment", y="Proportion flowered")
ggsave("Prop_flowered_4panel.jpg", height = 5, width = 7)
  
  


#flower_date = all_data %>% 
#filter(flowered_2 == "Y")%>%
#  filter(species != "C. rhomboidea") 

#ggplot(data=flower_date,aes(x=treatment_ord,y=length_largest_leaf_2, color=population))+
 # geom_boxplot()+
#facet_wrap(.~species, scales = "free_y")
  
#Research Question: How do different species of Clarkia vary in their response to varying amounts of water in terms of growth rate, flowering time, and final size?

#Growth Rate vs. Watering Level--------------------------------

  ##GR_OH/day-------------------------
data=all_data %>% 
  filter(species != "C.amoena")
ggplot(data=data,aes(x=treatment_ord,y=overall_height_per_day, color=population))+
  geom_boxplot()+
  theme_cowplot()+
  theme(axis.text.x = element_text(angle = 90))+
  facet_wrap(.~species, scales = "free_y")+
  labs(x = "Watering treatment", y="Overall height per day (mm)")
ggsave("GR_OH_day_5panel.jpg", height = 5, width = 7)

  
  ##GR_NL/day-------------------------
data=all_data %>% 
filter(species != "C.amoena")
ggplot(data=data,aes(x=treatment_ord,y=num_leaves_per_day, color=population))+
  geom_boxplot()+
  theme_cowplot()+
  theme(axis.text.x = element_text(angle = 90))+
  facet_wrap(.~species, scales = "free_y")+
  labs(x = "Watering treatment", y="Number of leaves per day")
ggsave("GR_NL_day_5panel.jpg", height = 5, width = 7)
































  ## Final Size vs. Watering Level
  
  ## Flowering Time vs. Watering Level
    #for response metrics at hand, do they all tell us simiiar things for a species or do they all tell us the same things 



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
  
  

