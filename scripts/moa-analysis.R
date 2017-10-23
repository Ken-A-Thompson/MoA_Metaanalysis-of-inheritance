# MoA Script
# Author: Ken A Thompson
# TBA

# pacakges

library(modEvA)
# library(scales)
library(tidyverse)

# plot theme

theme_meta <- theme(aspect.ratio=1.0,panel.background = element_blank(),
                        panel.grid.major = element_blank(),
                        panel.grid.minor = element_blank(),
                        panel.border=element_blank(),
                        axis.line = element_line(size=1),
                        axis.line.x = element_line(color="black", size = 1),
                        axis.line.y = element_line(color="black", size = 1),
                        axis.ticks=element_line(color="black"),
                        axis.text=element_text(color="black"),
                        axis.title=element_text(color="black"),
                        axis.title.y=element_text(vjust=0.2, size=12),
                        axis.title.x=element_text(vjust=0.1,size=12),
                        axis.text.x=element_text(size=10),
                        axis.text.y=element_text(size=10))

# read data
moa.data <- read.csv('/Users/ken.thompson/Dropbox/!Ph.D./!MoA_Meta-analysis_of_Inheritance/EDD_Database/2017-10-23-EDD_Database.csv')

#create unique study variable

moa.data <- moa.data %>% 
  mutate(study.id = match(Study, unique(Study)))

# Practising with a single study

moa.sticklestudy <- moa.data %>% 
  filter(study.id == 2) %>% 
  mutate(Trait_var = as.numeric(as.character(Trait_var))) %>% 
  mutate(ci = if_else(Trait_var_Cat == "SD", 1.96 * (Trait_var/sqrt(Trait_n)), false = 0)) # calculate CIs




# are traits different, if so, are hybrids in the middle?

# # start by spreading 
# moa.sticklestudy.wide <- moa.sticklestudy %>% 
#   group_by(study.id) %>% 
#   spread(key = Parent_Hybrid, value = c(Trait_mean, Trait_var))





# set one parent to -1 other to +1
moa.sticklestudy.a <- moa.sticklestudy %>%
  group_by(study.id, TraitNo) %>% # group by study and trait
  mutate(Trait.mean.subbed = Trait_mean - min(Trait_mean[Parent_Hybrid == "Parent"])) %>% # subtract minimum parent value from all, rendering one parent = 0
  mutate(Trait.mean.scaled = Trait.mean.subbed  * (2 / max(Trait.mean.subbed[Parent_Hybrid == "Parent"])) - 1)  # now divide all values by 2/max - 1 to set parent 1 =-1, parent 2 = 1; note: this doesn't appropriately scale SD
  # re-orient all variables so the same parent is -1 for all traits

# left here 2017-10-23
# need to figure out how to muliply a 'trait' by -1 if the specified parent does not equal -1 or 1.
# trying some solutions below but it's proving difficult

# # NEED to create a logical function that, if specified parent is equal to 1, returns TRUE or False
# 
# test.data <- moa.sticklestudy.a %>% 
#   select(Parent_Hybrid, Species_or_CrossType, Trait.mean.scaled) %>% 
#   mutate(SpeciesNo = match(Species_or_CrossType, unique(Species_or_CrossType))) %>% 
#   filter(SpeciesNo == 1)
# ifelse(test.data$Trait.mean.scaled == 1, T, F)
# 
# reorient.y.n <- function(x){
# x <- x %>%
#     select(Parent_Hybrid, Species_or_CrossType, Trait.mean.scaled) %>% 
#     mutate(SpeciesNo = match(Species_or_CrossType, unique(Species_or_CrossType))) %>% 
#     filter(SpeciesNo == 1)
# return(ifelse(x$Trait.mean.scaled == 1, T, F))
# }
# 
#










if(moa.sticklestudy$Species_or_CrossType[1] > moa.sticklestudy$Species_or_CrossType[2])






# plot each trait
trait.mean.plot <- 
  ggplot(moa.sticklestudy.a,
         aes(x = Trait.mean.scaled, y = TraitDesc, fill = Species_or_CrossType)) + 
  geom_point(aes(shape = factor(Species_or_CrossType), fill = factor(Species_or_CrossType)), size = 3) +
  geom_vline(xintercept = 0, col = "red") +
  # geom_errorbarh(aes(xmax = scale.trait.mean + scale.trait.var, xmin = scale.trait.mean - scale.trait.var)) + # plots horizontal error bars
  theme_meta
trait.mean.plot

