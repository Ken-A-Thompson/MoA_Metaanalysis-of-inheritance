# MoA Script
# Author: Ken A Thompson
# TBA

# pacakges

library(scales)
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
moa.data <- read.csv('/Users/Ken/Dropbox/!Ph.D./!MoA_Meta-analysis_of_Inheritance/EDD_Database/2017-10-21-EDD_Database.csv')

#create unique study variable

moa.data <- moa.data %>% 
  mutate(study.id = match(Study, unique(Study)))

# Practising with a single study

moa.sticklestudy <- moa.data %>% 
  filter(study.id == 1) %>% 
  mutate(Trait_var = as.numeric(as.character(Trait_var)))

# need to scale variables on a common scale...
# want to have parents set to 0 and 1 (order doesn't matter, just biggest gets 0 and smallest gets 1)

rescale()

# calculate parental-midpoint
# TBA; will have to make a unique cross number and assign parents to the cross.
# Probably will only be for hybrid rows?

# calculate CIs based on var estimates
moa.sticklestudy %>% 
  mutate(ci= if_else(Trait_var_Cat == "SD", 1.96 * (Trait_var/sqrt(Trait_n)), false = 0)) %>% 
  head()

# plot each trait by parent/hybrid... need to scale


trait.mean.plot <- 
  ggplot(moa.sticklestudy,
         aes(x = Trait_mean, y = TraitDesc, fill = Species_or_CrossType)) + 
  geom_point(aes(shape = factor(Species_or_CrossType), fill = factor(Species_or_CrossType)), size = 3) +
  theme_meta
trait.mean.plot

