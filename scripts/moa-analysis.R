# MoA Script
# Author: Ken A Thompson
# TBA

# pacakges

# library(modEvA)
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
moa.data <- read.csv('/Users/Ken/Dropbox/!Ph.D./!MoA_Meta-analysis_of_Inheritance/EDD_Database/2017-10-23-EDD_Database.csv')

#create unique study variable

moa.data <- moa.data %>% 
  mutate(study.id = match(Study, unique(Study)))

# Practising with a single study

moa.sticklestudy <- moa.data %>% 
  filter(study.id == 2) %>% 
  mutate(Trait_var = as.numeric(as.character(Trait_var))) %>% 
  mutate(ci = if_else(Trait_var_Cat == "SD", 1.96 * (Trait_var/sqrt(Trait_n)), false = 0)) # calculate CIs


# are traits different, if so, are hybrids in the middle?


# set one parent to -1 other to +1
moa.sticklestudy.a <- moa.sticklestudy %>%
  group_by(study.id, TraitNo) %>% # group by study and trait
  mutate(Trait.mean.subbed = Trait_mean - min(Trait_mean[Parent_Hybrid == "Parent"])) %>% # subtract minimum parent value from all, rendering one parent = 0
  mutate(Trait.mean.scaled = Trait.mean.subbed  * (2 / max(Trait.mean.subbed[Parent_Hybrid == "Parent"])) - 1)  # now divide all values by 2/max - 1 to set parent 1 =-1, parent 2 = 1; note: this doesn't appropriately scale SD
  # mutate(ci.scaled = ci  * (2 / max(Trait.mean.subbed[Parent_Hybrid == "Parent"])) - 1) 
  # re-orient all variables so the same parent is -1 for all traits



# calculate opposing dominance
# variance works well for this



# calculate parental mid-pt
moa.parentmid <- moa.sticklestudy.a %>% 
  select(Parent_Hybrid, TraitNo ,Trait_mean) %>% 
  filter(Parent_Hybrid == "Parent") %>% 
  group_by(TraitNo) %>% 
  summarize(mean(Trait_mean))

#list of parental midpoints
parent.mids <- moa.parentmid$`mean(Trait_mean)`

# do CIs from hybrid contain midpoint?
moa.trait.ci <- moa.sticklestudy %>% 
  filter(Parent_Hybrid == "Hybrid") %>% 
  mutate(Low.Bound = Trait_mean - ci, High.Bound = Trait_mean + ci) %>% 
  mutate(parent.mid = parent.mids)

# proportion of dominant traits (later: among traits that differ between parents)
View(moa.trait.ci)
as.vector(moa.trait.ci$TraitDesc)
with(moa.trait.ci, Low.Bound <= parent.mid & High.Bound >= parent.mids)
# nice, works


# scale trait so that all one parent = -1, all other = 1 
moa.sticklestudy.b <- moa.sticklestudy.a %>%
  group_by(TraitNo) %>% 
  mutate(inverse.need = if_else(Trait.mean.scaled[Species_or_CrossType == "Paxton_Benthic"] == 1, T, F)) %>%  # true if all is good, false if need to invert 
  mutate(final.trait = ifelse(inverse.need == T, -1 * Trait.mean.scaled, 1 * Trait.mean.scaled))
View(test.data)

         
# net dominance
nd.data <- moa.sticklestudy.b %>% 
  ungroup() %>% 
  filter(Parent_Hybrid == "Hybrid") %>% 
  summarize(net = mean(final.trait))
nd <- nd.data$net
opp <- nd.data$opp # this is the 'opposing dominance' quantification (for now) unfortunate thing is it's not in the same 'units' as the parents...

# opposing dominance
# brute forece test
A <- moa.sticklestudy.b %>% filter(Species_or_CrossType == "Paxton_Benthic")
A <- as.vector(A$final.trait)

B <- moa.sticklestudy.b %>% filter(Species_or_CrossType == "Paxton_Limnetic")
B <- as.vector(B$final.trait)

P <- moa.sticklestudy.b %>% filter(Species_or_CrossType == "F1")
P <- as.vector(P$final.trait)

#bring to the origin
pa = P - A
ba = B - A

t = as.vector((pa %*% ba) / (ba %*% ba))
d = (pa - t * ba)
dist = sqrt(sum(d^2))
dist



# plot each trait
trait.mean.plot <- 
  ggplot(moa.sticklestudy.b,
         aes(x = final.trait, y = TraitDesc, fill = Species_or_CrossType)) + 
  geom_point(aes(shape = factor(Species_or_CrossType), fill = factor(Species_or_CrossType)), size = 3) +
  geom_vline(xintercept = 0, col = "red") +
  # geom_errorbarh(aes(xmax = Trait.mean.scaled + ci, xmin = Trait.mean.scaled - ci)) + # plots horizontal error bars; not sure show to do this.
  theme_meta
trait.mean.plot
