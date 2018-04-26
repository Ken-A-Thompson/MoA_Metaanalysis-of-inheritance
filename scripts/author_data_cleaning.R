# 2018-04-26 author data reformatting script
# Author. Ken A. Thompson

# library(PerformanceAnalytics) # for correlation chart
library(Hmisc)
library(tidyverse)

# bradshaw data

# Generate a figure with Bradshaw & Schemske Data

NIS.Bradshaw.Data <- read.csv(file = "data/author_data/bradshaw_sacred.csv", na.strings = "-")

# head(MOA.Bradshaw.Data)

Bradshaw.Wide <- NIS.Bradshaw.Data %>% 
  select(fam, flexup:nectar, height, mean.flower.., proj.area:white, lower.petal:mean.curl)

names(Bradshaw.Wide)


Bradshaw.Long <- Bradshaw.Wide %>%  #select relevent 'trait' columns
  gather(traitname, value, flexup:mean.curl) %>% 
  mutate(value = as.numeric(value)) %>% 
  na.omit() # drop all obs with NA
  
Bradshaw.Tidy <- Bradshaw.Long %>% 
  group_by(fam, traitname) %>% 
  summarise(mean = mean(value), sd = sd(value), count = n())

# assign parent/hybrid to study
Brad.parent <- c("L", "C")
Bradshaw.Tidy$Parent_Hybrid <- ifelse(Bradshaw.Tidy$fam %in% Brad.parent, "Parent", "Hybrid")

# assign TraitNo
Bradshaw.Tidy <- transform(Bradshaw.Tidy, id=as.numeric(factor(traitname)))

# correlation matrix
Bradshaw.Wide.NoFam <-  Bradshaw.Wide %>% 
  select(-fam)

Bradshaw.CorMat <- rcorr(as.matrix(Bradshaw.Wide.NoFam), type = "pearson")

Bradshaw.CorMat$r
