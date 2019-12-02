### Abby Pratt
### Biol 595 Final Project

# A project-specific ‘R’ project + Git Hub repository (10 points)
# Evidence of version control (5 points)
# https://github.com/abbyp96/Bio595_Project

# load libraries
library(stringr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(ggmap)
library(osmdata)

# Reading-in data (2 points)
# Application of the appropriate data storage structure: list, data frame, matrix or array (2 points)
obs = read.csv("Octopod_Observations.csv", header=T, na.strings=c("","NA"))
str(obs)
obs_depth = read.csv("Octopod_Observations_depth.csv", header=T, na.strings=c("","NA"))


# Adding Species Names ----------------------------------------------------

# Custom function(s) (10 points) 
# 'if else' statement (10 points)

Spp.fxn = function(x){
  
  y <- x$Species
  
  if(str_detect(string = y, pattern = "Bathy")){
    species_2 <- "Bathypolypus_bairdii"
    
  } else if(str_detect(string = y, pattern = "Gran")) {
    species_2 <- "Graneledone_verrucosa"
    
  } else if(str_detect(string = y, pattern = "Muus")) {
    species_2 <- "Muusoctopus_spp"
    
  } else if(str_detect(string = y, pattern = "Pter")) {
    species_2 <- "Pteroctopus_tetracirrhus"
    
  } else if(str_detect(string = y, pattern = "Scae")) {
    species_2 <- "Scaeurgus_unicirrhus"
    
  } else if(str_detect(string = y, pattern = "Scau")) {
    species_2 <- "Scaeurgus_unicirrhus"
    
  } else {}
  
  return(species_2)
  
}

# ‘for loop’ (10 points) 

obs$species_2 = NA

for(i in 1:nrow(obs)){
  obs[i,]$species_2 <- Spp.fxn(x = obs[i,])
}

obs_depth$species_2 = NA

for(i in 1:nrow(obs_depth)){
  obs_depth[i,]$species_2 <- Spp.fxn(x = obs_depth[i,])
}

obs$Species = NULL
obs_depth$Species = NULL
obs$Source = NULL
obs_depth$Source = NULL


# Merging and Subsetting --------------------------------------------------

#Merge or Join data frames (5 points)
obs$seq = seq(1,nrow(obs),1)
obs_depth$seq = seq(1,nrow(obs_depth),1)
obs_dll = merge(obs, obs_depth, by = c("seq","Obs.Type", "species_2"))

# Subsetting (2 points)
obs_dll = obs_dll[complete.cases(obs_dll), ]

# Ordering (2 points) 
obs_type = obs[order(obs$Obs.Type),]

# Example of indexing (2 points)
# Looking at all S. unicirrhus observations from OBIS
obs[c(1938:1956),]

# Ordering (2 points) 
obs_type = obs[order(obs$obs.Type),]

# Custom operator(s) (10 points)
#(sort by only caribbean observations based on lats less than)
# Exporting data set (2 points)

carib <- subset(obs, Lat < 25.06 & Lon < 87.72)
write.csv(carib, file="Caribbean_Obs.csv")


# Summarizing (5 points)
osb.type.sp = obs %>% group_by(Obs.Type, species_2) %>%
  summarise(count = length(species_2))
osb.type.sp

# Histogram plot (5 points)
ggplot(obs, aes(species_2)) +
  geom_histogram(stat="count")


# Point, bar, or line plot (whichever makes the most sense) (5 points)
obs_dll$Depth = as.character(obs_dll$Depth)
obs_dll$Depth = as.numeric(obs_dll$Depth)
ggplot(obs_dll, aes(species_2, Depth)) + 
  geom_boxplot()

# A map of showing the geographic location where the data was collected (10 points)
bb = c(left = -98, bottom = 7.5, 
       right = -50, top = 55.5)
map_bb = get_stamenmap(bb, zoom = 6, map = 'terrain-background')
ggmap(map_bb) + 
  geom_point(data = obs, aes(x = Lon, y = Lat))

# ‘ddply’ (10 points)
# Exporting and saving figures from ggplot (2 points)
library(dplyr)
library(plyr)

ddply(obs, .variables = c("species_2"), function(x){
  
  x <- na.omit(x)
  
  name = unique(x$species_2)
  
  map = ggmap(map_bb) + 
    geom_point(data = x, aes(x = Lon, y = Lat))+ 
    ggtitle(name)
  
  ggsave(filename = paste0(name, '.jpg'), plot = map, width = 4, height = 3, units = 'in', dpi = 600)
  
}, .progress = "text")




