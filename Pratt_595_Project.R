### Abby Pratt
### Biol 595 Final Project

# A project-specific ‘R’ project + Git Hub repository (10 points)
# Evidence of version control (5 points)
# https://github.com/abbyp96/Bio595_Project

# Reading-in data (2 points)
Obs = read.csv("Octopod_Observations.csv")

# Application of the appropriate data storage structure: list, data frame, matrix or array (2 points) ##########

# Example of indexing (2 points)
# Looking at all S unicirrhus observations from OBIS
Obs[c(1938:1956),]

# Subsetting (2 points)
Bbairdii = Obs[c(1:1533),]
Gver = Obs[c(1534:1721),]
Muus = Obs[c(1722:1837),]
Pter = Obs[c(1838:1915),]
Suni = Obs[c(1916:1956),]

# Ordering (2 points) ######### why is it ordering within species and not across all??????? #############
obs_type = Obs[order(Obs$Obs.Type),]

# Summarizing (5 points)
library(dplyr)
osb.type.sp = Obs %>% group_by(Obs.Type, Species) %>%
  summarise(count = length(Species))

# Merge or Join data frames (5 points) #########

# Custom function(s) (10 points) #############
# 'if else' statement (10 points)

Spp.fxn = function(x){
  
  if(str_detect(string = Obs$Species, pattern = "Bathy")){
    Species_2 <- "Bathypolypus_bairdii"
    
  } else if(str_detect(string = Obs$Species, pattern = "Gran")) {
    species_2 <- "Graneledone_verrucosa"
    
  } else if(str_detect(string = Obs$Species, pattern = "Muus")) {
    species_2 <- "Muusoctopus_spp"
    
  } else if(str_detect(string = Obs$Species, pattern = "Pter")) {
    species_2 <- "Pteroctopus_tetracirrhus"
    
  } else if(str_detect(string = Obs$Species, pattern = "Scae")) {
    species_2 <- "Scaeurgus_unicirrhus"
    
  } else {}
  
  return(Obs$Species)
  
}

# ‘for loop’ (10 points) ##############

library(stringr)
Obs$species_2 = NA

for(i in 1:nrow(Obs)){
  Obs[i,]$species_2 <- Spp.fxn(x = d[i,])
}

# Custom operator(s) (10 points)
#(sort by only caribbean observations based on lats less than ) ######## Is this what is meant by custom 
# operators??????? #########################################

carib <- subset(Obs, Lat < 25.06 & Lon < 87.72)

# Reshaping data with ‘melt’ and/or ‘dcast’ (5 points)


# Histogram plot (5 points)

library(ggplot2)
ggplot(Obs, aes(Species)) +
  geom_histogram(stat="count")


# Point, bar, or line plot (whichever makes the most sense) (5 points)


# ‘ggplot’ with at least 2 geoms (e.g. point, bar, tile), use one of the ‘scale_’ geoms, and adjusting the
# theme of the plot (10 points)



# A map of showing the geographic location where the data was collected (10 points)

library(tidyverse)
library(ggmap)
library(osmdata)
bb = c(left = -98, bottom = 7.5, 
       right = -50, top = 55.5)
map_bb = get_stamenmap(bb, zoom = 6, map = 'terrain-background')
ggmap(map_bb) + 
  geom_point(data = Obs, aes(x = Lon, y = Lat))

# Exporting data set (2 points) ############


# ‘ddply’ (10 points)
# Exporting and saving figures from ggplot (2 points) ##########


ddply(df15, .variables = "species", function(x){
  
  name = unique(x$species)
  
  map = ggmap(map_bb.f) + 
    geom_point(data = df15, aes(x = lon, y = lat))+ 
    ggtitle(paste0(name))
  
  
  ggsave(filename = paste0(name, '.jpg'), plot = map, width = 4, height = 3, units = 'in', dpi = 600)
  
}, .progress = "text")




