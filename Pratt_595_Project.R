### Abby Pratt
### Biol 595 Final Project

# A project-specific ‘R’ project + Git Hub repository (10 points)
# Evidence of version control (5 points)
# https://github.com/abbyp96/Bio595_Project

# Reading-in data (2 points)
# Application of the appropriate data storage structure: list, data frame, matrix or array (2 points)
Obs = read.csv("Octopod_Observations.csv")

# Example of indexing (2 points)
# Looking at all S. unicirrhus observations from OBIS
Obs[c(1938:1956),]

# Subsetting (2 points)
Bbairdii = Obs[c(1:1533),]
Gver = Obs[c(1534:1721),]
Muus = Obs[c(1722:1837),]
Pter = Obs[c(1838:1915),]
Suni = Obs[c(1916:1956),]

# Ordering (2 points) 
obs_type = Obs[order(Obs$Obs.Type),]

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

# ‘for loop’ (10 points) ##############

library(stringr)
Obs$species_2 = NA

for(i in 1:nrow(Obs)){
Obs[i,]$species_2 <- Spp.fxn(x = Obs[i,])
}


# Summarizing (5 points)
library(dplyr)
osb.type.sp = Obs %>% group_by(Obs.Type, Species) %>%
  summarise(count = length(Species))

# Merge or Join data frames (5 points) #########

b_g = merge.data.frame(Bbairdii, Gver, by = "Obs.Type")

# Custom operator(s) (10 points)
#(sort by only caribbean observations based on lats less than ) ######## Is this what is meant by custom 
# operators??????? #########################################

carib <- subset(Obs, Lat < 25.06 & Lon < 87.72)

# Reshaping data with ‘melt’ and/or ‘dcast’ (5 points)


# Histogram plot (5 points)

library(ggplot2)
ggplot(Obs, aes(species_2)) +
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
library(dplyr)
library(plyr)

ddply(Obs, .variables = c("species_2"), function(x){
  
  x <- na.omit(x)
  
  name = unique(x$species_2)
  
  map = ggmap(map_bb) + 
    geom_point(data = x, aes(x = Lon, y = Lat))+ 
    ggtitle(name)
  
  ggsave(filename = paste0(name, '.jpg'), plot = map, width = 4, height = 3, units = 'in', dpi = 600)
  
}, .progress = "text")




