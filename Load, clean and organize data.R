
################################################################################

# Jaguar Movement Project

################################################################################
# questions - is there a latitudinal or productivity gradient associated with
# space use variability?

# do less homogeneous habitats promote the use of clusters due to spatial variation
# and patchiness
################################################################################
# Importance: predictability in predator behavior - especially consistency in space-use
# is a prerequisite for prey to learn about dangerous locations, resulting in
# spatial risk effects (e.g., 'the landscape of fear'). These spatial risk effects
# are thought to emerge within heterogeneous environments where distinct locations or 
# microhabitats are especially risky for prey. Ambushing hunters that use aspects of 
# habitat structure to conceal themselves before an attack may also facilitate prey
# learning as their hunting domains tend to be narrower than predators that can chase
# prey great distances. These higher latitude locations tend to have a lower diversity
# and abundance of possible prey items, potentially leading to stronger top-down 
# effects on preferred prey species.
# Thus far, however, few studies have examined biogeopgraohical variation in the
# predictability of predator movement and space-use.

################################################################################
# Predictions

# 1) In more heterogeneous habitats, animals may be anchored to locations that are 
# especially good for hunting - resulting in consistent space use and recursion to
# particularly important hunting spots. 1a) Yet, predators in low productivity habitats
# tend to have larger ranges, suggesting that space use variability will be higher
# in these habitats, in order to track prey.
# 1b) In contrast, predators inhabiting homogeneous habitats may have less need to 
# shift their space use due to a more consistent climate, which reduces spatial
# variability in prey forage and abundance.
################################################################################

# Methods: Examine short-term variability in space use using GPS data on Jaguar 
# from a range of biomes in the Americas.

#Metrics:
# Space-use variability: Earth Mover's distance - monthly dbbmm occurrence distributions
# Recursion / revisitation rates:  whether clusters emerge in more heterogeneous 
# environments more often
# Daily maximum distance traveled

# Are these metrics most associated with biome, habitat heterogeneity (from MODIS), 
# potential prey diversity (from literature),
# or body size (e.g., may affect dietary niche, resulting in clustering for those taking particular prey)?
################################################################################
# Script Below
################################################################################

# Jaguar Individuality Project


#Sessioninfo()
#R version 4.1.3 (2022-03-10)
#Platform: x86_64-w64-mingw32/x64 (64-bit)
#Running under: Windows 10 x64 (build 22621)

# Import Dataset

library(readr)
Jaguar_Movement_Data_filtered <- read_csv("Jaguar_Movement_Data_filtered.csv", 
                                          col_types = cols(timestamp = col_datetime(format = "%m/%d/%Y %H:%M")))
Jag_df<-as.data.frame(Jaguar_Movement_Data_filtered)

head(Jag_df)
str(Jag_df)

#rename ID column
n_distinct(Jag_df$ID)
Jag_df$ID<-Jag_df$`individual.local.identifier (ID)`


# load packages
library(geosphere)
library(dplyr)
library(adehabitatLT)
library(broom)
library(ggplot2)
library(adehabitatLT)
library(broom)
library(ggplot2)
library(ggraph) 
library(knitr)
library(leaflet)
library(move2)

# plot the data
leaflet(Jag_df) %>%
  addTiles() %>%
  addCircles(lng = ~location.long , lat = ~location.lat, weight = 1)


# create a column for month-year and ID so that we can create monthly occurrence distributions
Jag_df$month <- format(Jag_df$timestamp, "%m")
Jag_df$year <- format(Jag_df$timestamp, "%Y")
Jag_df$day <- format(Jag_df$timestamp, "%d")
Jag_df$ID_Date <- paste("Jag",Jag_df$ID, Jag_df$month, Jag_df$year, sep = "_")


# function to calculate distance in meters between sequential points. This will 
# allow us to see where distance traveled was > than collar error, allowing us
# to denote active periods
Jag_df_dist<-Jag_df %>%
  group_by(ID) %>%
  mutate(dist = c(NA, geosphere::distVincentyEllipsoid(cbind(location.long, location.lat)))) %>%
  ungroup()

# check that distances are NA when ID changes
View(Jag_df_dist)

# Denote whether distance traveled in previous timestep was >100m, indicating 
# movement greater than likely telemetry error

Jag_df_dist$Active<-ifelse(Jag_df_dist$dist>100, 1, 0)


ctmmweb::app()

# function to calculate distance between sequential points. This will allow us to 
# see where distance traveled was > than collar error, allowing us to denote 
#active periods

Jag_df["distance"] <- c(NA,
                    sapply(seq.int(2,nrow(df)), function(i){
                      distm(c(df$Longitude[i-1],df$Latitude[i-1]),
                            c(df$Longitude[i], df$Latitude[i]),
                            fun = distHaversine)
                    })
)
