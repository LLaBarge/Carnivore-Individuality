
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


# load packages
library(geosphere)
library(adehabitatLT)
library(broom)
library(ggplot2)
library(ggraph) 
library(knitr)
library(leaflet)
library(move2)

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