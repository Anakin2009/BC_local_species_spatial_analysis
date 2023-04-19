getwd()
setwd('~/Desktop/UBC/DATA 589')

library(tidyverse)
library(spatstat)
library(sf)
library(sp)
library(maptools)
library(viridis)
library(raster)

# dataset with only longitude and latitude
coyote <- read.csv('BC_local_species_spatial_analysis/Project_Dataset/coyote_raw_coordinate.csv',
                   header = TRUE, fill = TRUE)

# coordinate system needs to be adjusted
coyote$decimalLatitude <- as.numeric(coyote$decimalLatitude)*10000
coyote$decimalLongitude <- as.numeric(coyote$decimalLongitude)*10000*(-1)
head(coyote)


plot(coyote$decimalLongitude, coyote$decimalLatitude, pch = 16, cex = 0.5, col = 'red')

load('BC_local_species_spatial_analysis/Project_Dataset/BC_Covariates.Rda')
ls()
BC_Covariates <- DATA
rm(DATA)





# Visualise Window
plot(DATA$Window, col="grey", border="black", main="BC Map")

# Visualise Elevation
plot(DATA$Elevation, col=grey(seq(0,1,0.01)), axes=FALSE, main="BC Elevation")

# Visualise Forest
plot(DATA$Forest, col=grey(seq(0,1,0.01)), axes=FALSE, main="BC Forest")

plot(DATA$HFI)

plot(DATA$Dist_Water)


# Convert to a ppp object
coyote_ppp <- ppp(x = coyote$decimalLongitude,
                  y = coyote$decimalLatitude,
                  window = as.owin(BC_Covariates$Window))
plot(coyote_ppp, pch = 16, cex = 0.5, cols = "#046C9A")
