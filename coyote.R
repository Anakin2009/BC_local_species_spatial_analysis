getwd()
setwd('~/Desktop/UBC/DATA 589')

library(tidyverse)
library(spatstat)
library(sf)
library(sp)
library(maptools)
library(viridis)
library(raster)

# Dataset with only longitude and latitude columns
coyote <- read.csv('BC_local_species_spatial_analysis/Project_Dataset/coyote_raw_coordinate.csv',
                   header = TRUE, fill = TRUE)

# BC Albers projection string
geo_proj <- "+proj=aea +lat_0=45 +lon_0=-126 +lat_1=50 +lat_2=58.5 +x_0=1000000 +y_0=0 +datum=NAD83 +units=m +no_defs"

# Create a SpatialPointsDataFrame object using the coyote data
coyote_sp <- SpatialPointsDataFrame(coords = data.frame(coyote$decimalLongitude, coyote$decimalLatitude),
                                    data = coyote)

# Assign the WGS84 CRS (EPSG:4326) to the SpatialPointsDataFrame object
 proj4string(coyote_sp) <- CRS("+proj=longlat +datum=WGS84")

# Transform the SpatialPointsDataFrame object to the BC Albers projection
coyote_spTrans <- spTransform(coyote_sp, CRS(geo_proj))

# Extract the longitude and latitude coordinates from the SpatialPointsDataFrame object
coyote_spTrans$decimalLongitude <- coyote_spTrans@coords[,1]
coyote_spTrans$decimalLatitude <- coyote_spTrans@coords[,2]

##########################################################################################
# Load the BC covariates
load('BC_local_species_spatial_analysis/Project_Dataset/BC_Covariates.Rda')
#ls()
BC_Covariates <- DATA # The dataset is called DATA or BC_Covariates
#rm(DATA)

##########################################################################################
# Visualise Window
plot(DATA$Window, col="grey", border="black", main="BC Map")

# Visualise Elevation
plot(DATA$Elevation, col=grey(seq(0,1,0.01)), axes=FALSE, main="BC Elevation")

# Visualise Forest
plot(DATA$Forest, col=grey(seq(0,1,0.01)), axes=FALSE, main="BC Forest")

# Visualise Human Footprint Index
# representing the degree of human impact or disturbance on the landscape.
# Higher values could indicate areas with more human activity or infrastructure,
# such as roads, settlements, or agriculture, while lower values could represent more
# pristine or undisturbed natural areas.
plot(DATA$HFI, main="BC Human Footprint Index")

# Visualise Distance to Water
# represents the distance to the nearest water body, such as rivers, lakes, or the ocean.
# The values in the raster would indicate the distance from each pixel to the closest water
# source. This information could be important in ecological studies, as the proximity to
# water can have a significant impact on species distributions and habitat characteristics.
plot(DATA$Dist_Water, main="BC Distance to Water")

##########################################################################################
# Convert to a ppp object
coyote_ppp <- ppp(x = coyote_spTrans$decimalLongitude,
                  y = coyote_spTrans$decimalLatitude,
                  window = as.owin(BC_Covariates$Window))

# Remove duplicated points
coyote_ppp <- unique.ppp(coyote_ppp)

##########################################################################################
# Visualise the coyote sightings
plot(coyote_ppp, pch = 16, cols = "#046C9A", cex= 0.5, main = "Coyote Sightings")

## Visualise the coyote sightings with the BC elevation
# Split the elevation image into 5 elevation classes
elev_cut <- cut(DATA$Elevation, 5, labels = c("very low", "low","medium","high","very high"))
# plot the elevation class image
plot(elev_cut, main = "Elevation classes")
# overlay the park locations
plot(coyote_ppp, cols = "black", pch = 16, cex = 0.6, add = TRUE)
plot(coyote_ppp, cols = viridis(5), pch = 16, cex = 0.5, add = TRUE)

## Visualise the coyote sightings with the BC forest
# Split the forest image into 5 forest classes
forest_cut <- cut(DATA$Forest, 5, labels = c("very low", "low","medium","high","very high"))
# plot the forest class image
plot(forest_cut, main = "Forest classes")
# overlay the park locations
points(coyote_ppp, pch = 16, cex = 0.5)

## Visualise the coyote sightings with the BC human footprint index
# Split the human footprint index image into 5 classes
hfi_cut <- cut(DATA$HFI, 5, labels = c("very low", "low","medium","high","very high"))
# plot the human footprint index class image
plot(hfi_cut, main = "Human Footprint Index classes")
# overlay the park locations
points(coyote_ppp, pch = 16, cex = 0.5)

## Visualise the coyote sightings with the BC distance to water
# Split the distance to water image into 5 classes
dist_water_cut <- cut(DATA$Dist_Water, 5, labels = c("very low", "low","medium","high","very high"))
# plot the distance to water class image
plot(dist_water_cut, main = "Distance to Water classes")
# overlay the park locations
points(coyote_ppp, pch = 16, cex = 0.5)


##########################################################################################
# Density plots

# Density plots of elevations coyote sightings overlayed on top of a
# Density plots of elevations within the window
plot(density(DATA$Elevation[]),
     col="#046C9A",
     main = "",
     xlab = "Elevation (m)",
ylim = c(0, 0.002 )) #blue
lines(density(DATA$Elevation[coyote_ppp]),
      col="red") # red
# Legend for clarity
legend("topright", c("Provincial elevation", "Coyote sightings elevation"),
       pch = 16,
       col=c("#046C9A","red"))

# Density plots of forest coyote sightings overlayed on top of a
# Density plots of forest within the window
plot(density(DATA$Forest[]),
     col="#046C9A",
     main = "",
     xlab = "Forest (%)",
     ylim = c(0, 0.05 )) #blue
lines(density(DATA$Forest[coyote_ppp]),
      col="red") # red
# Legend for clarity
legend("topright", c("Provincial forest", "Coyote sightings forest"),
       pch = 16,
       col=c("#046C9A","red"))

# Density plots of human footprint index coyote sightings overlayed on top of a
# Density plots of human footprint index within the window
plot(density(DATA$HFI[]),
     col="#046C9A",
     main = "",
     xlab = "Human Footprint Index",
     ylim = c(0, 22 )) #blue
lines(density(DATA$HFI[coyote_ppp]),
      col="red") # red
# Legend for clarity
legend("topright", c("Provincial human footprint index", "Coyote sightings human footprint index"),
       pch = 16,
       col=c("#046C9A","red"))

# Density plots of distance to water coyote sightings overlayed on top of a
# Density plots of distance to water within the window
plot(density(DATA$Dist_Water[]),
     col="#046C9A",
     main = "",
     xlab = "Distance to Water (m)",
     ylim = c(0, 0.0007 )) #blue
lines(density(DATA$Dist_Water[coyote_ppp]),
      col="red") # red
# Legend for clarity
legend("topright", c("Provincial distance to water", "Coyote sightings distance to water"),
       pch = 16,
       col=c("#046C9A","red"))


##########################################################################################
### Estimate intensity of coyote sightings per square kilometer in BC
# Calculate the area of the study region (BC) in square kilometers
area_bc_sq_km <- as.numeric(area.owin(as.owin(DATA$Window))) / 1000000
# Calculate the number of parks
num_coyotes <- npoints(coyote_ppp)
# Estimate the intensity of parks per square kilometer in BC (assuming homogeneity)
intensity_coyotes_sq_km <- num_coyotes / area_bc_sq_km
intensity_coyotes_sq_km

# The estimated intensity might not be trustworthy because we have not done any analysis
# to determine whether the assumption of homogeneity is met. Further analysis is needed.
# The distribution of coyote sightings in BC seems to be not homogeneous.
# Assuming homogeneity could lead to misleading intensity estimates.


##########################################################################################
# Split into a 2 by 3 quadrat and count points
Q <- quadratcount(coyote_ppp, nx = 2, ny = 3)

# Quadrat test for homogeneity
quadrat.test(Q)

# Visualize the quadrats, including the points
plot(Q, main = "Quadrats with Points",
     col = terrain.colors(100),
     border = "grey")
points(coyote_ppp, pch = 20, cex = 0.5, col = "black")

# Visualize the estimated intensity, including the points
plot(intensity(Q, image = TRUE), main = "Estimated Intensity with Points",
     col = viridis(200),
     border = "grey")
plot(coyote_ppp, pch = 20, cex = 0.5, cols = "white", add = TRUE)

# Based on the quadrat test result (p-value is very small), it suggests that there is
# a significant deviation from homogeneity and the assumption of homogeneity is not met.
# Therefore, the estimated intensity of coyote sightings per square kilometer in BC
# (assuming homogeneity) is not trustworthy.


##########################################################################################
# Estimate intensity using kernel estimation with likelihood cross validation bandwidth selection
lambda_u_hat <- density(coyote_ppp, sigma = bw.ppl)
lambda_u_hat

# Plot the estimated intensity to the bandwidth optimiser
plot(lambda_u_hat, main = "Kernel Density Estimation of Coyote sightings",
     #ribbon = F,
     col = viridis(200),
     border = "grey")

# Perform hot spot analysis to identify locations of elevated intensity
R <- bw.ppl(coyote_ppp) # Estimate R
LR <- scanLRTS(coyote_ppp, r = R) # Calculate test statistic


# Visualize the output (including the window)
plot(LR, main = "Hotspot Analysis of Coyote sightings")
plot(DATA$Window, add = TRUE, lwd = 2) # Add the window to the plot

# Given the hotspot analysis, it appears that Metro Vancouver, Kelowna
# have elevated intensity of parks.


##########################################################################################
# Estimate rho for the locations of coyote sightings

# define quartiles
b_elevation <- quantile(DATA$Elevation, probs = (0:4)/4, type = 2)
b_forest <- quantile(DATA$Forest, probs = (0:4)/4, type = 2)
b_hfi <- quantile(DATA$HFI, probs = (0:4)/4, type = 2)
b_dist_water <- quantile(DATA$Dist_Water, probs = (0:4)/4, type = 2)

# Split image into 4 equal-area quadrats based covariate values
Zcut_elevation <- cut(DATA$Elevation, breaks = b_elevation)
V_elevation <- tess(image = Zcut_elevation)

Zcut_forest <- cut(DATA$Forest, breaks = b_forest)
V_forest <- tess(image = Zcut_forest)

Zcut_hfi <- cut(DATA$HFI, breaks = b_hfi)
V_hfi <- tess(image = Zcut_hfi)

Zcut_dist_water <- cut(DATA$Dist_Water, breaks = b_dist_water)
V_dist_water <- tess(image = Zcut_dist_water)


# Count points in each quadrate
quadratcount(coyote_ppp, tess = V_elevation)
quadratcount(coyote_ppp, tess = V_forest)
quadratcount(coyote_ppp, tess = V_hfi)
quadratcount(coyote_ppp, tess = V_dist_water)


# Estimate Rho
rho_elevation <- rhohat(coyote_ppp, covariate = DATA$Elevation)
rho_forest <- rhohat(coyote_ppp, covariate = DATA$Forest)
rho_hfi <- rhohat(coyote_ppp, covariate = DATA$HFI)
rho_dist_water <- rhohat(coyote_ppp, covariate = DATA$Dist_Water)

# Visualize the estimated rho
plot(rho_elevation, xlim = c(0, range(DATA$Elevation)[2]))
plot(rho_forest, xlim = c(0, range(DATA$Forest)[2]))
plot(rho_hfi, xlim = c(0, range(DATA$HFI)[2]))
plot(rho_dist_water, xlim = c(0, range(DATA$Dist_Water)[2]))


##########################################################################################
# Ripley's K-function

set.seed(8888)
## Under homogeneity assumption
# Estimate the empirical K-function
K_obs <- Kest(coyote_ppp, correction = "border")

# Bootstrapped CIs
K_sim <- envelope(coyote_ppp,
                  Kest,
                  nsim = 19, # set nsim to 19, means alpha = 0.05
                  correction = "border",
                  rank = 1,
                  fix.n = T)

# Visualise the observed (empirical) K-function and the theoretical (simulated) K-function with 95% CIs
plot(K_sim, main = "Under homogeneity assumption")


## Under inhomogeneity assumption
# Estimate a strictly positive density
lambda_coyote_pos <- density(coyote_ppp, sigma = bw.ppl, positive = TRUE)

# Simulation envelope (with points drawn from the estimated intensity)
K_sim_inhom <- envelope(coyote_ppp,
                        Kinhom,
                        simulate = expression(rpoispp(lambda_coyote_pos)),
                        correction="border",
                        rank = 1,
                        nsim = 19,
                        fix.n = TRUE)

# Visualise the observed (empirical) K-function and the theoretical (simulated) K-function with 95% CIs
plot(K_sim_inhom, main = "Under inhomogeneity assumption")

# Zoom in on range where significant deviations appear
plot(K_sim_inhom, main = "", xlim = c(0, 20000))


##########################################################################################
# pair correlation functions

## Under homogeneity assumption
# Simulation envelope
#pcf_coyote <- envelope(coyote_ppp,pcf,nsim = 19,rank = 1) # runs very slow, uncomment to run
# Visualise the results
#plot(pcf_coyote, main = "Pair Correlation Function (Homogeneity Corrected)")

## Under inhomogeneity assumption
# Simulation envelope (with points drawn from the estimated intensity)
pcf_coyote_inhom <- envelope(coyote_ppp,
                            pcfinhom,
                            simulate = expression(rpoispp(lambda_coyote_pos)),
                            rank = 1,
                            nsim = 19)
# Visualise the results
plot(pcf_coyote_inhom, main = "Pair Correlation Function (Inhomogeneity Corrected)")


##########################################################################################
# Model fitting

#Fit the PPP model
fit1 <- ppm(coyote_ppp ~ Elevation + Forest + Dist_Water, data = DATA)
fit1

fit2 <- ppm(coyote_ppp ~ Elevation + I(Elevation^2) + Forest + I(Forest^2) + Dist_Water + I(Dist_Water^2), data = DATA)
fit2  # HFI seems to have an issue, so excluded from the model

fit_coord <- ppm(coyote_ppp ~ x + y, data = DATA) # Coordinates as covariate
fit_coord
##########################################################################################
# Model visualisation

#Plot the model predictions
plot(fit, se = FALSE, superimpose = FALSE)
plot(coyote_ppp, add = TRUE, pch = 16, cex = 0.5, cols = "black")

# Mean
E_elevation <- mean(DATA$Elevation)
E_forest <- mean(DATA$Forest)
E_hfi <- mean(DATA$HFI)
E_dist_water <- mean(DATA$Dist_Water)

# Elevational effect on lambda at mean forest cover
#elevation_forest <- effectfun(fit, "Forest", Elevation = E_elevation, se.fit = T)


##########################################################################################
# Model Selection

# Conduct a likelihood ratio test
anova(fit1, fit2, test = "LRT")

# AIC values
AIC(fit1); AIC(fit2); AIC(fit_coord)


##########################################################################################
# Model Validation

# Quadrat counting
# Run the quadrat test
quadrat.test(fit1, nx = 4, ny = 2)
quadrat.test(fit2, nx = 4, ny = 2)

# PPP Residuals
res1 <- residuals(fit1)
plot(res1, cols = "transparent")

res2 <- residuals(fit2)
plot(res2, cols = "transparent")


# Calculate the relative intensity as a function of covariates
#rh_elevation <- rhohat(fit1, DATA$Elevation)
#rh_forest <- rhohat(fit1, DATA$Forest)
#rh_hfi <- rhohat(fit1, DATA$HFI)
#rh_dist_water <- rhohat(fit1, DATA$Dist_Water)

#Side by side plotting
#par(mfrow = c(2,2))
#plot(rh_elevation, legend = FALSE, main = "", xlab = "Elevation (m)")
#plot(rh_forest, legend = FALSE, main = "", xlab = "Forest Cover (%)")
#plot(rh_hfi, legend = FALSE, main = "", xlab = "HFI")
#plot(rh_dist_water, legend = FALSE, main = "", xlab = "Distance to Water (m)")

# Lurking variable plot
lurking(fit1 , DATA$Elevation , type = "raw" , cumulative = F, envelope = T)
lurking(fit1 , DATA$Forest , type = "raw" , cumulative = F, envelope = T)
lurking(fit1 , DATA$HFI , type = "raw" , cumulative = F, envelope = T)
lurking(fit1 , DATA$Dist_Water , type = "raw" , cumulative = F, envelope = T)

lurking(fit2 , DATA$Elevation , type = "raw" , cumulative = F, envelope = T)
lurking(fit2 , DATA$Forest , type = "raw" , cumulative = F, envelope = T)
lurking(fit2 , DATA$HFI , type = "raw" , cumulative = F, envelope = T)
lurking(fit2 , DATA$Dist_Water , type = "raw" , cumulative = F, envelope = T)


# Calculate the partial Residuals
par_res_elevation <- parres(fit1 , "Elevation" )
plot(par_res_elevation, main = "Partial Residuals", xlab = "Elevation (m)")

par_res_forest <- parres(fit1 , "Forest" )
plot(par_res_forest, main = "Partial Residuals", xlab = "Forest Cover (%)")

par_res_hfi <- parres(fit1 , "HFI" )
plot(par_res_hfi, main = "Partial Residuals", xlab = "HFI")

par_res_dist_water <- parres(fit1 , "Dist_Water" )
plot(par_res_dist_water, main = "Partial Residuals", xlab = "Distance to Water (m)")


##########################################################################################