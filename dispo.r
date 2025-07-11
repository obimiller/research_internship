library(disdat)
library(sf)
library(ggplot2)

# create windows (on Linux that my dad helped me set up)
x11()

# one of AWT, CAN, NSW, NZ, SA, SWI
# there is no presence absence data for CAN or SWI or SA
region <- "AWT"

# combine the presence and background points (got this from code example in PDF in Appendix 2)

# loading the presence-background data
pr <- disPo(region)
bg <- disBg(region) # this is 10,000 random backgrounds

training <- rbind(pr, bg)

# create the sf object using the coordinates in the training data (this accepts UTM and lat/long)
training_sf_obj <- st_as_sf(training, coords = c("x", "y"), crs = 27200)

# the border geometry for the specified country as stored in the disdat library
region_border <- disBorder(region)

# AWT and NSW take a second argument "group = <group>"

# NSW groups -> ba db nb ot ou rt ru sr
# AWT groups -> "bird"	Birds "mam"	Mammals "ru" Reptiles "fro"	Frogs "plant"
testing_pa <- disPa(region, group="bird")

# create the sf object using the coordinates in the testing data (this accepts UTM and lat/long)
testing_sf_obj <- st_as_sf(testing_pa, coords = c("x", "y"), crs = 27200)

# get the names of all species columns
species_names <- names(testing_sf_obj)[!names(testing_sf_obj) %in% attr(testing_sf_obj, "sf_column")]

# just to see what the training data columns and species names are for a given region
print(names(training))
print(species_names)

# function to plot species and training data
plot_pa_species_and_training_data <- function(species_id, data_name) {

  # makes the data for P/A a diamond
    species_shape <- 23
  
    plot(st_geometry(region_border), col = "white", border = "black", main = species_id)
    plot(training_sf_obj[data_name], add= TRUE, cex = 0.3)
    plot(testing_sf_obj[species_id], pch = species_shape, bg = "red", add = TRUE, cex = 1.5)
}

# format species name according to region
species <- tolower(paste(region, "02", sep=""))

# create map with training and testing data
plot_pa_species_and_training_data(species, "slope")


# keep the window open until the program ends
while(TRUE) {
  Sys.sleep(1)
}

# Questions:
#
# When plotting the PA testing data for a given species, why do they get plotted with mulitple colors? Does one mean presence and one be absence?
#
# Is there no PA dat for Canada, South Africa and Switzerland (SWI?)?
#
# How do we use this data to make predicitons? I'm not sure how to do that. 
#
# Is there a way to know what the species actually are? NZ01, etc.