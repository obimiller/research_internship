library(disdat)
library(sf)
# library(ggplot2)
library(prettymapr)

# create windows (on Linux that my dad helped me set up)
x11()

# one of AWT, CAN, NSW, NZ, SA, SWI
# there is no presence absence data for CAN or SWI or SA
region <- "CAN"
species_num <- "01"

# combine the presence and background points (got this from code example in PDF in Appendix 2)

# loading the presence-background data
presence_only_data <- disPo(region)
bg <- disBg(region) # this is 10,000 random backgrounds

# print(head(presence_only_data, 100))

# create the sf object using the coordinates in the presence-only data (this accepts UTM and lat/long)
presence_only_obj <- st_as_sf(presence_only_data, coords = c("x", "y"), crs = 27200)

# the border geometry for the specified country as stored in the disdat library
region_border <- disBorder(region)

# AWT and NSW take a second argument "group = <group>"

# NSW groups -> ba db nb ot ou rt ru sr
# AWT groups -> "bird"	Birds "mam"	Mammals "ru" Reptiles "fro"	Frogs "plant"
# testing_pa <- disPa(region, group="ba")
testing_pa <- disPa(region)

# create the sf object using the coordinates in the testing data (this accepts UTM and lat/long)
testing_sf_obj <- st_as_sf(testing_pa, coords = c("x", "y"), crs = 27200)

# get the names of all species columns
species_names <- names(testing_sf_obj)[!names(testing_sf_obj) %in% attr(testing_sf_obj, "sf_column")]

# just to see what the presence_only data columns and species names are for a given region
# print(names(presence_only_data))
print(species_names)

# function to plot species and presence-only data
plot_pa_species_and_po_data <- function(species_id, data_name, region) {
    par(bg = "white")

    # this is to make sure the legend doesn't get cut off for SWI
    if (region == "SWI") {
      par(mar = c(0, 0, 20, 0))  # add more space to the right
    }

    plot(st_geometry(region_border), col = "white", border = "black", main = species_id)

    # these are just the points for the presence-only data
    coords_po <- st_coordinates(presence_only_obj[presence_only_obj$spid == species_id, ])
    points(coords_po, col = "forestgreen", pch = 23, cex = 1)

    # this is the presence/absence data depending on whether it's 0 or 1
    coords_pa <- st_coordinates(testing_sf_obj[species_id])
    pa_values <- testing_sf_obj[[species_id]]

    coords_presence <- coords_pa[pa_values == 1, ]
    coords_absence  <- coords_pa[pa_values == 0, ]

    colors_pa <- ifelse(pa_values > 0.5, "red", "blue")

    # points(coords_absence, col = rgb(0, 0, 1, 0.3), pch = 17, cex = 0.3)
    points(coords_presence, col = "red", pch = 17, cex = 0.5)


    # points(coords_pa, col = colors_pa, pch = 17, cex = 0.3)

    legend("topright",
       legend = c("Absent (0)", "Present (1)", "Presence Only"),
       pch = c(16, 16, 23),
       col = c("blue", "red", "forestgreen"),
       title = paste("Species", species_id, sep=": "))

    addscalebar(pos = "bottomright", style = "bar", padin = c(0.5, 0.5), label.cex = 0.8)

    # this saves the charts to disk
    file_name <- paste(species_id, "png", sep=".")
    dev.copy(png, filename = file_name, width = 800, height = 600)
    dev.off()
}

for (species_number in 1:20) {
  # format species name according to region

  if (species_number < 10) {
    species_number <- paste("0", species_number, sep="")
  }

  species <- tolower(paste(region, species_number, sep=""))

  # create map with presence-only and testing data
  plot_pa_species_and_po_data(species, "spid", region)
}


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