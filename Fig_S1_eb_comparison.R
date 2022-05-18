
# This code is used to analyse data or generating figures for Mandal et al., 2022
# Pre-print link of the paper: https://tc.copernicus.org/preprints/tc-2021-386/
# NOTE: for this figure, data can be used from Table S1 in the supplementary material

# Import packages 
rm(list = ls()) 
library(scales)
library(ggplot2)
library(sf)
library(ggpubr)
library(cartography)
library(ggrepel)
library(rgdal)
library(dplyr)
library(ggthemes)
library(reshape2)
library(devtools)
library(ggmap)
library(grid)
library(Manu)
library(scales)
library(MetBrewer)


# Glacier xy points in basemap 
eb_radiation <- read.csv('/home/icwar/Documents/Mandal_files/2 Chapter_Energy balance/DJFMA_new_files/revised_plots/data_rev_plots_git/Fig_S1.csv')

# Creating a Basemap
myMap <- get_stamenmap(bbox = c(left = 70,
                                 bottom = 25,
                                 right = 105,
                                 top = 42),
                        maptype = "terrain-background", 
                        crop = F,
                        zoom = 6) 
ggmap(myMap)

# Map
ggmap(myMap) + 
  labs(x = "Longitude (°E)", 
       y = "Latitude (°N)",
       title = "SEB studies in the High Mountain Asia (n=28)") +
  geom_point(data = eb_radiation, aes(x = x, y = y, fill = "red", alpha = 0.5), size = 2, shape = 19) +
  guides(fill = FALSE, alpha = FALSE, size = FALSE) +
  geom_text_repel(data = eb_radiation, aes(x = x, y = y, label = Glacier)) +
  ggeasy::easy_center_title()


# Save
ggsave("/Users/arindanmandal/Pictures/2 Chapter_Energy balance/seb_comparison_himalaya/only_map_test.jpeg", 
       units="in", width=8, height=7, dpi=300)


