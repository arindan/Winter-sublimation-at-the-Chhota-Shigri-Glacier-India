
# This code is used to analyse data or generating figures for Mandal et al., 2022
# Pre-print link of the paper: https://tc.copernicus.org/preprints/tc-2021-386/

# Import packages 
rm(list = ls())
library(dplyr)
library(readxl)
library(openxlsx)
library(lubridate)
library(reshape2)
library(tidyverse)
library(ggpmisc)
require(Metrics)
library(zoo)
library(patchwork)
library(ggExtra)

# Set WD
setwd("G:/Arindan Work/2 Chapter_Energy balance")                 # Windows
setwd("/Users/arindanmandal/Pictures/2 Chapter_Energy balance")   # Mac OSX
setwd("/home/icwar/Documents/Mandal_files/2 Chapter_Energy balance")   # Linux

# Data
plot_data <- read.csv("./DJFMA_new_files/revised_plots/data_rev_plots_git/Fig_8_cloud_frac.csv") 
plot_data$month <- factor(plot_data$month, levels = unique(plot_data$month))

# Plot
ggplot(plot_data, aes(x=month, y=value, fill=type)) +
  geom_bar(stat = "identity", position = position_dodge()) +
  geom_text(aes(label = value), fontface = "bold", vjust = 1.5,
            position = position_dodge(.9), size = 4) +
  labs(y= "CF fraction (%)", fill=NULL, x=NULL) +
  scale_fill_brewer() +
  theme_bw() +
  theme(legend.position = 'top',
        axis.ticks.length=unit(-0.12, "cm"),
        axis.text.y = element_text(margin=margin(5,7,5,5,"pt")),
        axis.text.x = element_text(margin=margin(7,5,5,5,"pt")))
  
# Save
ggsave("./DJFMA_new_files/cloud_fraction.jpeg", units="in", width=3.5, height=3, dpi=300)













