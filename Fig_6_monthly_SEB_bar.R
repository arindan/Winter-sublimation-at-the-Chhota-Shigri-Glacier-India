
# This code is used to analyse data or generating figures for Mandal et al., 2022
# Pre-print link of the paper: https://tc.copernicus.org/preprints/tc-2021-386/

# Import packages 
rm(list = ls())   
library(ggplot2)
library(scales)
library(lubridate)
library(zoo)
library(dplyr)
library(openxlsx)
library(ggpubr)
library(RColorBrewer)
library(gridExtra)
library(cowplot)
library(grid)
library(Manu)
library(tidyr)

# Set WD
setwd("G:/Arindan Work/2 Chapter_Energy balance")                 # Windows
setwd("/Users/arindanmandal/Pictures/2 Chapter_Energy balance")   # Mac OSX
setwd("/home/icwar/Documents/Mandal_files/2 Chapter_Energy balance")   # Linux


# Make monthly plots
monthly_bar <- readxl::read_excel("./DJFMA_new_files/revised_plots/data_rev_plots_git/Fig_6_monthly_SEB_bar.xlsx", sheet = "absolute")
monthly_fsurf_line <- readxl::read_excel("./DJFMA_new_files/revised_plots/data_rev_plots_git/Fig_6_monthly_SEB_bar.xlsx", sheet = "Fsurf_line")
monthly_percent <- readxl::read_excel("./DJFMA_new_files/revised_plots/data_rev_plots_git/Fig_6_monthly_SEB_bar.xlsx", sheet = "percent_plot")


monthly_bar$month <- factor(monthly_bar$month, levels = unique(monthly_bar$month))
monthly_fsurf_line$month <- factor(monthly_fsurf_line$month , levels = unique(monthly_fsurf_line$month))
monthly_percent$month <- factor(monthly_percent$month, levels = unique(monthly_percent$month))

# Legend label
# Subscript axis text and italics
labels <- c(expression(italic(""~R[net])),
            expression(italic(""~H)),
            expression(italic(""~LE)))

labels2 <- expression(italic(""~F[surface]))

# Plot
p1 <- ggplot() +
  geom_hline(yintercept=0, linetype="dashed", color = "grey10", size=.3) +
  geom_bar(data=monthly_bar, aes(x=month, y=value, fill=variable), stat = "identity", alpha=0.9) +
  geom_point(data = monthly_fsurf_line, aes(x=month, y=Fsurf, color="Fsurf")) +
  scale_color_manual("", label=labels2, values = c("Fsurf"="black")) +
  scale_fill_manual(name=NULL, values = c("Rnet"="#1F6683", "H"="#DD3C51", "LE"="#313657"), label=labels) +
  labs(x = NULL,
       y = expression("Energy flux"~ "[W" ~ m^-2~"]")) +
  theme_bw() +
  theme(legend.position = 'top',
        axis.ticks.length=unit(-0.12, "cm"),
        axis.text.y = element_text(margin=margin(5,7,5,5,"pt")),
        axis.text.x = element_text(margin=margin(7,5,5,5,"pt")))
p1

# Save
ggsave("./DJFMA_new_files/monthly_SEB_bar_percent_suppli.jpeg", units="in", width=3.6, height=2.6, dpi=300)

