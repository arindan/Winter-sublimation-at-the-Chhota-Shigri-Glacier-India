
# This code is used to analyse data or generating figures for Mandal et al., 2022
# Pre-print link of the paper: https://tc.copernicus.org/preprints/tc-2021-386/

# Import packages 
rm(list = ls())
library(ggplot2)
library(scales)
library(lubridate)
library(zoo)
library(dplyr)
library(ggpubr)
library(RColorBrewer)
library(gridExtra)
library(cowplot)
library(grid)
library(Manu)
library(tidyr)
library(GGally)
library(readxl)
library(patchwork)

# Set WD
setwd("G:/Arindan Work/2 Chapter_Energy balance")                      # Windows
setwd("/Users/arindanmandal/Pictures/2 Chapter_Energy balance")        # Mac OSX
setwd("/home/icwar/Documents/Mandal_files/2 Chapter_Energy balance")   # Linux

#import data
data <- read.csv("./DJFMA_new_files/revised_plots/data_rev_plots_git/Fig_13_djfma_df_selected_columns.csv")
data$Timesteps <- as.POSIXlt(data$Timesteps, tz=Sys.timezone())   # Make it as DateTime object


######################################################################################################
# Multi-Scatter Plot with Sublimation
# Data for multi-scatter plot sublimation vs other met variables
data_subli_multi_scatter <- data %>%
  mutate(hour_minute = as.numeric(format(Timesteps, "%H.%M"))) %>%
  filter(hour_minute >= 9 & hour_minute <= 16) %>% # make it 09-16 hrs considering CC Comment
  #filter(cf <= 0.2) %>%
  drop_na()


# Plots
sp1 <- ggplot(data_subli_multi_scatter , aes(x=subli, y=q*1000, color = WS)) + 
  geom_point(size=0.5, show.legend = F) +
  scale_color_gradientn(expression("u" ~"[m" ~ s^-1~"]"), colours=rev(brewer.pal(9,"YlGnBu"))) +
  xlab(NULL) +
  ylab(expression(italic(q) ~ "[g" ~ kg^-1~"]")) +
  theme_bw() +
  theme(axis.ticks.length=unit(-0.12, "cm"),
        legend.position = c(0.6, 0.88),
        legend.direction = "horizontal",
        legend.background = element_rect(fill = "transparent"),
        axis.text.y = element_text(margin=margin(5,7,5,5,"pt")),
        axis.text.x = element_text(margin=margin(7,5,5,5,"pt")))

sp2 <- ggplot(data_subli_multi_scatter, aes(x=subli, y=Tair, color = WS)) + 
  geom_point(show.legend = F, size=0.5) +
  scale_color_gradientn(colours=rev(brewer.pal(9,"YlGnBu"))) +
  geom_hline(yintercept=0, linetype="dashed", color = "grey50", size=.3) +
  xlab(NULL) +
  ylab(expression(~T[air]~"[°C]")) +
  theme_bw() +
  theme(axis.ticks.length=unit(-0.12, "cm"),
        axis.text.y = element_text(margin=margin(5,7,5,5,"pt")),
        axis.text.x = element_text(margin=margin(7,5,5,5,"pt")))

sp3 <- ggplot(data_subli_multi_scatter, aes(x=subli, y=Ts, color = WS)) + 
  geom_point(show.legend = T, size=0.5) +
  scale_color_gradientn(expression("u" ~"[m" ~ s^-1~"]"), colours=rev(brewer.pal(9,"YlGnBu"))) +
  geom_hline(yintercept=0, linetype="dashed", color = "grey50", size=.3) +
  xlab(NULL) +
  ylab(expression(~T[s]~"[°C]")) +
  theme_bw() +
  theme(legend.position = c(0.6, 0.25),
        legend.direction = "horizontal",
        axis.ticks.length=unit(-0.12, "cm"),
        axis.text.y = element_text(margin=margin(5,7,5,5,"pt")),
        axis.text.x = element_text(margin=margin(7,5,5,5,"pt")),
        legend.background = element_rect(fill = "transparent")) +
  guides(colour = guide_colourbar(title.position="top", title.hjust = 0.5))

sp4 <- ggplot(data_subli_multi_scatter, aes(x=subli, y=cf, color = WS)) + 
  geom_point(show.legend = FALSE, size=0.5) +
  scale_color_gradientn(colours=rev(brewer.pal(9,"YlGnBu"))) +
  xlab(expression("Sublimation rate"~ "[mm" ~ d^-1~"]")) +
  ylab('Cloud factor') +
  theme_bw() +
  theme(axis.ticks.length=unit(-0.12, "cm"),
        axis.text.y = element_text(margin=margin(5,7,5,5,"pt")),
        axis.text.x = element_text(margin=margin(7,5,5,5,"pt")))

sp5<- ggplot(data_subli_multi_scatter, aes(x=subli, y=Sin, color = WS)) + 
  geom_point(show.legend = FALSE, size=0.5) +
  scale_color_gradientn(colours=rev(brewer.pal(9,"YlGnBu"))) +
  #geom_hline(yintercept=0, linetype="dashed", color = "grey50", size=.3) +
  xlab(expression("Sublimation rate"~ "[mm" ~ d^-1~"]")) +
  ylab(expression("S"["in"] ~ "[W" ~ m^-2~"]")) +
  theme_bw() +
  theme(axis.ticks.length=unit(-0.12, "cm"),
        axis.text.y = element_text(margin=margin(5,7,5,5,"pt")),
        axis.text.x = element_text(margin=margin(7,5,5,5,"pt")))

sp6 <- ggplot(data_subli_multi_scatter, aes(x=subli, y=Lin, color = WS)) + 
  geom_point(show.legend = FALSE, size=0.5) +
  scale_color_gradientn(colours=rev(brewer.pal(9,"YlGnBu"))) +
  #geom_hline(yintercept=0, linetype="dashed", color = "grey50", size=.3) +
  xlab(expression("Sublimation rate"~ "[mm" ~ d^-1~"]")) +
  ylab(expression("L"["in"] ~ "[W" ~ m^-2~"]")) +
  theme_bw() +
  theme(axis.ticks.length=unit(-0.12, "cm"),
        axis.text.y = element_text(margin=margin(5,7,5,5,"pt")),
        axis.text.x = element_text(margin=margin(7,5,5,5,"pt")))

# Grid
plot_grid(sp1, sp2, sp3, sp4, sp5, sp6, align = "v", ncol=3, nrow=2, labels = "AUTO")

# Save
ggsave("./DJFMA_new_files/djfma_subli_multi_scatter_revised.jpeg", units="in", width=8.5, height=5, dpi=300)



