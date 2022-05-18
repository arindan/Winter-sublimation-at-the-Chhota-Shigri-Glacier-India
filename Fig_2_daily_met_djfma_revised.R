
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
library(egg)
library(patchwork)

# Set WD
setwd("G:/Arindan Work/2 Chapter_Energy balance")                 # Windows
setwd("/Users/arindanmandal/Pictures/2 Chapter_Energy balance")   # Mac OSX
setwd("/home/icwar/Documents/Mandal_files/2 Chapter_Energy balance")   # Linux

# Data
monthly <- read.csv("./DJFMA_new_files/revised_plots/data_rev_plots_git/Fig_2_Monthly_MET_SEB_filled2.csv")
monthly$month <- factor(monthly$month, levels = unique(monthly$month)) 

# Adding DJFMA rectangles
djfma <- data.frame(xmin = 3, xmax = 7, ymin = -Inf, ymax = Inf)

# Plot for Tair
m1 <- ggplot(data = monthly) + 
  geom_hline(yintercept=0, linetype="dashed", color = "grey50", size=.5) +
  geom_point(aes(x=month, y=Tair_mean_mean), colour="steelblue4") +
  geom_line(aes(x=month, y=Tair_mean_mean, group=1), colour="steelblue4") +
  geom_rect(data = djfma, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
            fill="blue", alpha=0.2) +
  scale_y_continuous(limits = c(-22,10)) +
  labs(x = NULL, 
       y = expression(~T[air]~"[°C]")) +
  geom_text(data = monthly, label="DJFMA", x = 5, y = 5, colour="blue") +
  geom_ribbon(aes(x=month, ymin=Tair_mean_mean-Tair_mean_sd, ymax=Tair_mean_mean+Tair_mean_sd, group=1), 
              fill="steelblue4", alpha=0.2) +
  theme_bw() +
  theme(axis.title = element_text(),
        axis.text.x=element_blank(),
        axis.ticks.length=unit(-0.12, "cm"),
        axis.text.y = element_text(margin=margin(5,7,5,5,"pt")))

# Plot for Ts
m2 <- ggplot(data = monthly) + 
  geom_hline(yintercept=0, linetype="dashed", color = "grey50", size=.5) +
  geom_point(aes(x=month, y=Ts_mean_mean), colour="steelblue4") +
  geom_line(aes(x=month, y=Ts_mean_mean, group=1), colour="steelblue4") +
  geom_rect(data = djfma, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
            fill="blue", alpha=0.2) +
  scale_y_continuous(limits = c(-28,18)) +
  labs(x = NULL, 
       y = expression(~T[s]~"[°C]")) +
  geom_ribbon(aes(x=month, ymin=Ts_mean_mean-Ts_mean_sd, ymax=Ts_mean_mean+Ts_mean_sd, group=1), 
              fill="steelblue4", alpha=0.2) +
  theme_bw() +
  theme(axis.title = element_text(),
        axis.text.x=element_blank(),
        axis.ticks.length=unit(-0.12, "cm"),
        axis.text.y = element_text(margin=margin(5,7,5,5,"pt")))


# Plot for RH
m3 <- ggplot(data = monthly) + 
  geom_point(aes(x=month, y=RH_mean_mean), colour="steelblue4") +
  geom_line(aes(x=month, y=RH_mean_mean, group=1), colour="steelblue4") +
  geom_rect(data = djfma, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
            fill="blue", alpha=0.2) +
  scale_y_continuous(limits = c(0,100)) +
  labs(x = NULL, 
       y = "RH [%]") +
  geom_ribbon(aes(x=month, ymin=RH_mean_mean-RH_mean_sd, ymax=RH_mean_mean+RH_mean_sd, group=1), 
              fill="steelblue4", alpha=0.2) +
  theme_bw() +
  theme(axis.title = element_text(),
        axis.text.x=element_blank(),
        axis.ticks.length=unit(-0.12, "cm"),
        axis.text.y = element_text(margin=margin(5,7,5,5,"pt")))

# Plot for WS
m4 <- ggplot(data = monthly) + 
  geom_point(aes(x=month, y=WS_mean_mean), colour="steelblue4") +
  geom_line(aes(x=month, y=WS_mean_mean, group=1), colour="steelblue4") +
  geom_rect(data = djfma, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
            fill="blue", alpha=0.2) +
  scale_y_continuous(limits = c(0,10), breaks = c(0,2,4,6,8,10)) +
  #scale_y_continuous(limits = c(0,16), breaks = c(0,4,8,12,16)) + # before revision to mathch with daily plots as well
  labs(x = NULL, 
       y = parse(text = "u ~~ group('[', m * ~~ s^-1, ']')")) +
  geom_ribbon(aes(x=month, ymin=WS_mean_mean-WS_mean_sd, ymax=WS_mean_mean+WS_mean_sd, group=1), 
              fill="steelblue4", alpha=0.2) +
  theme_bw() +
  theme(axis.title = element_text(),
        axis.text.x=element_blank(),
        axis.ticks.length=unit(-0.12, "cm"),
        axis.text.y = element_text(margin=margin(5,7,5,5,"pt")))

# Plot for Albedo
m5 <- ggplot(data = monthly) + 
  geom_hline(yintercept=0.4, linetype="dashed", color = "#990000", size=0.5) +
  geom_hline(yintercept=0.2, linetype="dashed", color = "black", size=0.5) +
  geom_point(aes(x=month, y=Albedo_acc_mean_mean), colour="steelblue4") +
  geom_line(aes(x=month, y=Albedo_acc_mean_mean, group=1), colour="steelblue4") +
  geom_rect(data = djfma, aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax), 
            fill="blue", alpha=0.2) +
  scale_y_continuous(limits = c(0,1), labels = number_format(accuracy = 0.1)) +
  labs(x = "Month", 
       y = "Albedo") +
  geom_ribbon(aes(x=month, ymin=Albedo_acc_mean_mean-Albedo_acc_mean_sd, ymax=Albedo_acc_mean_mean+Albedo_acc_mean_sd, group=1), 
              fill="steelblue4", alpha=0.2) +
  theme_bw() +
  theme(axis.ticks.length=unit(-0.12, "cm"),
        axis.text.y = element_text(margin=margin(5,7,5,5,"pt")),
        axis.text.x = element_text(margin=margin(7,5,5,5,"pt"))) 


# Grid
patch = m1 / m2 / m3 / m4 / m5
patch + plot_annotation(tag_levels = "A") & theme(plot.tag.position  = c(0.3, .9))

# Save
ggsave("./DJFMA_new_files/daily_met_djfma_revised.jpeg", units="in", width=3, height=8, dpi=300)
