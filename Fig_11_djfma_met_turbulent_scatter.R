
# This code is used to analyse data or generating figures for Mandal et al., 2022
# Pre-print link of the paper: https://tc.copernicus.org/preprints/tc-2021-386/

# Import packages 
rm(list = ls())   
library(ggplot2)
library(scales)
library(lubridate)
library(ggpmisc)
library(dplyr)
library(ggpubr)
library(RColorBrewer)
library(gridExtra)
library(cowplot)
library(grid)
library(Manu)
library(tidyr)
library(patchwork)
library(cowplot)

# Set WD
setwd("G:/Arindan Work/2 Chapter_Energy balance")                 # Windows
setwd("/Users/arindanmandal/Pictures/2 Chapter_Energy balance")   # Mac OSX
setwd("/home/icwar/Documents/Mandal_files/2 Chapter_Energy balance")   # Linux

# Data
data_all      <- readxl::read_excel("./DJFMA_new_files/revised_plots/data_rev_plots_git/Fig_11_djfma_hourly_clear_overcast_scatter.xlsx")

# Labels
colnames(data_all)[1] <- "h"
colnames(data_all)[2] <- "le"
colnames(data_all)[3] <- "ta_ts"
colnames(data_all)[4] <- "q_qRS"

# filter
data_clear <- data_all %>%
  filter(type == "Clear-sky")
data_over <- data_all %>%
  filter(type == "Overcast")

#correlation of two variables with p-value
# Clear-sky
cor.test(data_over$h, data_over$WS)
# Other way for Pearson correlation
cor(data_all$h, data_all$ta_ts)
# Other way for R2 using linear model: coefficient of determination of a linear regression model
corr_lm = lm(h ~ WS, data=data_over) 
summary(corr_lm)
round(summary(corr_lm)$r.squared, 2)


#scatter point size
size=1.5
# Formula
formula1 <- y ~ x
# Plot: H
p1 <- ggplot(data=data_all, aes(x=ta_ts, y=h, color=type)) +
  geom_hline(yintercept=0, linetype="dashed", color = "grey50", size=.3) +
  geom_vline(xintercept=0, linetype="dashed", color = "grey50", size=.3) +
  geom_point(alpha=0.3, size=size, shape=20) +
  labs(x=NULL,
       y=expression("H"~ "[W" ~ m^-2~"]")) +
  scale_colour_manual("",values = c("Overcast"="darkblue","Clear-sky"="tomato")) +
  theme_bw() +
  theme(legend.position = c(.25, .85),
        legend.background = element_rect(fill = "transparent"),
        axis.ticks.length=unit(-0.12, "cm"),
        axis.text.y = element_text(margin=margin(5,7,5,5,"pt")),
        axis.text.x = element_text(margin=margin(7,5,5,5,"pt"))) 

p2 <- ggplot(data=data_all, aes(x=q_qRS*1000, y=h, color=type)) +
  geom_hline(yintercept=0, linetype="dashed", color = "grey50", size=.3) +
  geom_point(alpha=0.3, size=size, shape=20) +
  labs(x=NULL,
       y=NULL) +
  scale_colour_manual("",values = c("Overcast"="darkblue","Clear-sky"="tomato")) +
  theme_bw() +
  theme(legend.position = 'none',
        legend.background = element_rect(fill = "transparent"),
        axis.ticks.length=unit(-0.12, "cm"),
        axis.text.y = element_text(margin=margin(5,7,5,5,"pt")),
        axis.text.x = element_text(margin=margin(7,5,5,5,"pt"))) 

p3 <- ggplot(data=data_all, aes(x=WS, y=h, color=type)) +
  geom_hline(yintercept=0, linetype="dashed", color = "grey50", size=.3) +
  geom_point(alpha=0.3, size=size, shape=20) +
  labs(x=NULL,
       y=NULL) +
  scale_colour_manual("",values = c("Overcast"="darkblue","Clear-sky"="tomato")) +
  theme_bw() +
  theme(legend.position = 'none',
        legend.background = element_rect(fill = "transparent"),
        axis.ticks.length=unit(-0.12, "cm"),
        axis.text.y = element_text(margin=margin(5,7,5,5,"pt")),
        axis.text.x = element_text(margin=margin(7,5,5,5,"pt"))) 

# Plot: LE
p4 <- ggplot(data=data_all, aes(x=ta_ts, y=le, color=type)) +
  geom_hline(yintercept=0, linetype="dashed", color = "grey50", size=.3) +
  geom_vline(xintercept=0, linetype="dashed", color = "grey50", size=.3) +
  geom_point(alpha=0.3, size=size, shape=20) +
  labs(x=expression(~T[air]~"-"~T[s]~ "[°C]"),
       y=expression("LE"~ "[W" ~ m^-2~"]")) +
  scale_colour_manual("",values = c("Overcast"="darkblue","Clear-sky"="tomato")) +
  theme_bw() +
  theme(legend.position = 'none',
        legend.background = element_rect(fill = "transparent"),
        axis.ticks.length=unit(-0.12, "cm"),
        axis.text.y = element_text(margin=margin(5,7,5,5,"pt")),
        axis.text.x = element_text(margin=margin(7,5,5,5,"pt"))) 

p5 <- ggplot(data=data_all, aes(x=q_qRS*1000, y=le, color=type)) +
  geom_hline(yintercept=0, linetype="dashed", color = "grey50", size=.3) +
  geom_point(alpha=0.3, size=size, shape=20) +
  labs(x=expression(~q~"-"~q[s]~ "[g" ~ kg^-1~"]"),
       y=NULL) +
  scale_colour_manual("",values = c("Overcast"="darkblue","Clear-sky"="tomato")) +
  theme_bw() +
  theme(legend.position = 'none',
        legend.background = element_rect(fill = "transparent"),
        axis.ticks.length=unit(-0.12, "cm"),
        axis.text.y = element_text(margin=margin(5,7,5,5,"pt")),
        axis.text.x = element_text(margin=margin(7,5,5,5,"pt"))) 

p6 <- ggplot(data=data_all, aes(x=WS, y=le, color=type)) +
  geom_hline(yintercept=0, linetype="dashed", color = "grey50", size=.3) +
  geom_point(alpha=0.3, size=size, shape=20) +
  labs(x=expression('u'~"[m" ~ s^-1~"]"),
       y=NULL) +
  scale_colour_manual("",values = c("Overcast"="darkblue","Clear-sky"="tomato")) +
  theme_bw() +
  theme(legend.position = 'none',
        legend.background = element_rect(fill = "transparent"),
        axis.ticks.length=unit(-0.12, "cm"),
        axis.text.y = element_text(margin=margin(5,7,5,5,"pt")),
        axis.text.x = element_text(margin=margin(7,5,5,5,"pt"))) 

# Arrange all
plot_grid(p1,p2,p3,p4,p5,p6, align = "hv", labels = "AUTO")

# Save
ggsave("./DJFMA_new_files/djfma_met_turbulent_scatter_without_r2.jpeg", units="in", width=8.5, height=6, dpi=300)

