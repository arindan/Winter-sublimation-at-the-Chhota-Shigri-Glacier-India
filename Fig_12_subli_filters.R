
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
library(reshape2)
library(GGally)
library(caret)
library(readxl)
library(patchwork)

# Set WD
setwd("G:/Arindan Work/2 Chapter_Energy balance")                      # Windows
setwd("/Users/arindanmandal/Pictures/2 Chapter_Energy balance")        # Mac OSX
setwd("/home/icwar/Documents/Mandal_files/2 Chapter_Energy balance")   # Linux


# Import data
all_figs <- readxl::read_excel("./DJFMA_new_files/revised_plots/data_rev_plots_git/Fig_12_djfma_subli_different_filters.xlsx") #need to copy updated values from the RAW file into this to procede further plotting 
colnames(all_figs)
all_figs$condition <- factor(all_figs$condition, levels = unique(all_figs$condition))
unique(all_figs$condition)

# Set labels as desired
my_labeller <- as_labeller(c("entire data"="No~filter",
                             "wind_speed>10" ="Wind~speed>10~m~sec^-1",
                             "specific_humidity>2"="Specific~humidity>2~g~kg^-1",
                             "specific_humidity<1"="Specific~humidity<1~g~kg^-1",
                             "Ts_higher"="Ts~higher~than~-10~degC",
                             "Ts_lower"="Ts~lower~than~-10~degC"),
                           default = label_parsed)


# Easy way to handle secondary axis
ylim.prim <- c(0, 15)   # in this example, RH
ylim.sec <- c(0, 10)    # in this example, u
b <- diff(ylim.prim)/diff(ylim.sec)
a <- ylim.prim[1] - b*ylim.sec[1] # there was a bug here

# Plot
ggplot(all_figs, aes(x=hour)) +
  geom_line(data=all_figs, aes(y=a+subli_RS_mean*b, color='Sublimation', group=condition)) +
  geom_line(data=all_figs, aes(y=WS_mean, color='Wind speed', group=condition)) +
  geom_line(data=all_figs, aes(y=q_mean*1000, color='Specific humidity', group=condition)) +
  geom_ribbon(data=all_figs, aes(x = hour, 
                                 ymin=a+subli_RS_mean*b-(a+subli_RS_mean*b), 
                                 ymax=a+subli_RS_mean*b+(a+subli_RS_mean*b), group=condition),fill= "#DD3C51",alpha=0.15) +
  geom_ribbon(data=all_figs, aes(x = hour, ymin=WS_mean-WS_sd, ymax=WS_mean+WS_sd, group=condition),fill= "#313657",alpha=0.15) +
  geom_ribbon(data=all_figs, aes(x = hour, ymin=q_mean*1000-q_sd*1000, ymax=q_mean*1000+q_sd*1000, group=condition),fill= "#51806a",alpha=0.15) +
  scale_color_manual("",
                     values = c("Sublimation"="#DD3C51",
                                "Wind speed"="#313657",
                                "Specific humidity"="#51806a")) +
  labs(x="Local time [IST]") +
  scale_y_continuous(name = expression("Wind speed" ~ "[m" ~ s^-1~"]" ~ "Specific humidity" ~ "[g" ~ kg^-1~"]"), 
                     sec.axis = sec_axis(~ (. - a)/b, name = expression("Sublimation"~ "[mm" ~ d^-1~"]")),
                     breaks = seq(0,14,3)) +
  theme_bw() +
  theme(axis.ticks.length=unit(-0.12, "cm"),
        axis.line.y.right = element_line(color = "#DD3C51"),
        axis.ticks.y.right = element_line(color = "#DD3C51"),
        axis.text.y.right = element_text(color = "#DD3C51", margin=margin(5,5,5,7,"pt")), 
        axis.title.y.right = element_text(color = "#DD3C51", vjust=3),
        axis.text.y = element_text(margin=margin(5,7,5,5,"pt")),
        axis.text.x = element_text(margin=margin(7,5,5,5,"pt")),
        legend.position = c(0.18, 0.95),
        legend.background = element_rect(fill = "transparent")) +
  scale_x_continuous(breaks = seq(from = 9, to = 16, by = 1)) +
  facet_wrap(. ~condition, nrow = 3, labeller = my_labeller) 

# Save
ggsave("./DJFMA_new_files/djfma_subli_different_filters_revised.jpeg", units="in", width=5.5, height=7, dpi=300)
