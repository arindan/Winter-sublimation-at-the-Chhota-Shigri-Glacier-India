
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

# Data
hourly <- readxl::read_excel("./DJFMA_new_files/revised_plots/data_rev_plots_git/Fig_7_djfma_hourly.xlsx") 
colnames(hourly)[1] <- "hour"

# Labels
labels1 <- expression(italic(~T[air]),
                      italic(~T[s]))
labels2 <- expression(italic("RH"),
                      italic("u"))
labels3 <- expression(italic("CF"),
                      italic("L"["in"]),
                      italic("L"["out"]),
                      italic("S"["in"]),
                      italic("S"["out"]))
labels4 <- expression(italic("H"),italic("LE"), italic("R"["ib"]))
labels5 <- expression(italic("R"["net"]), italic("H"~"+"~"LE"), italic("F"["isurface"]))

# Plots
p1 <- ggplot(data = hourly, aes(x=hour)) +
  geom_hline(yintercept=0, linetype="dashed", color = "grey50", size=.5) +
  geom_point(aes(y=Tair_mean, color="Tair")) + 
  geom_line(aes(y=Tair_mean, color="Tair")) +
  geom_point(aes(y=Ts_mean, color="Ts")) +
  geom_line(aes(y=Ts_mean, color="Ts")) +
  geom_ribbon(aes(ymin = Tair_mean-Tair_sd, ymax = Tair_mean+Tair_sd), 
              fill = "#313657", alpha=0.2) +
  geom_ribbon(aes(ymin = Ts_mean-Ts_sd, ymax = Ts_mean+Ts_sd), 
              fill = "#DD3C51", alpha=0.2) +
  scale_color_manual("", values = c("Tair"="#313657", "Ts"="#DD3C51"), label=labels1) +
  labs(x = NULL, y = "Temperature [?C]") +
  scale_x_continuous(limits = c(0,23), breaks = c(0,6,12,18,23)) +
  theme_bw() +
  theme(axis.title = element_text(),
        axis.text.x=element_blank(),
        axis.ticks.length=unit(-0.12, "cm"),
        axis.text.y = element_text(margin=margin(5,7,5,5,"pt")),
        legend.position = c(0.18,0.75),
        legend.background = element_rect(fill = "transparent"))

# rh and u
# Easy way to handle secondary axis
ylim.prim <- c(0, 100)   # in this example, rh
ylim.sec <- c(0, 20)    # in this example, u
b <- diff(ylim.prim)/diff(ylim.sec)
a <- ylim.prim[1] - b*ylim.sec[1] # there was a bug here

p2 <- ggplot(data = hourly, aes(x=hour)) +
  geom_point(aes(y=RH_mean, color="RH")) + 
  geom_line(aes(y=RH_mean, color="RH")) +
  geom_ribbon(aes(ymin = RH_mean-RH_sd, ymax = RH_mean+RH_sd), 
              fill = "#313657", alpha=0.2) +
  geom_point(aes(y=a+WS_mean*b, color="WS")) +
  geom_line(aes(y=a+WS_mean*b, color="WS")) +
  geom_ribbon(aes(ymin = (a+WS_mean*b)-(a+WS_sd*b), ymax = (a+WS_mean*b)+(a+WS_sd*b)), 
              fill = "#DD3C51", alpha=0.2) +
  scale_color_manual("", values = c("RH"="#313657", "WS"="#DD3C51"), label=labels2) +
  scale_y_continuous(name = "RH [%]", 
                     sec.axis = sec_axis(~ (. - a)/b, 
                                         name = parse(text = "u ~~ group('[', m * ~~ s^-1, ']')"), 
                                         breaks = seq(0,12,3))) +
  scale_x_continuous(name=NULL, limits = c(0,23), breaks = c(0,6,12,18,23)) +
  theme_bw() +
  theme(axis.title = element_text(),
        axis.text.x=element_blank(),
        axis.ticks.length=unit(-0.12, "cm"),
        axis.text.y = element_text(margin=margin(5,7,5,5,"pt")),
        axis.text.y.right = element_text(margin=margin(5,5,5,7,"pt"), color = "#DD3C51"),
        axis.line.y.right = element_line(color = "#DD3C51"), 
        axis.ticks.y.right = element_line(color = "#DD3C51"),
        axis.title.y.right = element_text(color = "#DD3C51"),
        legend.position = c(0.18,0.75),
        legend.background = element_rect(fill = "transparent"))
p2
# sin, sout, lin and lout
ylim.prim2 <- c(-100, 1000)   # in this example, sin
ylim.sec2 <- c(0.1, 0.9)    # in this example, u
b_1 <- diff(ylim.prim2)/diff(ylim.sec2)
a_1 <- ylim.prim2[1] - b*ylim.sec2[1] # there was a bug here

p3 <- ggplot(data = hourly, aes(x=hour)) +
  geom_hline(yintercept=0, linetype="dashed", color = "grey50", size=.5) +
  geom_point(aes(y=Sin_mean, color="Sin")) + 
  geom_line(aes(y=Sin_mean, color="Sin")) +
  geom_point(aes(y=Sout_mean, color="Sout")) +
  geom_line(aes(y=Sout_mean, color="Sout")) +
  geom_point(aes(y=Lin_mean, color="Lin")) +
  geom_line(aes(y=Lin_mean, color="Lin")) +
  geom_point(aes(y=Lout_mean, color="Lout")) +
  geom_line(aes(y=Lout_mean, color="Lout")) +
  scale_color_manual("", values = c("Sin"="#DD3C51", 
                                    "Sout"="#313657",
                                    "Lin"="#1F6683",
                                    "Lout"="#6C90B9",
                                    "cf"="#D1C7B5"), label=labels3) +
  geom_ribbon(aes(ymin = Sin_mean-Sin_sd, ymax = Sin_mean+Sin_sd), 
              fill = "#DD3C51", alpha=0.2) +
  geom_ribbon(aes(ymin = Sout_mean-Sout_sd, ymax = Sout_mean+Sout_sd), 
              fill = "#313657", alpha=0.2) +
  geom_ribbon(aes(ymin = Lin_mean-Lin_sd, ymax = Lin_mean+Lin_sd), 
              fill = "#1F6683", alpha=0.2) +
  geom_ribbon(aes(ymin = Lout_mean-Lout_sd, ymax = Lout_mean+Lout_sd), 
              fill = "#6C90B9", alpha=0.2) +
  geom_point(aes(y=a_1+cf_mean*b_1, color="cf")) +
  geom_line(aes(y=a_1+cf_mean*b_1, color="cf")) +
  geom_ribbon(aes(ymin = (a_1+cf_mean*b_1)-(a_1+cf_sd*b_1), ymax = (a_1+cf_mean*b_1)+(a_1+cf_sd*b_1)), 
              fill = "#D1C7B5", alpha=0.2) +
  scale_y_continuous(name = expression("Energy flux"~ "[W" ~ m^-2~"]"), 
                     sec.axis = sec_axis(~ (. - a_1)/b_1, 
                                         name = "CF")) +
  scale_x_continuous(name=NULL, limits = c(0,23), breaks = c(0,6,12,18,23)) +
  theme_bw() +
  theme(axis.title = element_text(),
        axis.text.x=element_blank(),
        axis.ticks.length=unit(-0.12, "cm"),
        axis.text.y = element_text(margin=margin(5,7,5,5,"pt")),
        axis.text.y.right = element_text(margin=margin(5,5,5,7,"pt"), color = "#D1C7B5"),
        axis.line.y.right = element_line(color = "#D1C7B5"), 
        axis.ticks.y.right = element_line(color = "#D1C7B5"),
        axis.title.y.right = element_text(color = "#D1C7B5"),
        legend.position = c(0.17,0.73),
        legend.background = element_rect(fill = "transparent"))

# h, le and Rib
p4 <- ggplot(data = hourly, aes(x=hour)) +
  geom_hline(yintercept=0, linetype="dashed", color = "grey50", size=.5) +
  geom_point(aes(y=H_mean, color="H")) + 
  geom_line(aes(y=H_mean, color="H")) +
  geom_point(aes(y=LE_mean, color="LE")) +
  geom_line(aes(y=LE_mean, color="LE")) +
  scale_color_manual("", values = c("H"="#DD3C51", 
                                    "LE"="#313657",
                                    "Rib"="#1F6683"), label=labels4) +
  geom_point(aes(y=R_b_mean*900, color="Rib")) +
  geom_line(aes(y=R_b_mean*900, color="Rib")) +
  scale_y_continuous(name = expression("Energy flux"~ "[W" ~ m^-2~"]"), 
                     sec.axis = sec_axis(~. /900, 
                                         name = expression(italic("R"["ib"])))) +
  geom_ribbon(aes(ymin = H_mean-H_sd, ymax = H_mean+H_sd), 
              fill = "#DD3C51", alpha=0.2) +
  geom_ribbon(aes(ymin = LE_mean-LE_sd, ymax = LE_mean+LE_sd), 
              fill = "#313657", alpha=0.2) +
  geom_ribbon(aes(ymin = (R_b_mean**900)-(R_b_sd*900), ymax = (R_b_mean*900)+(R_b_sd*900)), 
              fill = "#1F6683", alpha=0.2) +
  geom_text(data = hourly, label="stable", x = 20, y = 105, colour="#1F6683", size=3) +
  geom_text(data = hourly, label="unstable", x = 20, y = -160, colour="#1F6683", size=3) +
  scale_x_continuous(name=NULL, limits = c(0,23), breaks = c(0,6,12,18,23)) +
  theme_bw() +
  theme(axis.ticks.length=unit(-0.12, "cm"),
        axis.text.x=element_blank(),
        axis.text.y = element_text(margin=margin(5,7,5,5,"pt")),
        axis.text.y.right = element_text(margin=margin(5,5,5,7,"pt"), color = "#1F6683"),
        axis.line.y.right = element_line(color = "#1F6683"), 
        axis.ticks.y.right = element_line(color = "#1F6683"),
        axis.title.y.right = element_text(color = "#1F6683"),
        legend.position = c(0.18,0.3),
        legend.background = element_rect(fill = "transparent"))
  

# Rnet, Fsurf
p5 <- ggplot(data = hourly, aes(x=hour)) +
  geom_hline(yintercept=0, linetype="dashed", color = "grey50", size=.5) +
  geom_point(aes(y=R_mean, color="Rnet")) + 
  geom_line(aes(y=R_mean, color="Rnet")) +
  geom_point(aes(y=turblent_combined_mean, color="H+LE")) +
  geom_line(aes(y=turblent_combined_mean, color="H+LE")) +
  geom_point(aes(y=F_mean, color="Fsurf")) +
  geom_line(aes(y=F_mean, color="Fsurf")) +
  scale_color_manual("", values = c("Rnet"="#DD3C51", 
                                    "H+LE"="#313657",
                                    "Fsurf"="#1F6683"), label=labels5) +
  geom_ribbon(aes(ymin = R_mean-R_sd, ymax = R_mean+R_sd), 
              fill = "#DD3C51", alpha=0.2) +
  geom_ribbon(aes(ymin = turblent_combined_mean-turblent_combined_sd, ymax = turblent_combined_mean+turblent_combined_sd), 
              fill = "#313657", alpha=0.2) +
  geom_ribbon(aes(ymin = F_mean-F_sd, ymax = F_mean+F_sd),
              fill = "#1F6683", alpha=0.2) +
  labs(y=expression("Energy flux"~ "[W" ~ m^-2~"]")) +
  scale_x_continuous(name="Local time [IST]", limits = c(0,23), breaks = c(0,6,12,18,23)) +
  theme_bw() +
  theme(axis.ticks.length=unit(-0.12, "cm"),
        axis.text.y = element_text(margin=margin(5,7,5,5,"pt")),
        axis.text.x = element_text(margin=margin(7,5,5,5,"pt")),
        legend.position = c(0.22,0.85),
        legend.background = element_rect(fill = "transparent"))

# Grid
plot_grid(p1,p2,p3,p4,p5, ncol = 1, align = "v", rel_heights = c(2.5,2.5,2.5,2.5,3))


# Save
ggsave("./DJFMA_new_files/djfma_hourly.jpeg", units="in", width=3.5, height=10, dpi=300)







  



