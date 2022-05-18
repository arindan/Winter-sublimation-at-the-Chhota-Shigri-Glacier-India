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
library(patchwork)

# Set WD
setwd("G:/Arindan Work/2 Chapter_Energy balance")                 # Windows
setwd("/Users/arindanmandal/Pictures/2 Chapter_Energy balance")   # Mac OSX
setwd("/home/icwar/Documents/Mandal_files/2 Chapter_Energy balance")   # Linux

# Data
data_clear    <- readxl::read_excel("./DJFMA_new_files/revised_plots/data_rev_plots_git/Fig_9_djfma_hourly_clear.xlsx", sheet = "plot_clear")
data_over     <- readxl::read_excel("./DJFMA_new_files/revised_plots/data_rev_plots_git/Fig_9_djfma_hourly_clear.xlsx", sheet = "plot_over")
data_clear_met <- readxl::read_excel("./DJFMA_new_files/revised_plots/data_rev_plots_git/Fig_9_djfma_hourly_clear.xlsx", sheet = 1)
data_over_met <- readxl::read_excel("./DJFMA_new_files/revised_plots/data_rev_plots_git/Fig_9_djfma_hourly_overcast.xlsx", sheet = 1)
data_clear_met$hour <- seq(9,16,0.5)
data_over_met$hour <- seq(9,16,0.5)

# factor
data_clear$variable <- factor(data_clear$variable, levels = unique(data_clear$variable))
data_over$variable  <- factor(data_over$variable, levels = unique(data_over$variable))

#subscript axis text and italics
labels_t <- expression(italic(""~T[air]),
                       italic("u"),
                       italic(""~R[ib]))

labels <- expression(italic("S"["in"]),
                     italic("S"["out"]),
                     italic("L"["in"]),
                     italic("L"["out"]))

labels_turb <- expression(italic("H"),
                     italic("LE"))

labels2 <- expression(italic(""~F[surface]))

# Plot

p1 <- ggplot() +
  geom_hline(yintercept=0, linetype="dashed", color = "grey10", size=.3) +
  geom_point(data = data_clear_met, aes(x=hour, y=Tair_mean, color="Tair")) +
  geom_point(data = data_clear_met, aes(x=hour, y=WS_mean, color="WS")) +
  geom_point(data = data_clear, aes(x=hour3, y=value3*150, color="Rib")) +
  scale_color_manual("", values = c("Tair"="steelblue4",
                                    "WS"="black", 
                                    "Rib"="#990000"),
                     label=labels_t) +
  scale_y_continuous(limits = c(-15, 10.5), breaks = seq(-15, 10.5, by = 3), sec.axis = sec_axis(~./150, name = NULL)) +
  labs(x = NULL,
       color=NULL,
       y = expression(~T[air]~"[?C]" ~","~ "u"~ "[m" ~ s^-1~"]"),
       title = "Clear-sky [CF < 0.2]") +
  scale_x_continuous(breaks = seq(from = 9, to = 16, by = 1)) +
  theme_bw() +
  theme(legend.position = c(0.45,0.95),
        legend.background = element_rect(fill = "transparent"),
        axis.ticks.length=unit(-0.12, "cm"),
        axis.text.y = element_text(margin=margin(7,7,7,7,"pt")),
        axis.text.x = element_text(margin=margin(7,5,5,5,"pt")),
        axis.text.y.right = element_text(angle = 90, hjust = 0, margin=margin(5,5,5,7,"pt"), color = "#990000"),
        axis.line.y.right = element_line(color = "#990000"), 
        axis.ticks.y.right = element_line(color = "#990000")) +
  guides(color=guide_legend(nrow = 1)) 

p2 <- ggplot() +
  geom_hline(yintercept=0, linetype="dashed", color = "grey10", size=.3) +
  geom_point(data = data_over_met, aes(x=hour, y=Tair_mean, color="Tair")) +
  geom_point(data = data_over_met, aes(x=hour, y=WS_mean, color="WS")) +
  geom_point(data = data_over, aes(x=hour3, y=value3*150, color="Rib")) +
  scale_color_manual("", values = c("Tair"="steelblue4",
                                    "WS"="black", 
                                    "Rib"="#990000"),
                     label=labels_t) +
  scale_y_continuous(limits = c(-15, 10.5), breaks = seq(-15, 10.5, by = 3), sec.axis = sec_axis(~./150, name = parse(text ="R[ib]"))) +
  labs(x = NULL,
       color=NULL,
       y = NULL,
       title = "Overcast [CF > 0.8]") +
  scale_x_continuous(breaks = seq(from = 9, to = 16, by = 1)) +
  theme_bw() +
  theme(legend.position = 'none',
        axis.ticks.length=unit(-0.12, "cm"),
        axis.text.y = element_text(margin=margin(5,7,5,5,"pt")),
        axis.text.x = element_text(margin=margin(7,5,5,5,"pt")),
        axis.text.y.right = element_text(angle = 90, hjust = 0, margin=margin(5,5,5,7,"pt"), color = "#990000"),
        axis.line.y.right = element_line(color = "#990000"), 
        axis.ticks.y.right = element_line(color = "#990000"),
        axis.title.y.right = element_text(color = "#990000"))

p3 <- ggplot() +
  geom_hline(yintercept=0, linetype="dashed", color = "grey10", size=.3) +
  geom_bar(data=data_clear, aes(x=hour, y=value, fill=variable), stat = "identity", alpha=0.9, show.legend = F) +
  ylim(-250,2250) +
  scale_fill_manual(name=NULL, values = c("Sin"="#DD3C51", "Sout"="#313657",
                                    "Lin"="#1F6683", "Lout"="#6C90B9"), label=labels) +
  labs(x = NULL,
       y = expression("Energy flux"~ "[W" ~ m^-2~"]"),
       title = NULL) +
  scale_x_continuous(breaks = seq(from = 9, to = 16, by = 1)) +
  theme_bw() +
  theme(axis.ticks.length=unit(-0.12, "cm"),
        axis.text.y = element_text(margin=margin(5,7,5,5,"pt")),
        axis.text.x = element_text(margin=margin(7,5,5,5,"pt")))

p4 <- ggplot() +
  geom_hline(yintercept=0, linetype="dashed", color = "grey10", size=.3) +
  geom_bar(data=data_over, aes(x=hour, y=value, fill=variable), stat = "identity", alpha=0.9) +
  ylim(-250,2250) +
  scale_fill_manual(name=NULL, values = c("Sin"="#DD3C51", "Sout"="#313657",
                                          "Lin"="#1F6683", "Lout"="#6C90B9"), label=labels) +
  labs(x = NULL,
       y = NULL,
       title = NULL) +
  scale_x_continuous(breaks = seq(from = 9, to = 16, by = 1)) +
  theme_bw() +
  theme(legend.position = c(0.5,0.8),
        axis.ticks.length=unit(-0.12, "cm"),
        axis.text.y = element_text(margin=margin(5,7,5,5,"pt")),
        axis.text.x = element_text(margin=margin(7,5,5,5,"pt")),
        legend.spacing.y = unit(-0.1, "cm"),
        legend.background = element_rect(fill = "transparent"),
        legend.key.size = unit(0.4, "cm")) +
  guides(fill=guide_legend(nrow=2))

p5 <- ggplot() +
  geom_hline(yintercept=0, linetype="dashed", color = "grey10", size=.3) +
  geom_bar(data=data_clear, aes(x=hour4, y=value4, fill=variable4), stat = "identity", alpha=0.9, show.legend = F) +
  geom_point(data = data_clear, aes(x=hour2, y=value2, color="Fsurf"), show.legend = F) +
  ylim(-300,50) +
  scale_fill_manual(name=NULL, values = c("H"="#BA2F00", 
                                          "LE"="#21282F"), label=labels_turb) +
  scale_color_manual("", label=labels2, values = c("Fsurf"="black")) +
  labs(x = "Local time [IST]",
       y = expression("Turbulent flux"~ "[W" ~ m^-2~"]"),
       title = NULL) +
  scale_x_continuous(breaks = seq(from = 9, to = 16, by = 1)) +
  theme_bw() +
  theme(axis.ticks.length=unit(-0.12, "cm"),
        axis.text.y = element_text(margin=margin(5,7,5,5,"pt")),
        axis.text.x = element_text(margin=margin(7,5,5,5,"pt")))

p6 <- ggplot() +
  geom_hline(yintercept=0, linetype="dashed", color = "grey10", size=.3) +
  geom_bar(data=data_over, aes(x=hour4, y=value4, fill=variable4), stat = "identity", alpha=0.9) +
  geom_point(data = data_over, aes(x=hour2, y=value2, color="Fsurf")) +
  ylim(-300,50) +
  scale_fill_manual(name=NULL, values = c("H"="#BA2F00", 
                                          "LE"="#21282F"), label=labels_turb) +
  scale_color_manual("", label=labels2, values = c("Fsurf"="black")) +
  labs(x = "Local time [IST]",
       y = NULL,
       title = NULL) +
  scale_x_continuous(breaks = seq(from = 9, to = 16, by = 1)) +
  theme_bw() +
  theme(legend.position = c(0.5,0.3),
        axis.ticks.length=unit(-0.12, "cm"),
        axis.text.y = element_text(margin=margin(5,7,5,5,"pt")),
        axis.text.x = element_text(margin=margin(7,5,5,5,"pt")),
        legend.spacing.y = unit(-0.1, "cm"),
        legend.background = element_rect(fill = "transparent"),
        legend.key.size = unit(0.4, "cm")) +
  guides(fill=guide_legend(nrow=1))

# arrange all
plot_grid(p1,p2,p3,p4,p5,p6, ncol = 2, align = "v", labels = "AUTO", label_x = 0.1, label_size = 12)


# save
ggsave("./DJFMA_new_files/djfma_hourly_clear_overcast.jpeg", units="in", width=6.5, height=6.5, dpi=300)

