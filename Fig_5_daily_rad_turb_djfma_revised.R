
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
daily_djfma <- readxl::read_excel("./DJFMA_new_files/revised_plots/data_rev_plots_git/Fig_4_Fig_5_Fig_S2_daily_djfma.xlsx") 


# Date
colnames(daily_djfma)[1] <- "Date"
daily_djfma$Date <- as.Date(daily_djfma$Date, origin = "1899-12-30")

# Insert a new Date2 column (in case you would need date for later)
daily_djfma <- daily_djfma %>% 
  mutate(Date2 = Date,
         fake_year = if_else(lubridate::month(Date2) == 12, 1999, 2000))

# Now overwrite the date2 year
year(daily_djfma$Date2) = daily_djfma$fake_year
daily_djfma$hydro_year <- factor(daily_djfma$hydro_year, labels = unique(daily_djfma$hydro_year))

# Define the number of colors you want
nb.cols <- 11
mycolors <- colorRampPalette(brewer.pal(11, "RdBu"))(nb.cols)

#adding DJFMA rectangles
djfma <- data.frame(xmin = 3, xmax = 7, ymin = -Inf, ymax = Inf)

# size for stat_summary mean line
stat_sum_size=0.5

#Sin
p2 <- ggplot(daily_djfma) +
  geom_line(aes(x = Date2, y = Sin_mean, colour = hydro_year), alpha=0.5, show.legend = F) +
  stat_summary(aes(x = Date2, y = stoa_mean, group=1), fun = mean, geom="line", size=stat_sum_size) +
  stat_summary(aes(x = Date2, y = Sin_mean, group=1), fun = mean, geom="line", size=stat_sum_size) +
  scale_colour_manual(values = rev(mycolors)) +
  scale_y_continuous(limits = c(0,450), breaks = c(0,100,200,300,400)) +
  scale_x_date(date_labels = "%d-%b", 
               limits = c(lubridate::ymd("1999-12-01"), # left/start date
                          lubridate::ymd("2000-04-30"))) +
  labs(x = NULL, 
       y = expression("S"["in"] ~ "[W" ~ m^-2~"]")) +
  theme_bw() +
  theme(axis.title = element_text(),
        axis.text.x=element_blank(),
        axis.ticks.length=unit(-0.12, "cm"),
        axis.text.y = element_text(margin=margin(5,7,5,5,"pt"))) +
  theme(panel.border = element_rect(fill=NA,color="blue",linetype="solid", size=0.75)) +
  geom_text(data = daily_djfma, label="DJFMA", x = as.numeric(as.Date("1999-12-12")), y = 380, colour="blue", size=6) +
  geom_text(data = daily_djfma, label=expression("S"["TOA"]), x = as.numeric(as.Date("2000-01-30")), y = 320, colour="black", size=4)  


# Sout
p3 <- ggplot(daily_djfma) +
  geom_line(aes(x = Date2, y = Sout_mean, colour = hydro_year), alpha=0.5, show.legend = T) +
  stat_summary(aes(x = Date2, y = Sout_mean, group=1), fun = mean, geom="line", size=stat_sum_size) +
  scale_colour_manual(values = rev(mycolors)) +
  scale_y_continuous(limits = c(0,420), breaks = c(0,100,200,300,400)) +
  scale_x_date(date_labels = "%d-%b", 
               limits = c(lubridate::ymd("1999-12-01"), # left/start date
                          lubridate::ymd("2000-04-30"))) +
  labs(x = NULL, 
       y = expression("S"["out"] ~ "[W" ~ m^-2~"]"),
       colour = NULL) +
  theme_bw() +
  theme(axis.title = element_text(),
        axis.text.x=element_blank(),
        axis.ticks.length=unit(-0.12, "cm"),
        axis.text.y = element_text(margin=margin(5,7,5,5,"pt")),
        legend.position = c(0.48,0.78),
        legend.background = element_rect(fill = "transparent")) +
  theme(panel.border = element_rect(fill=NA,color="blue",linetype="solid", size=0.75)) +
  guides(color=guide_legend(nrow = 2))

# CF
p5 <- ggplot(daily_djfma) +
  geom_line(aes(x = Date2, y = cf, colour = hydro_year), alpha=0.5, show.legend = F) +
  stat_summary(aes(x = Date2, y = cf, group=1), fun = mean, geom="line", size=stat_sum_size) +
  scale_colour_manual(values = rev(mycolors)) +
  scale_x_date(date_labels = "%d-%b", 
               limits = c(lubridate::ymd("1999-12-01"), # left/start date
                          lubridate::ymd("2000-04-30"))) +
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.5,1)) +
  labs(x = NULL, 
       y = "CF") +
  theme_bw() +
  theme(axis.title = element_text(),
        axis.text.x=element_blank(),
        axis.ticks.length=unit(-0.12, "cm"),
        axis.text.y = element_text(margin=margin(5,7,5,5,"pt"))) +
  theme(panel.border = element_rect(fill=NA,color="blue",linetype="solid", size=0.75)) +
  geom_text(data = daily_djfma, label="DJFMA", x = as.numeric(as.Date("1999-12-10")), y = 400, 
            colour="blue", size=6)

# Lin
p6 <- ggplot(daily_djfma) +
  geom_line(aes(x = Date2, y = Lin_mean, colour = hydro_year), alpha=0.5, show.legend = F) +
  stat_summary(aes(x = Date2, y = Lin_mean, group=1), fun = mean, geom="line", size=stat_sum_size) +
  scale_colour_manual(values = rev(mycolors)) +
  scale_y_continuous(limits = c(100,350), breaks = c(100,200,300)) +
  
  scale_x_date(date_labels = "%d-%b", 
               limits = c(lubridate::ymd("1999-12-01"), # left/start date
                          lubridate::ymd("2000-04-30"))) +
  labs(x = NULL, 
       y = expression("L"["in"] ~ "[W" ~ m^-2~"]")) +
  theme_bw() +
  theme(axis.title = element_text(),
        axis.text.x=element_blank(),
        axis.ticks.length=unit(-0.12, "cm"),
        axis.text.y = element_text(margin=margin(5,7,5,5,"pt"))) +
  theme(panel.border = element_rect(fill=NA,color="blue",linetype="solid", size=0.75))

# Lout
p7 <- ggplot(daily_djfma) +
  geom_line(aes(x = Date2, y = Lout_mean, colour = hydro_year), alpha=0.5, show.legend = F) +
  stat_summary(aes(x = Date2, y = Lout_mean, group=1), fun = mean, geom="line", size=stat_sum_size) +
  scale_colour_manual(values = rev(mycolors)) +
  scale_y_continuous(limits = c(100,350), breaks = c(100,200,300)) +
  scale_x_date(date_labels = "%d-%b", 
               limits = c(lubridate::ymd("1999-12-01"), # left/start date
                          lubridate::ymd("2000-04-30"))) +
  labs(x = NULL, 
       y = expression("L"["out"] ~ "[W" ~ m^-2~"]")) +
  theme_bw() +
  theme(axis.text.x=element_blank(),
        axis.title = element_text(),
        axis.ticks.length=unit(-0.12, "cm"),
        axis.text.y = element_text(margin=margin(5,7,5,5,"pt"))) +
  theme(panel.border = element_rect(fill=NA,color="blue",linetype="solid", size=0.75))


#### Revision
# Rnet
p9 <- ggplot(daily_djfma) +
  geom_hline(yintercept=0, linetype="dashed", color = "grey50", size=.5) +
  geom_line(aes(x = Date2, y = R_mean, colour = hydro_year), alpha=0.5, show.legend = F) +
  stat_summary(aes(x = Date2, y = R_mean, group=1), fun = mean, geom="line", size=stat_sum_size) +
  scale_colour_manual(values = rev(mycolors)) +
  scale_x_date(date_labels = "%d-%b", 
               limits = c(lubridate::ymd("1999-12-01"), # left/start date
                          lubridate::ymd("2000-04-30"))) +
  labs(x = NULL, 
       y = expression("R"["net"] ~ "[W" ~ m^-2~"]"),
       color = "Year") +
  theme_bw() +
  theme(axis.title = element_text(),
        axis.text.x=element_blank(),
        axis.ticks.length=unit(-0.12, "cm"),
        axis.text.y = element_text(margin=margin(5,7,5,5,"pt"))) +
  theme(panel.border = element_rect(fill=NA,color="blue",linetype="solid", size=0.75)) 

#H
p10 <- ggplot(daily_djfma) +
  geom_hline(yintercept=0, linetype="dashed", color = "grey50", size=.5) +
  geom_line(aes(x = Date2, y = H_mean, colour = hydro_year), alpha=0.5, show.legend = F) +
  stat_summary(aes(x = Date2, y = H_mean, group=1), fun = mean, geom="line", size=stat_sum_size) +
  scale_colour_manual(values = rev(mycolors)) +
  scale_x_date(date_labels = "%d-%b", 
               limits = c(lubridate::ymd("1999-12-01"), # left/start date
                          lubridate::ymd("2000-04-30"))) +
  labs(x = NULL, 
       y = expression("H" ~ "[W" ~ m^-2~"]")) +
  theme_bw() +
  theme(axis.title = element_text(),
        axis.text.x=element_blank(),
        axis.ticks.length=unit(-0.12, "cm"),
        axis.text.y = element_text(margin=margin(5,7,5,5,"pt"))) +
  theme(panel.border = element_rect(fill=NA,color="blue",linetype="solid", size=0.75))

# LE
p11 <- ggplot(daily_djfma) +
  geom_hline(yintercept=0, linetype="dashed", color = "grey50", size=.5) +
  geom_line(aes(x = Date2, y = LE_RS_mean, colour = hydro_year), alpha=0.5, show.legend = F) +
  stat_summary(aes(x = Date2, y = LE_RS_mean, group=1), fun = mean, geom="line", size=stat_sum_size) +
  scale_colour_manual(values = rev(mycolors)) +
  scale_x_date(date_labels = "%d-%b", 
               limits = c(lubridate::ymd("1999-12-01"), # left/start date
                          lubridate::ymd("2000-04-30"))) +
  labs(x = NULL, 
       y = expression("LE" ~ "[W" ~ m^-2~"]")) +
  theme_bw() +
  theme(panel.border = element_rect(fill=NA,color="blue",linetype="solid", size=0.75),
        axis.ticks.length=unit(-0.12, "cm"),
        axis.text.y = element_text(margin=margin(5,7,5,5,"pt")),
        axis.text.x = element_text(margin=margin(7,5,5,5,"pt"))) +
  theme(panel.border = element_rect(fill=NA,color="blue",linetype="solid", size=0.75))


# Grid of all plots
plot_grid(p2,p3,p5,p6,p7,p9,p10,p11,
          ncol = 1, align = "v", rel_heights = c(0.9,0.9,0.5,0.9,0.9,0.9,0.9,1.1), labels = 'AUTO', label_x = 0.11, label_y = 0.95)


# Save
ggsave("./DJFMA_new_files/daily_rad_djfma_revised.jpeg", units="in", width=6.5, height=9, dpi=300)
