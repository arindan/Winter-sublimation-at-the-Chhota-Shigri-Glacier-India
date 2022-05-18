
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


# Precipitation data filter: NA values
daily_djfma_removed_na <- daily_djfma %>%
  select(Date2,p_cumsum,hydro_year) %>%
  drop_na()

# Data: mean
daily_djfma2 <- readxl::read_excel("./DJFMA_new_files/revised_plots/data_rev_plots_git/Fig_4_Fig_S2_daily_djfma.xlsx", sheet = 2)
daily_djfma2$date3 <- as.Date(daily_djfma2$date3, origin = "1899-12-30")

# Plot
ggplot() +
  geom_line(data = daily_djfma_removed_na, aes(x = Date2, y = p_cumsum, colour = hydro_year)) +
  geom_line(data = daily_djfma2, aes(x = date3, y = Mean, color='Mean'), size=1) +
  scale_colour_brewer(palette = "Dark2", direction = -1) +
  labs(x=NULL,
       y="Cumulative precipitation [mm]",
       colour=NULL) +
  theme_bw() +
  theme(legend.position = c(0.2,0.7),
        legend.background = element_rect(fill = "transparent"),
        axis.ticks.length=unit(-0.12, "cm"),
        axis.text.y = element_text(margin=margin(5,7,5,5,"pt")),
        axis.text.x = element_text(margin=margin(7,5,5,5,"pt")))

# save
ggsave("./DJFMA_new_files/p_cumsum.jpeg", units="in", width=3, height=3, dpi=300)

