
# This code is used to analyse data or generating figures for Mandal et al., 2022
# Pre-print link of the paper: https://tc.copernicus.org/preprints/tc-2021-386/

# Import packages 
rm(list = ls())
library(ggplot2)

# Set WD
setwd("G:/Arindan Work/2 Chapter_Energy balance")                 # Windows
setwd("/Users/arindanmandal/Pictures/2 Chapter_Energy balance")   # Mac OSX
setwd("/home/icwar/Documents/Mandal_files/2 Chapter_Energy balance")   # Linux

# Data
data <- readxl::read_xlsx("./DJFMA_new_files/revised_plots/data_rev_plots_git/Fig_14_sensitivity_data_DJFMA.xlsx")

# Lock in factor level order
data$Perturbation <- factor(data$Perturbation, levels = unique(data$Perturbation))

# Plot Sensitivity Analysis
ggplot(data=data, aes(x=change_percent, y=Perturbation)) + 
  geom_bar(width = 0.7, alpha=0.55, fill="darkblue", stat = "identity") +
  geom_vline(xintercept=0, linetype="dashed", color = "grey10", size=.3) +
  labs(x="Change in cumulative sublimation [%]",
       y="Perturbation") +
  scale_x_continuous(limits = c(-20, 25), breaks = seq(-20, 25, by = 10)) +
  theme_bw() +
  theme(axis.ticks.length=unit(-0.12, "cm"),
        axis.text.y = element_text(margin=margin(5,7,5,5,"pt")),
        axis.text.x = element_text(margin=margin(7,5,5,5,"pt")))

# Save
ggsave("./DJFMA_new_files/Sensitivity.jpeg", units="in", width=4, height=3.5, dpi=300)   

