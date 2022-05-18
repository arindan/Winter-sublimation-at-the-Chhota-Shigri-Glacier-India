
# This code is used to analyse data or generating figures for Mandal et al., 2022
# Pre-print link of the paper: https://tc.copernicus.org/preprints/tc-2021-386/

###############################################################
# Model SEB using AWS datasets
# Site: Chhota Shigri Glacier, AWS-M at 4863 m a.s.l.
#
# Created:          19/02/2021
# Latest Revision:  18/05/2022
#
# Arindan Mandal/IISc, Bangalore | Email: arindan.141@gmail.com

# Clear workspace and import required libraries
rm(list = ls())
library(dplyr)
library(readxl)
library(lubridate)
library(reshape2)
library(dplyr)
library(ggpmisc)
require(Metrics)
library(zoo)
library(patchwork)
library(ggExtra)
library(cowplot)

# Set WD
setwd("G:/Arindan Work/2 Chapter_Energy balance")                      # Windows
setwd("/Users/arindanmandal/Pictures/2 Chapter_Energy balance")        # Mac OSX
setwd("/home/icwar/Documents/Mandal_files/2 Chapter_Energy balance")   # Linux

# Data using here is night-filled version
# See "night_fillAWSm.R" for more details; after filling, old columns removed and replaced with filled columns

# LOAD 30-min/hourly meteorological datasets: pre-processed data
data <- read_excel("/home/icwar/Documents/Mandal_files/2 Chapter_Energy balance/DJFMA_new_files/revised_plots/SEB_in_R_filled.xlsx")
df <- as.data.frame(data) # Make a dataframe for easy going

# Datetime
df$Timesteps <- ymd_hms(df$Timesteps) 

# Raw data related information: 
# From 22.02.2015 to 02.10.2016 no night values due to battery problem not data storage,
# For SW, all values <5 W m-2 forced to 0 W m-2, sensor uncertainty
# Entire dataset starts from 1 Oct 2009 to 6 Oct 2020; ~3657 days; 11 years

# SENSITIVITY ANALYSIS
# SKIP this when running: Normal
# Run it for Sensitivity analysis: Tair (+-1°C), WS (10%), RH (10%) and snow roughness length (?0.001 m)
# MAKE SURE: You re-ran the fresh DF in L29, so that you get fresh/raw data 
df$Tair <- df$Tair+1             # +1°C change
df$Tair <- df$Tair-1             # -1°C change 
df$RH <- df$RH+(df$RH*10/100)    # +10% change
df$RH <- df$RH-(df$RH*10/100)    # -10% change
df$WS <- df$WS+(df$WS*10/100)    # +10% change
df$WS <- df$WS-(df$WS*10/100)    # -10% change
df$Ts <- df$Ts+1                 # +1°C change
df$Ts <- df$Ts-1                 # -1°C change 


# CONSTANTS FOR SEB 
elev_aws = 4863             # Elevation of AWS-M (m a.s.l.)
grav = 9.81                 # Gravitational acceleration (m s^-2)
k = 0.40                    # Von Karman Constant
z0_m = 0.001                # Roughness length for snow (m; Azam et al. 2014)
#z0_m = z0_m+0.001          # +0.001 m change in roughness length
#z0_m = z0_m+0.002          # Standard deviation mentioned in  Azam et al. 2014 (0.003 m)
#z0_m = z0_m+0.003          # Standard deviation mentioned in  Azam et al. 2014 (0.003 m)
#z0_m = 0.0005              # Suggestion by P Wagnon
z0_t = z0_m                # Surface roughness parameters for temperature (m; Azam et al. 2014)
z0_q = z0_m                # Surface roughness parameters for moisture (m; Azam et al. 2014)
cpd = 1005                  # Specific heat capacity of dry air at constant pressure (J/kg)
Lv = 2.514*10**6            # Latent heat of vaporization (J/kg)
Ls = 2.834*10**6            # Latent heat of sublimation (J/kg)
Lf = 3.337*10**5            # Latent heat of fusion (J/kg)
rho_i = 910                 # Ice density (kg/m3)
rho_w = 1000                # Density of water (kg m^-3)
Rgas = 8.31447              # Gas constant (J mol^-1 K^-1)
boltz = 5.67*10**-8         # Stefan Boltzmann constant
z_u = 3                     # Measurement height of wind speed sensor (m) 
z_t = 1.5                   # Measurement height of temperature and rh (m)
p0 = 101325                 # Standard sea level pressure (Pa)
T0 = 288.15                 # Standard sea level temperature (K)
Mair = 0.0289644            # Molar mass of dry air (kg mol^-1)
Lapse = 0.0065              # % Temperature lapse rate (K m^-1)
sec_day = 86400             # Seconds in a day


#########################################################################
# MAKE RELEVANT TIMESERIES FOR SEB

# Surface temperature from Lout; Ts_Lout (using Stefan-Boltzmann law)
df$Ts_Lout <- (df$Lout / boltz)^(1/4) - 273.15

# Relative humidity (RH) correction
df$RH[df$RH > 100] <- 100
df$RH[df$RH < 10]  <- 10  #n=8070

# Net shortwave (Snet), Net longwave (Lnet) and Net radiation (R)
df$Snet <- df$Sin-df$Sout
df$Lnet <- df$Lin-df$Lout
df$R <- df$Snet+df$Lnet

# Accumulated Albedo (from Van den Broeke et al. 2004)
acc_albedo <- read_xlsx(path = "./SEB_in_R_filled_albedo.xlsx", sheet = "Combined", range = cell_cols("I"))
acc_albedo <- data.frame(acc_albedo)
df <- cbind(df,acc_albedo)                 # Combine th albedo column with existing DF
rm(acc_albedo)                             # Remove the file for keeping space free    
df$Albedo_acc <- as.numeric(df$Albedo_acc) # Make everything numeric in the column
df$Albedo_acc[df$Albedo_acc>1] <- 0.90     # Convert super high albedo values to 0.9, general practice

# Cloud factor (DF) using Sin and STOA
stoa <- read.csv("./DJFMA_new_files/stoa_halfhourly.csv")
stoa <- stoa %>% select(stoa)
df <- cbind(df,stoa)
rm(stoa)                            # Remove the file for keeping space free 
df$cf <- 1.3-1.4*(df$Sin/df$stoa)   # Following Sicart et al. 2010
df$cf[is.nan(df$cf)] <- NA
df$cf[df$cf >= 1]  <- NA
df$cf[df$cf <= 0] <- NA
df$cf[which(hour(df$Timesteps) %in% c(0:8, 17:23))] <- NA # Make all shade hours (0:8 and 17:23) are forced to NA 

# Filter only DJFMA data-points for SEB
df <- df %>%
  filter(month(Timesteps) %in% c(12,1,2,3,4)) %>% # n=76248
  filter(Albedo_acc >= 0.4)                       # n=73273
  #filter(Ts <= 0) %>%                            # n=71617

# Air pressure; Pair and air density; rho air (kg/m3)
p_a <- p0*((1-(Lapse*elev_aws/T0))^(grav*Mair/(Rgas*Lapse)))  # Following Steiner et al. (2018) : https://github.com/fidelsteiner/debriscoveredglaciers/blob/master/model_branch/d2DEB.R (Line No 142)
p_a_hpa <- p_a/100                                            # Covert to hPa/mbar (from Pa)
df$rho_air <- p_a/(T0*(df$Tair+273.15))                       # 273.15 is to covert Tair in Kelvin
 
# Stability correction for turbulent heat fluxes / bulk method

# Rib: Richardson number
df$R_b  <- (grav*(((df$Tair+273.15) - (df$Ts+273.15)) / (z_t - z0_m))) / ((df$Tair+273.15) * (df$WS / (z_u - z0_m))^2) # Based on Wagnon et al. 2003

# See some statistics of Rib for filtration 
range(df$R_b[is.finite(df$R_b)]) # to see range of Rib
length(df$R_b[df$R_b > 0.23])  # 6304
length(df$R_b[df$R_b < -0.40]) # 4478
# Total 10782 (14.71%) data-points beyond R_b -0.41 to 0.23

# Discard those 10782 rows from SEB
df <- df %>% filter(R_b >= -0.40 & R_b  <= 0.23) # n=62492

# PHI; Stability function is a non-dimensional stability function
df$R_b[df$WS==0] <- 0                                                # When u is 0, Rib is also 0
df$phi_stab[is.na(df$R_b)]<-NA

df$phi_stab[df$R_b>=0] <- (1-5*df$R_b[df$R_b>=0])^2
df$phi_stab[df$R_b<0] <- (1-16*df$R_b[df$R_b<0])^0.75
#df$phi_stab[df$R_b > 0.23 | df$R_b < -0.41] <- 1
# df$c_bt <- k^2 / log(z_u/z0_m) / log(z_t/z0_t) * df$phi_stab       # From Steiner et al., 2018

# Saturation vapour pressure of air (E) and surface (Es) and vapour pressure deficit (D in Kpa)
df$E  <- (df$RH/100)*0.6108*exp((21.875*df$Tair)/(df$Tair+265.5))*10      # Saturation vapour pressure [kPa](Tetens (1930) equation at level of humidity measurements) and converted to mbar
df$Es <- 0.6108*exp((21.875*df$Ts)/(df$Ts+265.5))*10                      # Actual vapour pressure [kPa](at level of humidity measurements) and converted to mbar
df$D  <- df$Es-df$E                                                       # Vapour pressure deficit in mbar

# Specific humidity of air (q) and surface (qs)
df$q    <- df$E*0.622/p_a_hpa
df$qs    <- df$Es*0.622/p_a_hpa

# Temperature difference (Tair-Ts) and specific humiity difference (q-qs)
df$Tair_Ts  <- df$Tair-df$Ts
df$q_qs     <- df$q-df$qs

# Specific heat capacity of humid air
df$Cp <- cpd * (1 + 0.84 * df$q)
df$Cp[df$Cp > 1010] <- mean(df$Cp)
range(df$Cp, na.rm=T)


#########################################################################
# CALCULATE TURBULENT HEAT FLUXES

# Turbulent sensible heat flux (H) 
df$H <- (df$rho_air * df$Cp * k^2 * df$WS * (df$Tair - df$Ts) * df$phi_stab) / (log(z_u/z0_m) * log(z_t/z0_t))  # From Wagnon et al. 2003
# df$H_ = df$rho_air * df$Cp * df$c_bt * df$WS * (df$Tair - df$Ts)                                              # sensible heat flux (W m-2)

range(df$H, na.rm = T)
mean(df$H, na.rm = T)
length(df$H[df$H <= -500]) # n=47; this was mostly when Ts > 0 dC
df$H[df$H <= -500] <- NA
length(df$H[df$H >  500]) # n=67 
length(df$H[df$H >= 0])*100/length(df$H) # 56%

# Turbulent latent heat flux (LE)
# First, define Sublimation or Evaporation: Ts<0, use Ls or Ts>0 use Lv
l_vs <- Lv + df$Tair*0
l_vs[which(df$Ts < 0)] <- Ls

df$LE <- df$rho_air * l_vs * k^2 * df$WS * (df$q - df$qs) * df$phi_stab / (log(z_u/z0_m) * log(z_t/z0_q))        # From Wagnon et al. 2003
#df$LE_ = df$rho_air * l_vs * df$c_bt * df$WS * (df$q - df$qs)                                                   # latent heat flux (W m-2)

range(df$LE)
length(which(df$LE <= -500)) # n=467
df$LE[df$LE <= -500] <- NA
length(which(df$LE >= 0))    # n=1349; When RH was > 90% or fully saturated atmosphere at 2m
mean(df$LE, na.rm = T)
length(df$LE[df$LE > 0])*100/length(df$LE) # LE > 0 for 2%

# relations of H and le, just to look quick
cor.test(df$q_qs, df$LE)

# H + LE
df$turblent_combined <- df$H+df$LE

# Correlation of Rnet and H + LE
lm=lm(R~turblent_combined,data=df) # Linear model
summary(lm)                        # R square value 0.6542
cor.test(df$R, df$turblent_combined)

# Residual energy; Fsurf
df$F <- df$Snet+df$Lnet+df$H+df$LE

# Sublimation (mm)
df$subli <- ifelse(df$LE < 0 & df$Ts < 0, -df$LE/Ls*sec_day, NA)   # 1000 if for m to mm/day conversion and '-' sign to covert to positive)
round(mean(df$subli, na.rm = T),1)
mean(df$LE, na.rm = T)

# Sublimation in clear-sky and overcast
round(mean(df$subli[df$cf >= 0.8], na.rm = T),1) # mean
round(sd(df$subli[df$cf >= 0.8], na.rm = T),1)   # sd


############################################################################## SEB DONE!
# Export final half-hourly SEB file FOR SELECTED COLUMNS
df_selected <- df %>%
  select(Timesteps,subli,WS,Tair,Ts,Sin,Lin,R,D,q,qs,E,LE,cf,F,Tair_Ts,q_qs)
write.csv(df_selected , "./DJFMA_new_files/djfma_df_selected_columns.csv")


# Subset data for DJFMA (1 Dec to 30 April; each year)
# To make daily mean of each variable
df_DJFMA_daily <- df %>%
  filter(month(Timesteps) %in% c(12,1,2,3,4)) %>%
  group_by(date(Timesteps)) %>%
  summarise_all(list(mean=mean), na.rm = TRUE) 

write.xlsx(df_DJFMA_daily, "./DJFMA_new_files/daily_djfma3.xlsx")


# To make Monthly mean of each variable
df_DJFMA_monthly <- df_DJFMA_daily %>%
  group_by(month(`date(Timesteps)`)) %>%
  summarise_all(list(mean=mean, sd=sd),
                na.rm = TRUE)

write.csv(df_DJFMA_monthly, "./DJFMA_new_files/Monthly_MET_SEB_filled2.csv")


# To make hourly mean of each variable
df_DJFMA_hourly <- df %>%
  group_by(hour(Timesteps)) %>%
  summarise_all(list(mean=mean, sd=sd), na.rm = TRUE)

write.xlsx(df_DJFMA_hourly, "./DJFMA_new_files/djfma_hourly.xlsx")


# To make hourly mean of overcast and clear-sky: for hourly clear overcast figure
# total data 9-16 hours is 19062
# overcast, cf > 0.8 (n=2226; DJFMA; 9-16 hours)
df_DJFMA_hourly_overcast <- df %>%
  mutate(hour_minute = as.numeric(format(Timesteps, "%H.%M"))) %>%
  filter(month(Timesteps) %in% c(12,1,2,3,4)) %>%
  filter(cf >= 0.8) %>%
  filter(hour_minute >= 9 & hour_minute <= 16) %>%
  group_by(hour_minute) %>%
  summarise_all(list(mean=mean, sd=sd), na.rm = TRUE)
  
write.xlsx(df_DJFMA_hourly_overcast, "./DJFMA_new_files/djfma_hourly_overcast2.xlsx")

# Clear-sky: cf , 0.2 (n=5650; DJFMA; 9-16 hours)
df_DJFMA_hourly_clear <- df %>%
  mutate(hour_minute = as.numeric(format(Timesteps, "%H.%M"))) %>%
  filter(month(Timesteps) %in% c(12,1,2,3,4)) %>%
  filter(cf <= 0.2) %>%
  filter(hour_minute >= 9 & hour_minute <= 16) %>%
  group_by(hour_minute) %>%
  summarise_all(list(mean=mean, sd=sd), na.rm = TRUE)

write.xlsx(df_DJFMA_hourly_clear, "./DJFMA_new_files/djfma_hourly_clear2.xlsx")

# For clear-sky Overcast scatter plot
# Figure 11
df_DJFMA_hourly_overcast <- df %>%
  mutate(hour_minute = as.numeric(format(Timesteps, "%H.%M"))) %>%
  filter(month(Timesteps) %in% c(12,1,2,3,4)) %>%
  filter(cf >= 0.8) %>%
  filter(hour_minute >= 9 & hour_minute <= 16) %>%
  select(Timesteps,H,LE,Tair_Ts,q_qs,WS) %>%
  drop_na()

write.xlsx(df_DJFMA_hourly_overcast, "./DJFMA_new_files/djfma_hourly_overcast_scatter.xlsx")

# Clear-sky: cf , 0.2 (n=5650; DJFMA; 9-16 hours)
df_DJFMA_hourly_clear <- df %>%
  mutate(hour_minute = as.numeric(format(Timesteps, "%H.%M"))) %>%
  filter(month(Timesteps) %in% c(12,1,2,3,4)) %>%
  filter(cf <= 0.2) %>%
  filter(hour_minute >= 9 & hour_minute <= 16) %>%
  select(Timesteps,H,LE,Tair_Ts,q_qs,WS) %>%
  drop_na()

write.xlsx(df_DJFMA_hourly_clear, "./DJFMA_new_files/djfma_hourly_clear_scatter.xlsx")


# SUBLIMATION Stats
# Daily subli
df_DJFMA_subli_daily <- df %>%
  group_by(date=date(Timesteps)) %>%
  summarise_at(vars(subli,LE),
             list(mean),
             na.rm = TRUE)
round(mean(df_DJFMA_subli_daily$subli),2)
round(sd(df_DJFMA_subli_daily$subli),2)
ggplot(data = df_DJFMA_subli_daily, aes(x=date, y=subli)) + geom_point() # To see
#write.xlsx(df_DJFMA_subli_daily, "./DJFMA_new_files/djfma_sublimation_daily.xlsx")

# Monthly subli
df_DJFMA_subli_monthly <- df_DJFMA_subli_daily %>%
  group_by(year(date), month(date)) %>%
  summarise_at(vars(subli),
               list(subli_sum=sum),
               na.rm = TRUE)
#write.csv(df_DJFMA_subli_monthly, "./DJFMA_new_files/djfma_sublimation_monthly.csv")
# Sum by 5 index interval to get sum of every hydro-year; using rollapply() from zoo
sum_per_hydroyear <- rollapply(df_DJFMA_subli_monthly$subli_sum, width = 5, by = 5, FUN = sum)
sum_per_hydroyear
round(mean(round(sum_per_hydroyear,1)),1)

