##################################################
## Project: Climate Hotspots
## Script purpose:Preprocess Data for Plotting
## Date:
## Author:
##################################################

library(tidyverse)
library(ggthemes)
library(cmocean)

df <- readRDS("rcp6p0_data.rds")

# appereantyl I did all of this
df <- df %>% filter(total_har > 0)
df$share_rainfed <- df$total_har_r / df$total_har
df  <- df %>%  mutate( bin_share_rainfed=cut_width(share_rainfed, width=0.02, boundary=0) )
df$cal_yld <- df$cal_yld_i + df$cal_yld_r


df$growth_rate <- ((df$cal_yld_rcp6p0_2080sH_i + df$cal_yld_rcp6p0_2080sH_r) - df$cal_yld) / df$cal_yld
df$growth_rate[is.infinite(df$growth_rate)] <- 0  
df$growth_rate[df$cal_yld == 0] <- 0

# does this make sense
df$productivity <- df$cal_yld / (df$total_har * 1000)  

ggplot(df, aes(x=productivity)) +
  geom_histogram(bins=200) +
  theme_hc()


# next one
new_population <- read.csv("G:/Meine Ablage/agricultural_sustain/population/population_country_2020.csv")

new_population[, c("x", "y")] <- round(new_population[, c("x", "y")], 3)

df <- left_join(df, new_population)

sum(df$pop_2020)# only consider cells where there is agricultural production therefore population will be smaller

# add the new cols 

data_2050 <- readRDS("rcp6p0_2050_data.rds")

df <- left_join(df, data_2050)

df$growth_rate_50 <- ((df$cal_yld_rcp6p0_2050sH_i + df$cal_yld_rcp6p0_2050sH_r) - df$cal_yld) / df$cal_yld

df$growth_rate_50[is.infinite(df$growth_rate_50)] <- 0  
df$growth_rate_50[df$cal_yld == 0] <- 0

df$cal_dif_50 <-(df$cal_yld_rcp6p0_2050sH_i + df$cal_yld_rcp6p0_2050sH_r) - 
  (df$cal_yld)

