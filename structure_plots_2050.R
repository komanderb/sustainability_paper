##################################################
## Project: Climate Hotspots Paper
## Script purpose: Step by Step through the paper plots for 2050
## Date:
## Author:
##################################################
library(tidyverse)
library(ggthemes)
library(cmocean)
library(countrycode)
library(raster)
library(cowplot)
library(rnaturalearth)
library(rnaturalearthdata)
source("plot_data.R", local = TRUE)
# Heterogenous Effects on Agriculture

#### PREPARE DATA AND VARIABLES
SUBTITLE = "Under RCP 6.0 for the period 2040-2070 projected to today"
map_font_theme <- theme_map()$text
cmocean_colors <- cmocean('curl', direction=-1, clip=0.1)(100)
curl_colors <- cmocean("curl", clip=0.1)
# Extract only one half (positive side) of the colormap
half_curl_colors <- curl_colors(100)[51:100]

## Convert to People fed yearly ------
df$pfy_dif <- df$cal_dif_50 / (365*2000)

# Country Data ---- 

country_data <- df %>%
  group_by(ISO_A3) %>%
  summarise(cal_dif = sum(cal_dif_50, na.rm = T), 
            cal_yld = sum(cal_yld, na.rm = T), 
            future_cal_yld = sum(cal_yld_rcp6p0_2050sH_i + cal_yld_rcp6p0_2050sH_r, na.rm = T))
country_data$country = countrycode(country_data$ISO_A3, origin = "iso3c", destination = "country.name")
country_data <- country_data[!(is.na(country_data$country)),]
country_data$cal_growth_rate <- ((country_data$future_cal_yld - country_data$cal_yld) / country_data$cal_yld)
high <- country_data[-3] %>%
  top_n(n=10, cal_dif)
low <- country_data[-3] %>%
  top_n(n=-10, cal_dif)
country_df <- rbind(high, low)
country_df$color <- ifelse(country_df$cal_dif < 0, "negative","positive")
country_df$country <- countrycode(country_df$ISO_A3, origin = "iso3c", destination = "country.name")

## Calories Per Person ------- 
df$cal_cap <- (df$cal_yld / 365) / df$pop_2020 
df$cal_cap_future <- ((df$cal_yld_rcp6p0_2050sH_i + df$cal_yld_rcp6p0_2050sH_r) / 365) / df$pop_2020 

df$cal_cap[is.na(df$cal_cap)] <- 0
df$cal_cap[is.infinite(df$cal_cap)] <- 0
df$cal_cap_future[is.na(df$cal_cap_future)] <- 0
df$cal_cap_future[is.infinite(df$cal_cap_future)] <- 0  

## Location of Hotspots ----- 
df$pfy_dummy <- ifelse(df$pfy_dif < -5000, 1, 0)
df$growth_dummy <- ifelse(df$growth_rate_50 < -0.15, 1, 0)
df_dummy <- df %>% filter(pfy_dummy == 1)
df_growth_dummy <- df %>% filter(growth_dummy == 1)
df$subsistence_dummy <- ifelse((df$cal_cap > 2000 & df$cal_cap_future < 2000), 1, 0)
df_subsistence <- df %>% filter(subsistence_dummy == 1)

## Population in Danger -------

df$pfy <- df$cal_yld / (365*2000)
df$pfy_50 <- df$pfy + df$pfy_dif
df$si_0 <- ifelse(df$pop_2020 > df$pfy, (df$pfy / df$pop_2020), 1)
df$si_50 <- ifelse(df$pop_2020 > df$pfy_50, (df$pfy_50 / df$pop_2020), 1)
df$population_in_danger <- (df$si_0 - df$si_50) * df$pop_2020
df$population_in_danger[df$population_in_danger < 0] <- 0
pid_df<- df %>% filter(population_in_danger > 0)
pid_df$population_in_danger[pid_df$population_in_danger < 1] <- 1 

df$population_in_danger[df$population_in_danger < 1] <- 1 

breaks <- c(0, 0.05, 0.1, 0.3, 0.5, 1)
breaks <- 1- rev(breaks)
x <- pid_df$population_in_danger
return_vector <- rep(0, length(x))

qs <- quantile(x, breaks, na.rm = TRUE)
labels <- tapply(x, findInterval(x, qs), mean)
return_x <- cut(ntile(x, 100), breaks = breaks*100, labels = labels[-length(labels)])
return_x <- as.numeric(as.character(return_x))
#return_x <- as.numeric(levels(return_x))[x]
return_x <- round(return_x)
pid_df$pid_levels <- return_x
left_join(df, pid_df)
result <- df %>%
  left_join(dplyr::select(pid_df, x, y,  pid_levels), by=c("x"="x", "y"="y"))
ggplot(pid_df) +
  geom_point(aes(x=population_in_danger, y=pfy_dif))

# PLOTS -----------------------------------------

## Population in Danger Map ------ 
result$pid_levels[is.na(result$pid_levels)] <- 0
sort_values <- sort(round(unique(result$pid_levels)))
my_color <- cmocean(name = 'curl', direction = 1, start = 0.5, end = 0.9)(6)

# Print the resulting colors
names(my_color) <- as.character(sort_values)

out_plot <- ggplot(result, aes(x, y)) +
  geom_tile(aes(fill = as.character(pid_levels))) +
  xlim(-180, 180) +
  ylim(-70, 90) +
  coord_equal() +
  scale_fill_manual(values = my_color, name = "Bin Mean",
                    labels = sort_values)  +
  theme_map() +
  theme(legend.direction = 'vertical',
        legend.position = c(.05, .25),
        legend.spacing.y = unit(0, "pt"),
        legend.title = element_text(size = 9, color = "#4e4d47"),
        legend.text = element_text(size = 7, color = "#4e4d47"),
        plot.caption = element_text(size = 6, 
                                    hjust = 0.92, 
                                    margin = margin(t = 0.2, 
                                                    b = 0, 
                                                    unit = "cm"), 
                                    color = "#939184"),
        plot.title = element_text(hjust = 0.5, color = "#4e4d47"),
        plot.subtitle = element_text(hjust = 0.5, color = "#4e4d47")) +
  labs(x = NULL, 
       y = NULL, 
       title = paste("Population in Danger"), 
       subtitle = "For the period 2040-2070 projected to today")

out_plot

pdf_name <-"population_in_danger_map.pdf"
#save_plot(pdf_name, out_plot, base_height = 7,dpi = 300) #if pdf="print"
base_height <- 7

# Calculate the width based on the aspect ratio of the plot (default is typically 1.5:1)
aspect_ratio <- 1.5  # Adjust based on your plot’s desired aspect ratio
base_width <- base_height * aspect_ratio
ggsave(pdf_name, plot = out_plot, width = base_width, height = base_height, dpi = 300)

## Density Plot Growth --------
ggplot(df, aes(x = growth_rate_50, fill = (growth_rate_50 >= 0))) +
  geom_histogram(binwidth = 0.01, position = "identity", alpha = 0.65) +
  xlim(-1, 2) +
  labs(title = "Distribution of Produced Calories Growth Rates",
       subtitle = SUBTITLE,
       x = "Growth Rate", 
       y = "", 
       fill = "Growth Rate") +
  scale_fill_manual(
    values = c("#BC5060FF", "#1A8A7DFF"),
    labels = c("FALSE" = "negative", "TRUE" = "non-negative")
  ) + 
  #theme(plot.subtitle = element_text(color = "grey45", face = "italic"),
  #      axis.title.x = element_text(color = "grey45"),
  #      axis.title.y.left = element_text(color = "grey45")) +
  theme(
    legend.title = element_text(family = map_font_theme$family, size = 9, color = "#4e4d47"),
    legend.text = element_text(family = map_font_theme$family, size = 7, color = "#4e4d47"),
    text = map_font_theme,                 # Apply map font to all text
    axis.title = element_text(family = map_font_theme$family, hjust = 0.5, color = "#4e4d47"),   # Axis title font from theme_map
    plot.title = element_text(family = map_font_theme$family, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "#4e4d47")# Plot title font from theme_map
  ) +
  theme(
    axis.ticks.y = element_blank(),     # Removes y-axis ticks
    axis.text.y = element_blank()       # Removes y-axis text labels
  ) + 
  theme_hc() 

ggsave("density_growth_rate_2050_2.pdf", width = 6, height = 4)


## Density Plot Absolute Difference
ggplot(df, aes(x = pfy_dif, fill = (pfy_dif >= 0))) +
  geom_histogram(binwidth = 50, position = "identity", alpha = 0.65) +
  labs(title = "Distribution of Absolute Change in Produced Calories",
       subtitle = SUBTITLE,
       x = "Absolute Calorie Changes expressed in People Fed Yearly", 
       y = "", 
       fill = "Absolute Calorie Changes") +
  xlim(-1e4, 1e4) +
  scale_fill_manual(
    values = c("#BC5060FF", "#1A8A7DFF"),
    labels = c("FALSE" = "negative", "TRUE" = "non-negative")
  ) + 
  theme(
    legend.title = element_text(family = map_font_theme$family, size = 9, color = "#4e4d47"),
    legend.text = element_text(family = map_font_theme$family, size = 7, color = "#4e4d47"),
    text = map_font_theme,                 # Apply map font to all text
    axis.title = element_text(family = map_font_theme$family, color = "#4e4d47"),   # Axis title font from theme_map
    plot.title = element_text(family = map_font_theme$family, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "#4e4d47")# Plot title font from theme_map
  ) +
  theme(
    axis.ticks.y = element_blank(),     # Removes y-axis ticks
    axis.text.y = element_blank()       # Removes y-axis text labels
  ) +
  theme_hc()
  
ggsave("density_cal_dif_2050_2.pdf", width = 6, height = 4)

## Country considerations ---- 

country_df %>% arrange(cal_dif) %>% 
  ggplot(aes(x= reorder(country, cal_dif), y = (cal_dif / (365*2000*1000000)))) +
    geom_bar(stat = "identity", aes(fill = color)) +
    #geom_hline(aes(yintercept = 0), color = 'darkred', linetype = "dashed", size = 1) + 
    coord_flip() +
    scale_fill_manual(values=c(positive="#117D79FF",negative="#AE4060FF"), name="Change") +
    labs(title = "Aggregated Calorie Change",
         subtitle = SUBTITLE, 
         y = "Expressed in people fed yearly changes") +
  theme(
    legend.title = element_text(family = map_font_theme$family, size = 9, color = "#4e4d47"),
    legend.text = element_text(family = map_font_theme$family, size = 7, color = "#4e4d47"),
    text = map_font_theme,                 # Apply map font to all text
    axis.title = element_text(family = map_font_theme$family, color = "#4e4d47"),   # Axis title font from theme_map
    plot.title = element_text(family = map_font_theme$family, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "#4e4d47"), # Plot title font from theme_map
    axis.title.y.left = element_blank()) +
    theme_hc()

ggsave("calorie_change_top_countries_2050_2.pdf", width = 6, height = 4)

# Population -------------------------------------------

## Growth Rate Cumulative Population ------
sum(df$pop_2020[df$growth_rate_50 < -0.15])

plot <- df %>% filter(growth_rate_50 <= 1) %>%
  arrange(growth_rate_50) %>%
  mutate(cumulative_population = cumsum(pop_2020)) %>%
  ggplot(aes(x=growth_rate_50, y=cumulative_population)) +
  geom_line(size=0.75) +
  geom_vline(aes(xintercept=0), linetype='dashed', size=0.5, alpha=0.5, color='grey45')+
  geom_vline(aes(xintercept=-0.15), color='darkred', linetype='dashed', size=0.75) +
  annotate(x=-0.1,y=3e+09,label="972 Million People affected by Growth rate < -0.15",vjust=0.5,geom="label", size=4, color='darkred') +
  labs(title="Cumulative Population per Produced Calories Growth Rate", 
       subtitle = SUBTITLE, 
       y="Cumulative Population", x="Produced Calories Growth Rate") +
  theme(
    legend.title = element_text(family = map_font_theme$family, size = 9, color = "#4e4d47"),
    legend.text = element_text(family = map_font_theme$family, size = 7, color = "#4e4d47"),
    text = map_font_theme,                 # Apply map font to all text
    axis.title = element_text(family = map_font_theme$family, hjust = 0.5, color = "#4e4d47"),   # Axis title font from theme_map
    plot.title = element_text(family = map_font_theme$family, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "#4e4d47")# Plot title font from theme_map
  ) + 
  theme_hc()
save_plot("pop_growth_rate_50_2.pdf", plot, base_height=5, dpi="print")
#ggsave("pop_growth_rate_50.pdf", width = 6, height = 4)
# here we can also play with the share of rainfed agriculture
# for produced calories


## Absolute difference cumulative population ------ 
sum(df$pop_2020[df$pfy_dif < -5000])

plot <- df %>%
  arrange(pfy_dif) %>%
  mutate(cumulative_population = cumsum(pop_2020)) %>%
  ggplot(aes(x=pfy_dif, y=cumulative_population)) +
  geom_line(size=0.75) +
  geom_vline(aes(xintercept=0), linetype='dashed', size=0.5, alpha=0.5, color='grey45')+
  geom_vline(aes(xintercept=-5000), color='darkred', linetype='dashed', size=0.75) +
  annotate(x=-0.1,y=3e+09,label="1.75 Billion People affected by Absolute Difference of < -5000 PFY",vjust=0.5,geom="label", size=4, color='darkred') +
  xlim(-5e4, 5e4) +
  labs(title="Cumulative Population per Absolute Change in Produced Calories",
       subtitle=SUBTITLE, 
       y="Cumulative Population", x="Absolute Calorie Difference in People Fed Yearly") +
  theme(
    legend.title = element_text(family = map_font_theme$family, size = 9, color = "#4e4d47"),
    legend.text = element_text(family = map_font_theme$family, size = 7, color = "#4e4d47"),
    text = map_font_theme,                 # Apply map font to all text
    axis.title = element_text(family = map_font_theme$family, hjust = 0.5, color = "#4e4d47"),   # Axis title font from theme_map
    plot.title = element_text(family = map_font_theme$family, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "#4e4d47")# Plot title font from theme_map
  ) + 
  theme_hc()
save_plot("pop_cal_dif_50_2.pdf", plot, base_height=5, dpi="print")
#ggsave("pop_cal_dif_50.pdf", width = 6, height = 4)

## Below Subsistence Level Cumulative Population ------
sum(df$pop_2020[(df$cal_cap > 2000 & df$cal_cap_future < 2000)])
plot <- df %>% filter(cal_cap > 2000) %>%
  arrange(cal_cap_future) %>%
  mutate(cumulative_population = cumsum(pop_2020)) %>%
  ggplot(aes(x=cal_cap_future, y=cumulative_population)) +
  geom_line(size=0.75) +
  geom_vline(aes(xintercept=2000), linetype='dashed')+
  annotate(x=2000,y=3e+08,label="109 Million People move below subsistence level",vjust=0.5,geom="label", size=4, color='darkred') +
  labs(title="RCP 6.0 Cumulative Population per Daily Calories per Capita ", y="Cumulative Population", x="Daily Calories per Capita") +
  scale_x_continuous(limits = c(0, 3000)) +
  scale_y_continuous(limits=c(0, 1e+09)) +
  theme(
    legend.title = element_text(family = map_font_theme$family, size = 9, color = "#4e4d47"),
    legend.text = element_text(family = map_font_theme$family, size = 7, color = "#4e4d47"),
    text = map_font_theme,                 # Apply map font to all text
    axis.title = element_text(family = map_font_theme$family, hjust = 0.5, color = "#4e4d47"),   # Axis title font from theme_map
    plot.title = element_text(family = map_font_theme$family, hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5, color = "#4e4d47")# Plot title font from theme_map
  ) +  
  theme_hc()

save_plot("pop_cal_capita_50_2.pdf", plot, base_height=5, dpi="print")
#ggsave("pop_cal_capita_50.pdf", width = 6, height = 4)


#### MAPS -----
plot <- ggplot(df_dummy, aes(x, y)) +
  borders("world", colour = "grey45", size = 0.01) + 
  geom_point(aes(size = pop_2020, color=pop_2020), shape=16,
             alpha=0.05) +
  scale_size_continuous(range = c(0.3, 3), name="Population") +  # Adjust point size
  xlim(-180, 180) +
  ylim(-60, 90) +
  #coord_cartesian(xlim = c(-150, 170), ylim = c(-60, 80)) +
  coord_equal() +
  scale_color_gradientn(colours = half_curl_colors, trans = "log10",
                        name="Population") +
  theme_map() +
  
  theme(legend.direction = 'vertical',
        #legend.position = c(-0.25, .25),
        legend.spacing.y = unit(0, "pt"),
        legend.title = element_text(size = 9, color = "#4e4d47"),
        legend.text = element_text(size = 7, color = "#4e4d47"),
        plot.caption = element_text(size = 6, 
                                    hjust = 0.92, 
                                    margin = margin(t = 0.2, 
                                                    b = 0, 
                                                    unit = "cm"), 
                                    color = "#939184"),
        plot.title = element_text(hjust = 0.5, color = "#4e4d47"),
        plot.subtitle = element_text(hjust = 0.5, color = "#4e4d47")) +
  labs(x = NULL, 
       y = NULL, 
       title = "Population with Absolute Change in Produced Calories below 5000 People Fed Yearly", 
       subtitle =SUBTITLE)

save_plot("pop_abs_dif_location_50.png", plot, base_height = 7,dpi =300)

# NEXT MAP -----
plot <- ggplot(df_growth_dummy, aes(x, y)) +
  borders("world", colour = "grey45", size = 0.01) + 
  geom_point(aes(size = pop_2020, color=pop_2020), shape=16,
             alpha=0.05) +
  scale_size_continuous(range = c(0.3, 3), name="Population") +  # Adjust point size
  xlim(-180, 180) +
  ylim(-60, 90) +
  #coord_cartesian(xlim = c(-150, 170), ylim = c(-60, 80)) +
  coord_equal() +
  scale_color_gradientn(colours = half_curl_colors, trans = "log10",
                        name="Population") +
  theme_map() +
  
  theme(legend.direction = 'vertical',
        #legend.position = c(-0.25, .25),
        legend.spacing.y = unit(0, "pt"),
        legend.title = element_text(size = 9, color = "#4e4d47"),
        legend.text = element_text(size = 7, color = "#4e4d47"),
        plot.caption = element_text(size = 6, 
                                    hjust = 0.92, 
                                    margin = margin(t = 0.2, 
                                                    b = 0, 
                                                    unit = "cm"), 
                                    color = "#939184"),
        plot.title = element_text(hjust = 0.5, color = "#4e4d47"),
        plot.subtitle = element_text(hjust = 0.5, color = "#4e4d47")) +
  labs(x = NULL, 
       y = NULL, 
       title = "Population with Produced Calories Growth Rate below -0.15", 
       subtitle = SUBTITLE)

save_plot("pop_growth_location_50.png", plot, base_height = 7,dpi =300)




# NEXT MAP ------ 

plot <- ggplot(df_subsistence, aes(x, y)) +
  borders("world", colour = "grey45", size = 0.01) + 
  geom_point(aes(size = pop_2020, color=pop_2020), shape=16,
             alpha=0.1) +
  scale_size_continuous(range = c(0.3, 3), name="Population") +  # Adjust point size
  xlim(-180, 180) +
  ylim(-60, 90) +
  #coord_cartesian(xlim = c(-150, 170), ylim = c(-60, 80)) +
  coord_equal() +
  scale_color_gradientn(colours = half_curl_colors, trans = "log10",
                        name="Population") +
  theme_map() +
  
  theme(legend.direction = 'vertical',
        #legend.position = c(-0.25, .25),
        legend.spacing.y = unit(0, "pt"),
        legend.title = element_text(size = 9, color = "#4e4d47"),
        legend.text = element_text(size = 7, color = "#4e4d47"),
        plot.caption = element_text(size = 6, 
                                    hjust = 0.92, 
                                    margin = margin(t = 0.2, 
                                                    b = 0, 
                                                    unit = "cm"), 
                                    color = "#939184"),
        plot.title = element_text(hjust = 0.5, color = "#4e4d47"),
        plot.subtitle = element_text(hjust = 0.5, color = "#4e4d47")) +
  labs(x = NULL, 
       y = NULL, 
       title = "Population moving below Subsistence Level", 
       subtitle = SUBTITLE)
save_plot("subsistence_location_50.png", plot, base_height = 7,dpi = 300)



# Population in Danger ----- 
plot <- ggplot(pid_df, aes(x, y)) +
  #borders("world", colour = "grey45", size = 0.01) + 
  geom_point(aes(size = population_in_danger, color=population_in_danger), shape=16,
             alpha=0.05) +
  scale_size_continuous(range = c(0.3, 3), name="Population") +  # Adjust point size
  xlim(-180, 180) +
  ylim(-60, 90) +
  #coord_cartesian(xlim = c(-150, 170), ylim = c(-60, 80)) +
  coord_equal() +
  scale_color_gradientn(colours = half_curl_colors,trans = "log10",
                        name="Population") +
  theme_map() +
  
  theme(legend.direction = 'vertical',
        #legend.position = c(-0.25, .25),
        legend.spacing.y = unit(0, "pt"),
        legend.title = element_text(size = 9, color = "#4e4d47"),
        legend.text = element_text(size = 7, color = "#4e4d47"),
        plot.caption = element_text(size = 6, 
                                    hjust = 0.92, 
                                    margin = margin(t = 0.2, 
                                                    b = 0, 
                                                    unit = "cm"), 
                                    color = "#939184"),
        plot.title = element_text(hjust = 0.5, color = "#4e4d47"),
        plot.subtitle = element_text(hjust = 0.5, color = "#4e4d47")) +
  labs(x = NULL, 
       y = NULL, 
       title = "Population in Danger", 
       subtitle = SUBTITLE)

save_plot("population_in_test.pdf", plot, base_height = 7,dpi = "print")


#USE cases? North Africa and INDIA -------------------------------

bin_data <- readRDS("C:/Users/lenovo/Documents/GitHub/sustainability_webpage/bin_data.rds")
bin_data[, c("x", "y")] <- round(bin_data[, c("x", "y")], 3)
bin_data <- bin_data[, c("x", "y", names(bin_data)[13:18])]
df <- left_join(df, bin_data)

india_df <- df%>% filter(ISO_A3 == "IND")
#changes in distribution 
cmocean(name = 'curl', direction = -1, clip = 0.2)(2)

ggplot(india_df) +
  geom_density(aes(x = cal_cap, color="Distribution Today"), fill = "#1A3651FF", alpha = 0.25) +
  geom_density(aes(x = cal_cap_future, color=" Distribution RCP6.0"), fill = "#571350FF", alpha = 0.5) +
  labs(title = "Changes in Daily Calories per Capita Distribution",
       x = "Daily Calories per Person", y = "Density") +
  xlim(0, 15000) +
  scale_color_manual(values = c("#1A3651FF", "#571350FF")) +
  theme_hc()


bin_cols <- names(india_df)[32:34]
rcp_list = c("RCP 2.6", "RCP 6.0", "RCP 8.5")
india_title <- "Absolute Changes in Calories in India"

india_1 <- bin_plot(india_df, bin_cols[1], xlim_low = 68, xlim_high = 98, 
                    ylim_low = 6, ylim_high = 34, in_title = india_title, 
                    rcp=rcp_list[1], legend=-0.25, region="india")
india_2 <- bin_plot(india_df, bin_cols[2], xlim_low = 68, xlim_high = 98, 
                    ylim_low = 6, ylim_high = 34, in_title = india_title, 
                    rcp=rcp_list[2], legend=-0.25, region="india")

india_3 <- bin_plot(india_df, bin_cols[3], xlim_low = 68, xlim_high = 98, 
                    ylim_low = 6, ylim_high = 34, in_title = india_title, 
                    rcp=rcp_list[3], legend=-0.25, region="india")

india_df$cal_cap_growth_rate <- (india_df$cal_cap_future - india_df$cal_cap)/ india_df$cal_cap
india_df %>% filter(cal_cap_growth_rate < 3) %>% 
  ggplot(aes(x=cal_cap_growth_rate)) +
    geom_histogram(bins=200)

# could do something with this?
sum(india_df$pop_2020[(india_df$cal_cap > 2000 & india_df$cal_cap_future < 2000)])
india_df %>% filter(cal_cap > 2000) %>%
  arrange(cal_cap_future) %>%
  mutate(cumulative_population = cumsum(pop_2020)) %>%
  ggplot(aes(x=cal_cap_future, y=cumulative_population)) +
  geom_line(size=0.75) +
  geom_vline(aes(xintercept=2000), linetype='dashed')+
  annotate(x=2000,y=3e+08,label="53 Million People move below subsistence level",vjust=0.5,geom="label", size=3) +
  labs(title="RCP 6.0 Cumulative Population per Daily Calories per Capita ", y="Cumulative Population", x="Daily Calories per Capita") +
  scale_x_continuous(limits = c(0, 3000)) +
  scale_y_continuous(limits=c(0, 5e+08)) +
  theme_hc()


# North Africa -------------------------------------------------
na_title <- "Absolute Changes in Calories in North Africa"

na_1 <- bin_plot(df, bin_cols[1], xlim_low=-12, xlim_high=13, 
                    ylim_low=25, ylim_high=40, in_title=na_title, 
                    rcp=rcp_list[1],legend=-0.05, region = "north_africa")

na_2 <- bin_plot(df, bin_cols[2], xlim_low=-12, xlim_high=13, 
                 ylim_low=25, ylim_high=40, in_title=na_title, 
                 rcp=rcp_list[2],legend=-0.05, region = "north_africa")
na_3 <- bin_plot(df, bin_cols[3], xlim_low=-12, xlim_high=13, 
                 ylim_low=25, ylim_high=40, in_title=na_title, 
                 rcp=rcp_list[3], legend=-0.05, region = "north_africa")


### more

india_dummy <- df_dummy%>% filter(ISO_A3 == "IND")

plot <- ggplot(india_dummy, aes(x, y)) +
  #borders("world",  xlim= c(68, 98),ylim=c(6, 34),
  #        colour = "grey45", size = 0.01) + 
  geom_point(aes(size = pop_2020, color=pop_2020), shape=16,
             alpha=0.05) +
  scale_size_continuous(range = c(0.1, 10), name="Population") +  # Adjust point size
  xlim(68, 98) +
  ylim(6, 34) +
  coord_equal() +
  #coord_map(xlim=c(68, 98), ylim=c(6, 34)) +
  scale_color_gradientn(colours = half_curl_colors, trans = "log10",
                        name="Population") +
  theme_map() +
  
  theme(legend.direction = 'vertical',
        #legend.position = c(-0.25, .25),
        legend.spacing.y = unit(0, "pt"),
        legend.title = element_text(size = 9, color = "#4e4d47"),
        legend.text = element_text(size = 7, color = "#4e4d47"),
        plot.caption = element_text(size = 6, 
                                    hjust = 0.92, 
                                    margin = margin(t = 0.2, 
                                                    b = 0, 
                                                    unit = "cm"), 
                                    color = "#939184"),
        plot.title = element_text(hjust = 0.5, color = "#4e4d47"),
        plot.subtitle = element_text(hjust = 0.5, color = "#4e4d47")) +
  labs(x = NULL, 
       y = NULL, 
       title = "Population with Absolute Change in Produced Calories below 5000 People Fed Yearly", 
       subtitle ="India under RCP 6.0 for the period 2040-2070 projected to today")
plot
save_plot("india_abs_location.pdf", plot, base_height = 7,dpi ="print")

### Relative
india_growth <- df_growth_dummy %>% filter(ISO_A3 == "IND")
plot <- ggplot(india_growth, aes(x, y)) +
  #borders("world",  xlim= c(68, 98),ylim=c(6, 34),
  #        colour = "grey45", size = 0.01) + 
  geom_point(aes(size = pop_2020, color=pop_2020), shape=16,
             alpha=0.05) +
  scale_size_continuous(range = c(0.1, 10), name="Population") +  # Adjust point size
  xlim(68, 98) +
  ylim(6, 34) +
  #coord_cartesian(xlim = c(68, 98), ylim = c(6, 34)) +
  coord_equal() +
  #coord_map(xlim=c(68, 98), ylim=c(6, 34)) +
  scale_color_gradientn(colours = half_curl_colors, trans = "log10",
                        name="Population") +
  theme_map() +
  
  theme(legend.direction = 'vertical',
        #legend.position = c(-0.25, .25),
        legend.spacing.y = unit(0, "pt"),
        legend.title = element_text(size = 9, color = "#4e4d47"),
        legend.text = element_text(size = 7, color = "#4e4d47"),
        plot.caption = element_text(size = 6, 
                                    hjust = 0.92, 
                                    margin = margin(t = 0.2, 
                                                    b = 0, 
                                                    unit = "cm"), 
                                    color = "#939184"),
        plot.title = element_text(hjust = 0.5, color = "#4e4d47"),
        plot.subtitle = element_text(hjust = 0.5, color = "#4e4d47")) +
  labs(x = NULL, 
       y = NULL, 
       title = "Population with Produced Calories Growth Rate below -0.15", 
       subtitle ="India under RCP 6.0 for the period 2040-2070 projected to today")
plot
save_plot("india_rel_location.pdf", plot, base_height = 7,dpi ="print")


### Population in Danger

india_pid <- pid_df %>% filter(ISO_A3 == "IND")

plot <- ggplot(india_pid, aes(x, y)) +
  #borders("world",  xlim= c(68, 98),ylim=c(6, 34),
  #        colour = "grey45", size = 0.01) + 
  geom_point(aes(size = population_in_danger, 
                 color=population_in_danger), shape=16,
             alpha=0.05) +
  scale_size_continuous(range = c(0.1, 10), name="Population") +  # Adjust point size
  xlim(68, 98) +
  ylim(6, 34) +
  coord_equal() +
  #coord_map(xlim=c(68, 98), ylim=c(6, 34)) +
  scale_color_gradientn(colours = half_curl_colors, trans = "log10",
                        name="Population") +
  theme_map() +
  
  theme(legend.direction = 'vertical',
        #legend.position = c(-0.25, .25),
        legend.spacing.y = unit(0, "pt"),
        legend.title = element_text(size = 9, color = "#4e4d47"),
        legend.text = element_text(size = 7, color = "#4e4d47"),
        plot.caption = element_text(size = 6, 
                                    hjust = 0.92, 
                                    margin = margin(t = 0.2, 
                                                    b = 0, 
                                                    unit = "cm"), 
                                    color = "#939184"),
        plot.title = element_text(hjust = 0.5, color = "#4e4d47"),
        plot.subtitle = element_text(hjust = 0.5, color = "#4e4d47")) +
  labs(x = NULL, 
       y = NULL, 
       title = "Population in Danger", 
       subtitle ="India under RCP 6.0 for the period 2040-2070 projected to today")
plot
save_plot("india_pop_in_danger.pdf", plot, base_height = 7,dpi ="print")


### North Africa

na_df <- df_dummy %>%
  filter(x >= -12 & x <= 13, y >= 25 & y <= 40)
na_growth <- df_growth_dummy %>%
  filter(x >= -12 & x <= 13, y >= 25 & y <= 40)
na_pid <- pid_df %>%
  filter(x >= -12 & x <= 13, y >= 25 & y <= 40)

### Plot 1 (really this should be a function)

plot <- ggplot(na_df, aes(x, y)) +
  #borders("world",  xlim= c(-12, 13),ylim=c(25, 40),
  #        colour = "grey45", size = 0.01) + 
  geom_point(aes(size = pop_2020, color=pop_2020), shape=16,
             alpha=0.25) +
  scale_size_continuous(range = c(0.1, 10), name="Population") +  # Adjust point size
  xlim(-12, 13) +
  ylim(25, 40) +
  #coord_cartesian(xlim = c(68, 98), ylim = c(6, 34)) +
  coord_equal() +
  #coord_map(xlim=c(-12, 13), ylim=c(25, 40)) +
  scale_color_gradientn(colours = half_curl_colors, trans = "log10",
                        name="Population") +
  theme_map() +
  
  theme(legend.direction = 'vertical',
        #legend.position = c(-0.25, .25),
        legend.spacing.y = unit(0, "pt"),
        legend.title = element_text(size = 9, color = "#4e4d47"),
        legend.text = element_text(size = 7, color = "#4e4d47"),
        plot.caption = element_text(size = 6, 
                                    hjust = 0.92, 
                                    margin = margin(t = 0.2, 
                                                    b = 0, 
                                                    unit = "cm"), 
                                    color = "#939184"),
        plot.title = element_text(hjust = 0.5, color = "#4e4d47"),
        plot.subtitle = element_text(hjust = 0.5, color = "#4e4d47")) +
  labs(x = NULL, 
       y = NULL, 
       title = "Population with Absolute Change in Produced Calories below 5000 People Fed Yearly", 
       subtitle ="North Africa under RCP 6.0 for the period 2040-2070 projected to today")
plot
save_plot("na_abs_location.pdf", plot, base_height = 7,dpi ="print")

### Plot 2:


plot <- ggplot(na_growth, aes(x, y)) +
  #borders("world",  xlim= c(-12, 13),ylim=c(25, 40),
  #        colour = "grey45", size = 0.01) + 
  geom_point(aes(size = pop_2020, color=pop_2020), shape=16,
             alpha=0.05) +
  scale_size_continuous(range = c(0.1, 10), name="Population") +  # Adjust point size
  xlim(-12, 13) +
  ylim(25, 40) +
  #coord_cartesian(xlim = c(68, 98), ylim = c(6, 34)) +
  coord_equal() +
  #coord_map(xlim=c(-12, 13), ylim=c(25, 40)) +
  scale_color_gradientn(colours = half_curl_colors, trans = "log10",
                        name="Population") +
  theme_map() +
  
  theme(legend.direction = 'vertical',
        #legend.position = c(-0.25, .25),
        legend.spacing.y = unit(0, "pt"),
        legend.title = element_text(size = 9, color = "#4e4d47"),
        legend.text = element_text(size = 7, color = "#4e4d47"),
        plot.caption = element_text(size = 6, 
                                    hjust = 0.92, 
                                    margin = margin(t = 0.2, 
                                                    b = 0, 
                                                    unit = "cm"), 
                                    color = "#939184"),
        plot.title = element_text(hjust = 0.5, color = "#4e4d47"),
        plot.subtitle = element_text(hjust = 0.5, color = "#4e4d47")) +
  labs(x = NULL, 
       y = NULL, 
       title = "Population with Produced Calories Growth Rate below -0.15", 
       subtitle ="North Africa under RCP 6.0 for the period 2040-2070 projected to today")
plot
save_plot("na_rel_location.pdf", plot, base_height = 7,dpi ="print")

### PLOT 3
plot <- ggplot(na_pid, aes(x, y)) +
  #borders("world",  xlim= c(-12, 13),ylim=c(25, 40),
  #        colour = "grey45", size = 0.01) + 
  geom_point(aes(size = population_in_danger, color=population_in_danger), shape=16,
             alpha=0.25) +
  scale_size_continuous(range = c(0.1, 10), name="Population") +  # Adjust point size
  xlim(-12, 13) +
  ylim(25, 40) +
  #coord_cartesian(xlim = c(68, 98), ylim = c(6, 34)) +
  coord_equal() +
  #coord_map(xlim=c(-12, 13), ylim=c(25, 40)) +
  scale_color_gradientn(colours = half_curl_colors, trans = "log10",
                        name="Population") +
  theme_map() +
  
  theme(legend.direction = 'vertical',
        #legend.position = c(-0.25, .25),
        legend.spacing.y = unit(0, "pt"),
        legend.title = element_text(size = 9, color = "#4e4d47"),
        legend.text = element_text(size = 7, color = "#4e4d47"),
        plot.caption = element_text(size = 6, 
                                    hjust = 0.92, 
                                    margin = margin(t = 0.2, 
                                                    b = 0, 
                                                    unit = "cm"), 
                                    color = "#939184"),
        plot.title = element_text(hjust = 0.5, color = "#4e4d47"),
        plot.subtitle = element_text(hjust = 0.5, color = "#4e4d47")) +
  labs(x = NULL, 
       y = NULL, 
       title = "Population in Danger", 
       subtitle ="North Africa under RCP 6.0 for the period 2040-2070 projected to today")
plot
save_plot("na_pop_danger.pdf", plot, base_height = 7,dpi ="print")
