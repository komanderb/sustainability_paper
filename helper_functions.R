##################################################
## Project: Sustainability Paper
## Script purpose: Helper function to make main script cleaner
## Date:
## Author:
##################################################

library(tidyverse)
library(ggthemes)
library(cmocean)
library(countrycode)
library(raster)


bin_plot <- function(data, column, xlim_low, xlim_high, 
                     ylim_low, ylim_high, in_title, rcp, legend, region){
  sort_values <- sort(unique(round(unique(data[[column]]))))
  my_color <- cmocean(name = 'curl', direction = -1, clip = 0.1)(11)
  names(my_color) <- as.character(sort_values)
  
  out_plot <- ggplot(data, aes(x, y)) +
    geom_tile(aes(fill =as.character(round(get(column))))) +
    xlim(xlim_low, xlim_high) +
    ylim(ylim_low, ylim_high) +
    coord_equal() +
    scale_fill_manual(values = my_color, name = "Bin Mean",
                      labels = sort_values)  +
    theme_map() +
    theme(legend.direction = 'vertical',
          legend.position = c(legend, .25),
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
         title = paste(rcp, in_title), 
         subtitle = "For the period 2070−2100 compared to today. Calories expressed
         in People fed yearly") 
  pdf_name <- paste0(tolower(gsub("\\.", "p", rcp)), region,  
                     "_bin_cal_dif.pdf")
  save_plot(pdf_name, out_plot, base_height = 7,dpi = "print")
  return(out_plot)
}


new_bins_positive <- function(x, breaks){
  # intiate return vector
  return_vector <- rep(0, length(x))
  
  qs <- quantile(x, breaks, na.rm = TRUE)
  labels <- tapply(x, findInterval(x, qs), mean)
  return_x <- cut(ntile(x, 100), breaks = breaks*100, labels = labels[-length(labels)])
  return_x <- as.numeric(levels(return_x))[x]
  return(return_vector)
}

