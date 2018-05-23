library(readxl)
library(rusmaps)
library(tidyverse)
library(ggmap)
library(magick)

#read table. I've no way of disclosing this information.
df <- read_xlsx(paste0(getwd(), '/Day_4/long.xlsx')) #These small 3 lines of code are a lot of work to prepare the data
df_plot <- fortify(rus_sub) %>% 
  left_join(df)

titles <- str_extract_all(names(df_plot)[9:length(df_plot)], pattern = '\\d+') %>% 
  map_chr(function(x) paste(x[1], 'quarter', x[2]))

dir.create(paste0(getwd(), '/Day_4/Map'))

for(i in 9:length(df_plot)){

  ggplot(data = df_plot) + 
    geom_polygon(aes(x = long, y = lat, fill = df_plot[,i], group = group), color = "white") + 
    coord_quickmap() +
    theme_void() + 
    scale_fill_distiller(name = "The average duration of the disease, in days", palette = "RdYlGn") +
    theme(legend.position = "bottom") +
    labs(subtitle = titles[i - 8])
  
  ggsave(paste0("disease_duration", i, '.png'), path = 'Day_4/Map', height = 6, width = 7)
}

#animated
x <- list.files(path = "Day_4/Map", pattern = "*.png", full.names = T) %>% 
  map(image_read) %>% 
  map(image_resize, '700x700') %>% 
  image_join()

image_write(image_animate(x, fps=1), "Day_4/Map/map_disease_duration.gif") 
