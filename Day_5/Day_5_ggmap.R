library(ggmap)
library(viridis)
data(Sacramento, package = 'caret')
#price one sqft
Sacramento$cost_one_sqft <- with(Sacramento, price/sqft)

min_lon <- min(Sacramento$longitude)
max_lon <- max(Sacramento$longitude)
min_lat <- min(Sacramento$latitude)
max_lat <- max(Sacramento$latitude)

pos_sacramento <- c(left = min_lon - 0.5, bottom = min_lat - 0.5,
                    right = max_lon + 0.5, top = max_lat + 0.5)
geo_sacramento <- get_map(pos_sacramento)

ggmap(geo_sacramento, extent = "device") +
  geom_density2d(data = Sacramento, 
                 aes(x = longitude, y = latitude), size = 0.3) +
  stat_density2d(data = Sacramento, aes(x = longitude, y = latitude, fill = ..level.., alpha = ..level..),
                 bins = 10, geom = "polygon") + 
  scale_fill_gradient(low = "white", high = "red") + 
  scale_alpha(range = c(0, 0.3), guide = FALSE) +
  ggtitle('Density of the number of real estate transactions in Sacramento') + 
  theme(plot.title = element_text(size = 12, face = "bold"))
ggsave('Sacramento_transactions.jpg', path = 'Day_5/Map')

ggmap(geo_sacramento, extent = "device") +
  geom_point(aes(x = longitude, y = latitude, color = cost_one_sqft, size = log(cost_one_sqft), alpha = 0.05), data = Sacramento) + 
  scale_color_viridis() + 
  guides(size = FALSE, alpha = FALSE) +
  ggtitle('Cost per square foot of real estate in Sacramento') + 
  theme(plot.title = element_text(size = 12, face = "bold"))
ggsave('Sacramento_cost.jpg', path = 'Day_5/Map')
