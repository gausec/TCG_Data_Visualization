### Libraries 
```
library(tidyverse)
library(ggplot2)
library(usmap)
library(sf)
library(scales)

```
 &nbsp;
 
### Data 
```
Tribes<-as.data.frame(read.csv("Tribes.csv", header = TRUE, colClasses = c(fips = "character")))
```
 &nbsp;

### Creating data labels 
```
centroid_labels <- usmapdata::centroid_labels("states")
data_labels <- merge(centroid_labels, Tribes, by = "fips")
```
 &nbsp;
 
### Mapping
```
Tribes_Map<-
  
  plot_usmap(
  color = "white",
  linewidth = 0.1,
  regions = "states",
  data = Tribes,
  values = "Total"
) +
  
  scale_fill_gradient(
    trans = 'log',
    labels = scales::label_number(big.mark = ','),
    breaks = c(5,10,15,20,25,30,35),
    high = '#326249',
    low = 'white'
  ) +
  
  theme_void() +
  
  geom_sf_text(data = data_labels, ggplot2::aes(
    label = scales::number(Total)
  ), color = "black") +
  
  theme(text = element_text(family = 'Ariel', size = 11),
    legend.position = 'top') +
  
  labs(fill = 'Total Tribes') +
  
  guides(
    fill = guide_colorbar(
      barwidth = unit(10, 'cm')))
```


```{r}
ggsave("Tribes_Map.png", plot = Tribes_Map, width = 15, height = 8, dpi = 500)
```
