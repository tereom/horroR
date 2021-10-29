library(tidyverse)
library(ggplot2)
library(png)
library(jpeg)
library(ggiraph)

img <- readJPEG('ej-imagen-fondo/imagen1')
writePNG(img)

options(scipen = 999)

movies <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018/2018-10-23/movie_profit.csv')

p <- movies %>% 
    filter(genre == "Horror") %>% 
    mutate(
        release_date = as.Date(release_date, '%m/%d/%Y'),
        release_year = as.numeric(format(release_date, "%Y"))
        ) %>% 
    group_by(release_year) %>% 
    filter(worldwide_gross == max(worldwide_gross)) %>% 
    mutate(pelicula = paste0("\n", movie, "\n", release_year)) %>% 
    ggplot(aes(label = pelicula)) +
    annotation_custom(grid::rasterGrob(img,
                                       width = unit(1,"npc"),
                                       height = unit(1,"npc"))) +
    geom_point_interactive(aes(x = release_year, y = worldwide_gross), size = 3) +
    geom_point_interactive(aes(x = release_year, y = worldwide_gross, color = movie, tooltip = pelicula)) +
    scale_y_continuous(breaks = 1:8 * 100000000, labels = 1:8 ) +
    labs(
        title = "Pelicula con mas ganancias por año",
        subtitle = "Genero horror", 
        y = "Ganancias Mundiales (en cientos de millones)",
        x = "Año de Estreno"
        ) +
    theme_minimal() +
    theme(legend.position = "none")

ggiraph(ggobj = p)

#ggplotly(p, tooltip = "pelicula")
#summarise(promedio_worldide = mean(worldwide_gross))
#mutate(maxima_del_anio = ifelse(worldwide_gross == max(worldwide_gross), "Si", "No")) %>% 
#View()
