# paquetes
library(tidyverse)
library(magrittr)
library(ggimg)

# leemos los datos
movies <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018/2018-10-23/movie_profit.csv')
head(movies)

# filtramos las películas de terror y calculamos la ganancia
# nos quedamos únicamente con el top 7
movies_horror_top <- movies %>%
    filter(genre == "Horror", worldwide_gross > 0,
           production_budget > 0) %>% 
    mutate(ganancia = worldwide_gross - production_budget) %>% 
    arrange(desc(ganancia)) %>% 
    head(7)

# leemos las rutas a las imágenes
movies_horror_top %<>% 
    mutate(path = list.files('ej-ggimg', pattern = '*.png', full.names = T))

# creamos la gráfica
ggplot(movies_horror_top) +
    geom_point_img(aes(
        x = release_date,
        y = ganancia,
        img = path
    ), size = 1.5) +
    ylim(1e+08, 7e+08) +
    labs(title = 'Top 7 de películas con más ganancias',
         subtitle = 'Género: Terror',
         x = 'fecha de estreno') +
    theme(plot.background = element_rect(fill = "mediumorchid4")) +
    theme(panel.background = element_rect(fill = "mediumorchid4"),
          axis.title = element_text(colour = 'white'),
          axis.text = element_text(colour = 'white'),
          plot.title = element_text(colour = 'white'),
          plot.subtitle = element_text(colour = 'white')) 
    
    
