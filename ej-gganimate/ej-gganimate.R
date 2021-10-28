# Paquetes
library(tidyverse)
library(ggplot2)
library(gganimate)
library(ggimage)

# Leemos los datos
movies <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018/2018-10-23/movie_profit.csv')

# Creamos una tabla que contenga el presupuesto promedio de producción
# por año y género
movies_budget <- movies %>%
    mutate(year = as.integer(str_sub(release_date, start = -1L - 3, end = -1L))) %>% 
    select(c("production_budget","genre", "year")) %>% 
    group_by(year,genre) %>% 
    summarise(budget = mean(production_budget)) %>% 
    mutate(img = paste("ej-gganimate/img-movie/", paste(genre, "png",sep = "."), sep = "")) 

# Gráfica interactiva de péliculas por año
movies_budget %>%
    ggplot(aes(genre,budget)) +
    theme_minimal() +
    scale_y_continuous(labels = scales::comma)+
    geom_image(aes(image = img), size = 0.15) +
    theme(plot.background = element_rect(fill = "#9a77cf"))+
    labs(x = "Género", y = "Presupuesto promedio", 
         title = "Presupuesto promedio de producción por género",
         subtitle = "Año: {frame_time}") +
    transition_time(year) 

# Guardando gif
anim_save("production-budget-gganimate.gif")
