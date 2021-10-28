# paquetes
library(tidyverse)
library(plotly)
library(emojifont)

# leemos los datos
movies <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018/2018-10-23/movie_profit.csv')

# creamos una tabla que contenga únicamente las películas de terror con
# ganancia y presupuesto positivos
# creamos además una variable de año en que se estrenó
movies_horror <- movies %>%
  filter(genre == "Horror", worldwide_gross > 0, production_budget > 0) %>%
  mutate(year = as.numeric(str_sub(release_date, start = -1L - 3, end = -1L)))

gg_horror <- ggplot(movies_horror, aes(x = production_budget,
                          y = worldwide_gross, color = year)) +
  geom_point() +
  theme_minimal() +
  scale_x_log10() +
  scale_y_log10() +
  facet_wrap(~ mpaa_rating)
gg_horror

# Y creamos un gráfico interactivo con plotly
ggplotly(gg_horror)

# Halloween? Creamos una columna de emojis con el paquete emojifont!
movies_horror <- movies_horror %>% 
  mutate(emoji = case_when(
    mpaa_rating == "PG" ~ emoji("jack_o_lantern"),
    mpaa_rating == "PG-13" ~ emoji("ghost"),
    mpaa_rating == "R" ~ emoji("skull"),
    TRUE ~ emoji("imp")
  ))
glimpse(movies_horror)

# usamos ggplotly eligiendo que información queremos desplegar
gg_horror_emo <- ggplot(movies_horror, aes(text = emoji, x = production_budget, 
                                           y = worldwide_gross, color = year, 
                                           label = movie)) +
  geom_point() +
  theme_minimal() +
  scale_x_log10() +
  scale_y_log10() +
  facet_wrap(~ mpaa_rating)

ggplotly(gg_horror_emo, tooltip = c("emoji", "movie", "year"))
