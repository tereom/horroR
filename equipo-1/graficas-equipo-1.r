# paquetes
library(tidyverse)
library(plotly)
library(emojifont)

# leemos los datos
movies <- read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2018/2018-10-23/movie_profit.csv')

library(ggplot2)

movies_horror <- movies %>%
  mutate(year = as.numeric(str_sub(release_date, start = -1L - 3, end = -1L)),
         q_rentabilidad= worldwide_gross/production_budget) %>%
  filter(year %in% 2000:2014, worldwide_gross > 0, production_budget > 0)

movies_horror
glimpse(movies_horror)

gg_horror <- ggplot(movies_horror, aes(x = worldwide_gross,
                                       y = q_rentabilidad, color = genre, text=movie)) +
  geom_point(alpha=0.5) +
  theme_minimal() +
  scale_x_log10() +
  scale_y_log10() +
  facet_wrap(~ mpaa_rating)
gg_horror

ggplotly(gg_horror)

#Script Leonel
fig <- movies_horror %>% ggplot(aes(x=distributor, y=year, size = domestic_gross, color=genre)) +
  geom_point(alpha=0.5) + scale_size(range = c(.1, 20)) + theme(text = element_text(size=10),plot.background = element_rect(fill = "black"),
                                                                axis.text.x = element_text(angle=35, hjust=1), legend.position = 'none') + scale_x_discrete("Eje X") + scale_y_discrete("Eje Y") + scale_color_discrete(NULL) +
  labs(title = 'Título') + theme_dark()
fig <- ggplotly(fig)
fig
