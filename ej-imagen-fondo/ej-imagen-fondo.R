# paquetes
library(tidyverse)
library(png)
library(grid)
library(jpeg)

# leemos los datos
disfraces <- read.csv('https://www.engineeringbigdata.com/wp-content/uploads/halloween_costumes_state_google_frightgeist_2017.csv',
                      stringsAsFactors = F)
head(disfraces)

# creamos una tabla que contenga únicamente los disfraces
# y contamos las frecuencias de cada uno
# filtramos solamente los que tienen más de 10 menciones
ranking <- disfraces %>%
    dplyr::select(-State) %>%
    gather(rank, disfraz) %>%
    count(disfraz) %>%
    arrange(desc(n)) %>%
    mutate(disfraz = factor(disfraz, levels = disfraz)) %>%
    filter(n > 10)

# leemos la imagen que queremos usar de fondo, si no está en
# formato png la convertimos
img <- readJPEG('ej-imagen-fondo/imagen1')
writePNG(img)

# creamos el gráfico
ggplot(ranking, aes(x = disfraz, y = n)) +
    labs(title = "Top 5 de disfraces por estados en USA",
         subtitle = 'Año: 2017',
         y = 'número de estados') +
    scale_fill_continuous(guide = FALSE) +
    # aquí ponemos la imagen de fondo
    annotation_custom(grid::rasterGrob(img,
                                       width = unit(1,"npc"),
                                       height = unit(1,"npc"))) +
    geom_bar(stat="identity", fill = "black",
             position = "dodge", width = .7, alpha = 0.8) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))


