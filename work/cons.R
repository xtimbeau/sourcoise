library(tidyverse)
library(ofce)
library(ggiraph)

data <- sourcoise("data_construction.R")

gc <- ggplot() +
  geom_line(data = data$ENQ_DEM,
            aes(x = as.Date(DATE),
                y = ENQ_DEM,
                color = "Logements neufs")) +
  geom_line(data = data$ENQ_PERSP,
            aes(x = as.Date(DATE),
                y = ENQ_PERSP,
                color = "Mises en chantier")) +
  geom_point_interactive(data = data$ENQ_DEM,
                         aes(x = as.Date(DATE),
                             y = ENQ_DEM,
                             color = "Logements neufs",
                             tooltip = paste("Date:", as.Date(DATE), "<br>Demande de logements neufs:", ENQ_DEM))) +
  geom_point_interactive(data = data$ENQ_PERSP,
                         aes(x = as.Date(DATE),
                             y = ENQ_PERSP,
                             color = "Mises en chantier",
                             tooltip = paste("Date:", as.Date(DATE), "<br>Perspectives de mises en chantier:", ENQ_PERSP))) +
  PrettyCols::scale_color_pretty_d(palette = "Joyful", name = NULL) +
  labs(x = NULL, y = "Solde d'opinion") +
  scale_ofce_date(date_breaks = "2 year") +
  scale_y_continuous(labels = scales::comma_format()) +
  theme_ofce() +
  geom_hline(yintercept = 0, linetype = "solid") +
  ofce_caption(source="Insee", sub="Conjoncture du bâtiment")

graphs <- list( "c." = gc )

return(graphs)
