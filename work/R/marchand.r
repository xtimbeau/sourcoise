library(tidyverse)

nace <- tribble( ~a20, ~a10, ~marchand,
                 "A", "A", TRUE,
                 "B", "B-E", TRUE,
                 "C", "B-E", TRUE,
                 "D", "B-E", TRUE,
                 "E", "B-E", TRUE,
                 "F", "F", TRUE,
                 "G", "G-I", TRUE,
                 "H", "G-I", TRUE,
                 "I", "G-I", TRUE,
                 "J", "J", TRUE,
                 "K", "K", TRUE,
                 "L", "L", TRUE,
                 "M", "M_N", TRUE,
                 "N", "M_N", TRUE,
                 "O", "O-Q", FALSE,
                 "P", "O-Q", FALSE,
                 "Q", "O-Q", FALSE,
                 "R", "R-U", TRUE,
                 "S", "R-U", TRUE,
                 "T", "R-U", TRUE,
                 "U", "R-U", TRUE ) |>
  mutate(l20 = label_eurostat(a20, dic="nace_r2"),
         l10 = label_eurostat(a10, dic="nace_r2"))

marchand <- nace |> filter(marchand) |> pull(a20)
marchand2 <- c(nace |> filter(marchand) |> pull(a10))

return(list(marchand = marchand, marchand2 = marchand2))
