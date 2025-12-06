library(tidyverse)
sourcoise("work/commun.r")
data <- mtcars |>
  group_by(hp) |>
  summarize(y = sum(disp))
data
