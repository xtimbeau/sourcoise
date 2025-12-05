library(tidyverse)
data <- mtcars |>
  group_by(hp) |>
  summarize(y = sum(disp))
data
