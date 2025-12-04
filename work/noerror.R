# do something wrong :
library(tidyverse)
data <- mtcars |>
  group_by(hp) |>
  summarize(y = sum(disp))
ggplot(data) + geom_point(aes(x=hp, y=y))
