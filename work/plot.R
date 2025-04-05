library(tidyverse)

ggplot(mtcars)+geom_line(aes(x=mpg, y = disp))+ofce::ofce_caption(sub="é")+ofce::theme_ofce(marquee=TRUE)
