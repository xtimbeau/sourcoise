# do something wrong :
library(tidyverse)
data <- sourcoise("work/noerror3.r")
ggplot(data) + geom_point(aes(x=hp, y=y))
