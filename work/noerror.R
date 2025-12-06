# do something wrong :
library(tidyverse)
sourcoise("work/commun.r")
data <- sourcoise("work/noerror3.r")
ggplot(data) + geom_point(aes(x=hp, y=y))
