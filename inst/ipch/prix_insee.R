library(insee)
library(dplyr)
library(lubridate)

ipchm <- get_idbank_list("IPCH-2015") |>
  filter(COICOP2016=="00", FREQ=="M", NATURE=="INDICE") |>
  pull(idbank) |>
  get_insee_idbank() |>
  select(DATE, ipch = OBS_VALUE, IDBANK)

ipch <- ipchm |>
  mutate(DATE = floor_date(DATE, unit="quarter")) |>
  group_by(DATE) |>
  summarise(ipch = mean(ipch))

ipcha <- ipch |>
  mutate(y = year(DATE)) |>
  group_by(y) |>
  summarize(ipch = mean(ipch)) |>
  mutate(ipch = ipch / ipch[y == 2023])

return(list(ipcha = ipcha, ipchm = ipchm, ipch = ipch))
