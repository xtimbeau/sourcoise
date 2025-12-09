library(eurostat)
library(tidyverse)

pays <- source_data("nace.r")$pays3
nace <- source_data("nace.r")$nace

households.raw <- "nama_10_cp18" |>
  get_eurostat(filters = list(unit="CP_MEUR", geo= pays, coicop18 = c("CP041", "CP042"))) |>
  select(time, geo, values, coicop18) |>
  drop_na(values) |>
  pivot_wider(names_from = coicop18, values_from = values) |>
  mutate(lymen = CP041+CP042) |>
  arrange(geo, desc(time))

vaL <- source_data("vaq.r")$naq_a10 |>
  filter(nace_r2=="L") |>
  mutate(time=floor_date(time, "year")) |>
  group_by(geo, time) |>
  summarize(B1G = sum(B1G), p51c=sum(p51c), .groups="drop") |>
  arrange(geo, desc(time))

vamd <- source_data("vaq.r")$naaa |>
  filter(champ=="md") |>
  select(geo, time, van, vab)

households <- households.raw |>
  left_join(vaL, by = c("time", "geo")) |>
  left_join(vamd, by = c("time", "geo")) |>
  mutate( L = (B1G)/vab,
          Ll = (CP041+CP042)/vab,
          Lli = (CP042)/vab)

return(households)
