library(tidyverse)
library(ofce)

naq_a10 <- source_data("vaq.r")$naq_a10
nace <- source_data("nace.r")
pays <- nace$pays1

data <- naq_a10 |>
  filter( ! nace_r2 %in% c("L", "O-Q"), geo %in%  pays) |>
  select(geo, time, nace_r2, van, msa) |>
  drop_na(van, msa) |>
  arrange(geo, nace_r2, time)

mt <- data |>
  group_by(geo, nace_r2) |>
  summarise(mt = min(time), .groups = "drop") |>
  summarise(mt = max(mt)) |>
  pull(mt)

data1 <- data |>
  filter(time >= mt) |>
  mutate(time = floor_date(time, "year") ) |>
  group_by(geo, time, nace_r2) |>
  summarize(
    across(c(van, msa), ~sum(.x)/n() * 4) ) |>
  group_by(geo, time) |>
  mutate(
    s = msa/van,
    p = van/sum(van)) |>
  group_by(geo, nace_r2) |>
  mutate(
    s0 = p[time==mt] * s[time==mt],
    s_sec = (p - p[time==mt]) * s,
    s_sec2 = (p - p[time==mt]) * s[time==mt],
    s_psal = p[time==mt] * (s - s[time==mt]),
    s_psal2 = p * (s - s[time==mt]) )|>
  group_by(geo, time) |>
  summarize(
    across(c(s0, s_sec, s_psal, s_sec2, s_psal2), sum),
    .groups ="drop") |>
  mutate(geo = factor(geo, pays))

return(data1)
