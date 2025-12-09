library(tidyverse)
library(ofce)

pays <- source_data("nace.r")$pays1

mixte <- source_data("mixte.r")$a |>
  mutate(champ = "tb")

nace <- source_data("nace.r") |>
  pluck("nace") |>
  transmute(nace_r2=a20,
            md = marchand,
            hi = marchand&hors_imm,
            hifi = hifi&marchand,
            hfi = hfi&marchand)

d1 <- source_data("vaq.r")$naa |>
  select(geo, time, d1 = msanc, champ)

nsalw <- source_data("mixte.r")$ilc |>
  mutate(champ = "tb")

mixte_fr <- source_data("france-melodi.r")$aggr |>
  transmute(time, champ, b3g, b3n = msam-msanc, geo = "FR")

mes_sumarize <- c("sal", "self", "salh", "selfh")
effectifs <- source_data("naq10_e.r")$naa_e |>
  mutate(time = floor_date(time, "year")) |>
  group_by(geo, time, nace_r2) |>
  summarize(sal = first(SAL_DC), self = first(SELF_DC),
            salh = first(SAL_DC_h), selfh = first(SELF_DC_h),
            .groups = "drop") |>
  left_join(nace, by = "nace_r2") |>
  group_by(geo, time) |>
  summarize(
    across(all_of(mes_sumarize), ~sum(.x[md], na.rm=TRUE), .names = "{.col}_md"),
    across(all_of(mes_sumarize), ~sum(.x[hi], na.rm=TRUE), .names = "{.col}_mdhi"),
    across(all_of(mes_sumarize), ~sum(.x[hifi], na.rm=TRUE), .names = "{.col}_mdhifi"),
    across(all_of(mes_sumarize), ~sum(.x[hfi], na.rm=TRUE), .names = "{.col}_mdhfi"),
    across(all_of(mes_sumarize), ~sum(.x[hi], na.rm=TRUE), .names = "{.col}_mdhim"),
    across(all_of(mes_sumarize), ~sum(.x[hi&hfi], na.rm=TRUE), .names = "{.col}_mdhimfi"),
    across(all_of(mes_sumarize), ~sum(.x, na.rm=TRUE), .names = "{.col}_tb"),
    .groups = "drop")  |>
  pivot_longer(starts_with(c("sal", "self"))) |>
  separate(name, into = c("var", "champ"), sep = "_") |>
  pivot_wider(names_from = var, values_from = value) |>
  left_join(d1, by = c("geo", "time", "champ")) |>
  left_join(mixte, by = c("geo", "time", "champ")) |>
  left_join(nsalw, by= c("geo", "time", "champ")) |>
  left_join(mixte_fr, by= c("geo", "time", "champ"), suffix = c("",".fr"))

return(effectifs)
