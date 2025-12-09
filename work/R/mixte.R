library(tidyverse)
library(eurostat)
library(ofce)

nace <- source_data("nace.r")
pays <- nace$pays3

adj <- c("SCA", "SA", "CA", "NSA")

mixte_q <- "nasq_10_nf_tr" |>
  get_eurostat(
    filters = list(
      unit = "CP_MEUR",
      direct = "PAID",
      na_item = "B3G",
      geo = pays)) |>
  filter(sector == "S14_S15") |>
  mutate(s_adj = factor(s_adj, adj)) |>
  drop_na() |>
  group_by(geo, time) |>
  arrange(s_adj, geo, time) |>
  summarise(b3g = first(values)) |>
  transmute(geo, time, b3g)

mixte_a <- "nasa_10_nf_tr" |>
  get_eurostat(
    filters = list(
      unit = "CP_MEUR",
      direct = "PAID",
      na_item = "B3G",
      geo = pays)) |>
  filter(sector == "S14_S15") |>
  transmute(geo, time, b3g=values, year=year(time)) |>
  drop_na()

mdom <- mixte_a |>
  group_by(geo) |>
  drop_na() |>
  summarize(deb=year(min(time)), fin=year(max(time)))


naq10_e <- source_data("naq10_e.R")$naa_e |>
  select(geo, time, nace_r2, sal = SAL_DC, self = SELF_DC) |>
  mutate(year = year(time))

mixte_h <- mixte_a |>
  select(-time) |>
  left_join(naq10_e , by = c("geo", "year")) |>
  group_by(geo, time) |>
  filter(!all(is.na(sal))) |>
  ungroup() |>
  mutate(self = replace_na(self, 0)) |>
  group_by(geo, time) |>
  mutate(b3gh = b3g/4 * self/sum(self),
         b3nh = 0.88 * b3gh) |>
  select(geo, time, nace_r2, b3gh, b3nh)

mixte_ah <- mixte_h |>
  mutate(time = floor_date(time, "year")) |>
  group_by(geo, time, nace_r2) |>
  summarise(across(c(b3gh, b3nh), sum),
            .groups = "drop")

nsalw <- "ilc_di05" |>
  get_eurostat(
    filters = list(
      wstatus = c("SAL", "NSAL"),
      indic_il = "MEI_E",
      age = "Y16-64",
      sex = "T",
      unit = "EUR",
      geo = pays) ) |>
  transmute(
    time, geo, values, wstatus) |>
  pivot_wider(names_from = wstatus, values_from = values) |>
  drop_na() |>
  mutate(w = NSAL/SAL)

return(list(q = mixte_q, a = mixte_a, h = mixte_h, ah = mixte_ah, ilc = nsalw, dom = mdom))
