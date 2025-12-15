library(tidyverse)
library(eurostat)
library(ofce)

marchand <- source_data("nace.r")$marchand
marchand2 <- source_data("nace.r")$marchand2
nace <- source_data("nace.r")$nace

pays2 <- c("DE", "FR", "IT", "ES", "NL", "BE")
pays3 <- c("EA20", "US", "CH")

adj <- c("SCA", "SA", "CA", "NSA")

label_pays <- set_names(countrycode::countrycode(pays2, "eurostat", "country.name.fr"), pays2)

snfia <- "naidsa_10_nf_tr" |>
  get_eurostat(filters = list(
    sector = "S11",
    na_item = c("B1G", "B1N", "P51C", "D1", "D2", "D21", "D29", "D3", "D31", "D39", "D42", "D5"),
    geo = pays3  )) |>
  mutate(unit = factor(unit, c("CP_MEUR", "CP_MNAC"))) |>
  drop_na() |>
  group_by(direct, na_item, sector, geo, time) |>
  summarize(values = first(values, unit),
            unit = first(unit, unit),
            .groups = "drop") |>
  pivot_wider(names_from = direct, values_from = values) |>
  mutate(across(c(PAID, RECV), ~replace_na(.x, 0))) |>
  mutate(values = ifelse(na_item %in% c("B1G", "B1N", "D1", "D5", "P51C"), PAID, PAID-RECV)) |>
  select(-PAID, -RECV) |>
  pivot_wider(names_from = na_item, values_from = values) |>
  mutate(
    vab = B1G,
    van = B1N,
    msa = D1,
    psal = D1/B1N,
    div = D42/B1N,
    isp = (D5+D2+D3)/B1N,
    tp = (B1N-D1-D5-D2-D3)/B1N,
    label_geo = eurostat::label_eurostat(geo,"geo",lang="fr"))

assetsfi <- "naidsa_10_f_bs" |>
  get_eurostat(
    filters = list(unit = "MIO_EUR", na_item = c("F511", "F512", "BF90"), sector = "S11",
                   geo = pays3)) |>
  drop_na(values) |>
  mutate(co_nco = factor(co_nco, c("CO","NCO"))) |>
  group_by(sector, finpos, na_item, time, geo) |>
  summarize(values = first(values, co_nco),
            co_nco = first(co_nco, co_nco),
            .groups = "drop") |>
  pivot_wider(names_from = finpos, values_from = values) |>
  mutate(across(c(ASS, LIAB), ~replace_na(.x, 0)),
         NET = LIAB - ASS) |>
  select(-ASS, -LIAB) |>
  pivot_wider(names_from = na_item, values_from = NET) |>
  mutate(net_shares = F511+F512, net_shares_augmented = net_shares - BF90)

return(list(snfia = snfia, assetsfi = assetsfi))
