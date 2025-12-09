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

naq10_e <- source_data("naq10_e")$naq_e

snfq <-"nasq_10_nf_tr" |>
  get_eurostat(
    filters = list(unit = "CP_MEUR",
                   sector = c("S11", "S12"),
                   s_adj = c("SCA", "NSA"),
                   na_item = c("B1G", "B1N", "D1", "D5", "D4", "D42_TO_D45", "D41", "D42", "D3", "D2"),
                   geo = pays2) ) |>
  drop_na(values) |>
  group_by(geo, time, na_item, direct, sector) |>
  mutate(s_adj = factor(s_adj, c("SCA", "NSA"))) |>
  arrange(s_adj) |>
  summarise(across(c(values, s_adj), first ), .groups = "drop") |>
  pivot_wider(names_from = direct, values_from = values) |>
  mutate(across(c(PAID, RECV), ~replace_na(.x, 0))) |>
  mutate(NET = ifelse(na_item %in% c("B1G", "B1N", "D1", "D5"), PAID, PAID-RECV)) |>
  select(-PAID, -RECV) |>
  pivot_wider(id_cols = c(geo, time, sector),
              names_from =  na_item, values_from = c(NET, s_adj)) |>
  rename_with(~str_remove(.x, "NET_"))

snfqa <- snfq |>
  mutate(year = floor_date(time, unit="year")) |>
  group_by(year, geo, sector) |>
  summarize(
    across(c(B1G, B1N, D1, D4, D41, D42, D42_TO_D45, D5), ~sum(.x)/n()*4) ) |>
  rename(
    time = year )

snfa <- "nasa_10_nf_tr" |>
  get_eurostat(
    filters = list(unit = "CP_MEUR",
                   sector = c("S11", "S12"),
                   na_item = c("B1G", "B1N", "D1", "D5", "D42"),
                   geo = pays2) ) |>
  drop_na(values) |>
  pivot_wider(names_from = direct, values_from = values) |>
  mutate(values = ifelse(na_item %in% c("B1G", "B1N", "D1", "D5"), PAID, PAID-RECV)) |>
  select(-PAID, -RECV) |>
  pivot_wider(id_cols = c(geo, time, sector),
              names_from =  na_item, values_from = values) |>
  rename_with(~str_remove(.x, "values_"))

assetsnfi <- "nama_10_nfa_bs" |>
  get_eurostat(
    filters = list(unit = "CP_MEUR",
                   sector = c("S11",  "S12"),
                   geo = pays2) ) |>
  drop_na(values) |>
  pivot_wider(names_from = asset10, values_from = values) |>
  select(geo, time, sector, N1N, N2N, N11N)

assetsfi <- "naidsa_10_f_bs" |>
  get_eurostat(
    filters = list(unit = "MIO_EUR", na_item = c("F511", "F512", "BF90"), sector = c("S11", "S12"),
                   geo = pays2)) |>
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

return(list(snfq = snfq, snfa = snfa, assetsfi = assetsfi, assetsnfi = assetsnfi))
