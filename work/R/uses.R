library(tidyverse)
library(eurostat)
library(iotables)
library(ofce)
library(melodi)

nace <- source_data("ace.r")$nace
a20 <- nace$a20
pays <- source_data("nace.r")$pays1

vahi <- source_data("vaq.r")$naa |>
  filter(champ %in% c("mdhi"), geo %in% P6) |>
  select(geo, time, van, vab)

uses_1610 <- "naio_10_cp1610" |>
  iotables::iotables_download() |>
  filter(geo %in% pays,
         unit == "MIO_EUR",
         stk_flow == "TOTAL") |>
  unnest(data) |>
  filter(ind_use %in% a20,
         prd_ava %in% c("CPA_L68B")) |>
  rename(CI_L = values) |>
  drop_na() |>
  left_join(nace |> select(a20, md = marchand, hi=hors_imm), by = c("ind_use"="a20")) |>
  group_by(geo, time) |>
  summarize(
    across(c(CI_L), ~sum(.x[md&hi]), .names = "{.col}"),
    .groups = "drop") |>
  select(geo, time, CI_L) |>
  left_join(vahi, by = c("geo", "time")) |>
  arrange(geo, desc(time))

uses_15 <- "naio_10_cp15" |>
  iotables::iotables_download() |>
  filter(geo %in% pays,
         unit == "MIO_EUR",
         stk_flow == "TOTAL") |>
  unnest(data) |>
  filter(ind_impv %in% a20,
         prd_amo %in% c("CPA_L68B")) |>
  rename(CI_L = values) |>
  drop_na(CI_L) |>
  left_join(nace |> select(a20, md = marchand, hi=hors_imm), by = c("ind_impv"="a20")) |>
  group_by(geo, time) |>
  summarize(
    across(c(CI_L), ~sum(.x[md&hi]), .names = "{.col}"),
    .groups = "drop") |>
  select(geo, time, CI_L) |>
  left_join(vahi, by = c("geo", "time")) |>
  arrange(geo, desc(time))

return(list(u15 = uses_15, u1610 = uses_1610))
