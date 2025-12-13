library(tidyverse)
library(eurostat)
library(ofce)

nace <- source_data("nace.r")
pays <- nace$pays3
m_a20 <- nace$nace  |> pull(a20) |> unique()
m_a10 <- nace$nace |> pull(a10) |> unique()

adj <- c("SCA", "SA", "CA", "NSA")

naq <- get_eurostat("namq_10_a10",
                    filters = list(
                      nace_r2 = m_a10,
                      na_item = "D1",
                      geo = pays,
                      unit = "CP_MEUR") ) |>
  drop_na() |>
  mutate(s_adj = factor(s_adj, adj)) |>
  group_by(nace_r2, geo, time) |>
  summarise(across(c(values, s_adj), first),
            .groups = "drop") |>
  select(geo, time, nace_r2, values, s_adj) |>
  rename(D1 = values)

naa_e.raw <- "nama_10_a64_e" |>
  get_eurostat(
    filters = list(nace_r2  = m_a20, geo = pays,
                   unit = c("THS_PER", "THS_HW"),
                   na_item = c("SAL_DC", "SELF_DC")) ) |>
  mutate(unit = case_match(unit, "THS_PER" ~ "p", "THS_HW" ~ "h")) |>
  drop_na(values) |>
  select(nace_r2, geo, time, values, na_item, unit) |>
  pivot_wider(names_from = c(na_item, unit), values_from = values) |>
  rename(SAL_DC = SAL_DC_p, SELF_DC = SELF_DC_p) |>
  left_join(nace$nace |> select(a20, a10), by = c("nace_r2"="a20")) |>
  group_by(a10, geo, time) |>
  summarize(across(c(SAL_DC, SELF_DC, SAL_DC_h, SELF_DC_h), sum),
            .groups = "drop") |>
  rename(nace_r2 = a10)

emp_dom <- naa_e.raw |>
  drop_na() |>
  group_by(geo) |>
  summarize(
    deb = year(min(time)), fin = year(max(time)))

naa_e <- naa_e.raw |>
  transmute(
    geo, time, nace_r2, SAL_DC, SELF_DC, SAL_DC_h, SELF_DC_h) |>
  cross_join(tibble(month = seq(1, 10, by=3))) |>
  mutate(time = str_c(year(time), " ", month) |> lubridate::ym()) |>
  select(-month)

naq_e <- naq |>
  left_join(naa_e, by = c("geo", "time", "nace_r2")) |>
  group_by(geo, nace_r2) |>
  arrange(time) |>
  fill(SAL_DC, SELF_DC) |>
  group_by(geo, time, nace_r2) |>
  summarize( salw = sum(D1, na.rm = TRUE),
             selfw = sum(D1/SAL_DC*SELF_DC, na.rm = TRUE),
             sal = sum(SAL_DC, na.rm = TRUE),
             self = sum(SELF_DC, na.rm=TRUE),
             .groups = "drop" ) |>
  transmute( time, geo, nace_r2,
             tsalw = ifelse(salw>0, 1+selfw/salw,1),
             tsal = ifelse(sal>0, 1+self/sal, 1) ) |>
  complete(time, geo, nace_r2) |>
  group_by(geo, nace_r2) |>
  fill(tsal, tsalw) |>
  ungroup() |>
  drop_na(tsal)

return(list(naq_e = naq_e, naa_e = naa_e, dom = emp_dom))
