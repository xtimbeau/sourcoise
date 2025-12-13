library(tidyverse)
library(eurostat)

pays2 <- c("DE", "FR", "IT", "ES", "NL", "BE")
nace <- source_data("nace.r")$nace
m_a20 <- nace  |> pull(a20) |> unique()
m_a10 <- nace |> pull(a10) |> unique()
marchand <- source_data("nace.r")$marchand

naa <- get_eurostat("nama_10_a64",
                    filters = list(
                      nace_r2 = marchand,
                      na_item = "D1",
                      geo = pays2,
                      unit = "CP_MEUR") ) |>
  drop_na() |>
  rename(D1 = values)

naa_e.raw <- get_eurostat("nama_10_a64_e",
                          filters = list(nace_r2  = marchand, geo = pays2,
                                         unit = c("THS_PER"),
                                         na_item = c("SAL_DC", "SELF_DC")) ) |>
  drop_na(values) |>
  select(nace_r2, geo, time, values, na_item) |>
  pivot_wider(names_from = na_item, values_from = values)

naa_e <- naa_e.raw |>
  transmute(
    geo, time, nace_r2,
    tsal = 1+ SELF_DC/SAL_DC)

naa_ea <- naa_e.raw |>
  left_join(naa, by = c("geo", "time", "nace_r2")) |>
  group_by(time, geo) |>
  summarize( salw = sum(D1, na.rm = TRUE),
             selfw = sum(D1/SAL_DC*SELF_DC, na.rm = TRUE),
             sal = sum(SAL_DC, na.rm = TRUE),
             self = sum(SELF_DC, na.rm=TRUE),
             .groups = "drop" ) |>
  transmute( time, geo,
             tsalw = 1+selfw/salw,
             tsal = 1+self/sal )

return(list(naa_e = naa_e, naa_ea = naa_ea))
