library(OECD)
library(tidyverse)
library(glue)
sna <- "OECD.SDD.NAD,DSD_NAMAIN1@DF_QNA,1.1"
sectors <- "OECD.SDD.NAD,DSD_NASEC10@DF_TABLE14,1.1"

eo <- "OECD.ECO.MAD,DSD_EO@DF_EO,1.3"
query <- "A..GBR.........."
pays <- "USA+FRA+DEU+GBR+ITA+ESP+NLD+BEL+JPN"
# jpn <-  OECD::get_dataset(sna, query)
# sna_str <- OECD::get_data_structure(sna)

b1g <-  OECD::get_dataset(sna, "Q..{pays}...B1G.._T...V.N.T0101" |> glue())
b1gq <-  OECD::get_dataset(sna, "Q..{pays}...B1GQ....XDC.V.N.T0103" |> glue())
p51c <- OECD::get_dataset(sna, "Q..{pays}...P51C.....V.N.T0103" |> glue())
sal <- OECD::get_dataset(sna, "Q..{pays}...SAL.._T.....T0110" |> glue())
self <- OECD::get_dataset(sna, "Q..{pays}...SELF.._T.....T0110" |> glue())
d1 <- OECD::get_dataset(sna, "Q..{pays}...D1.._T.....T0103" |> glue())

p51c_eo <- OECD::get_dataset(eo, "{pays}.PITISK+KTPV+RSCRP.A" |> glue())

self_eo <- OECD::get_dataset(eo, "{pays}.EG+ET+ES.Q" |> glue()) |>
  select(geo = REF_AREA, time = TIME_PERIOD, value = ObsValue, var = MEASURE) |>
  mutate(time = yq(time),
         value = as.numeric(value)) |>
  pivot_wider(names_from = var, values_from = value) |>
  mutate(pnsal = ES/(ET-ES)) |>
  filter(year(time)>=1995) |>
  select(geo, time, pnsal)

map(list(b1g, b1gq, p51c, sal, self, d1),
    ~.x |>
      mutate(ADJUSTMENT = factor(ADJUSTMENT, c("Y", "N"))) |>
      arrange(ADJUSTMENT) |>
      group_by(across(-c(ADJUSTMENT, ObsValue))) |>
      summarize(across(c(ADJUSTMENT, ObsValue), first),
                .groups = "drop") |>
      group_by(REF_AREA) |>
      summarize(n=n(), nt = n_distinct(TIME_PERIOD), mint = min(TIME_PERIOD), maxt = max(TIME_PERIOD), adj = n_distinct(ADJUSTMENT)))

data <- map_dfr(list(b1g, b1gq, p51c, sal, self, d1),
    ~.x |>
      mutate(ADJUSTMENT = factor(ADJUSTMENT, c("Y", "N"))) |>
      arrange(ADJUSTMENT) |>
      group_by(across(-c(ADJUSTMENT, ObsValue))) |>
      summarize(across(c(ADJUSTMENT, ObsValue), first),
                .groups = "drop") |>
      select(geo = REF_AREA, time = TIME_PERIOD, value = ObsValue, var = TRANSACTION) |>
      mutate(time = yq(time),
             value = as.numeric(value),
             var = tolower(var)) ) |>
      pivot_wider(names_from = var, values_from = value) |>
  left_join(self_eo, by = c("time", "geo")) |>
  mutate(pnsal_sna = self/sal) |>
  group_by(geo) |>
  mutate(
    across(c(b1g, b1gq, p51c, d1),
           ~slider::slide_dbl(.x, .before=3L, .f = ~mean(.x, na.rm = TRUE) ))) |>
  mutate(psalaire = d1*(1+pnsal)/(b1gq-p51c)) |>
  ungroup() |>
  mutate(
    geo = countrycode::countrycode(geo, "iso3c", "eurostat"))

return(data)
