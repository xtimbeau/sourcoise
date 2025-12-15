library(tidyverse)
library(eurostat)
library(ofce)
library(gt)
library(OECD)

ofce::init_qmd()

wages <- OECD::get_dataset("OECD.ELS.SAE,DSD_EARNINGS@AV_AN_WAGE", filter = "BEL+ESP+NLD+ITA+DEU+FRA..USD_PPP..Q..") |>
  select(time = TIME_PERIOD, wage = ObsValue, pays = REF_AREA) |>
  mutate(time = ym(str_c(time, "-01")),
         wage = as.numeric(wage)) |>
  arrange(time)

ggplot(wages) +
  aes(x=time, y = wage, col = pays) +
  geom_line(data = ~.x |> rename(pays2 = pays), aes(group = pays2), col = "gray80", linewidth = 0.2) +
  geom_line(show.legend = FALSE) +
  facet_wrap(vars(pays), ncol = 2, labeller = lbl) +
  theme_ofce() +
  scale_y_log10()  +
  scale_ofce_date() +
  scale_color_pays()

earn.raw <- get_eurostat("earn_ses_annual", filters = list(isco08 = c("OC1", "OC9", "TOTAL"),
                                                           geo = "FR",
                                                           worktime = c("TOTAL", "PT", "FT"),
                                                           nace_r2 = c("B-S_X_O", "B-F", "G-N", "B-N", "P-S"),
                                                           age = c("Y_LT30", "Y_GE50", "TOTAL"),
                                                           sex = c("M", "F", "T"),
                                                           indic_se = c("MED_E_PPS", "D9_E_PPS", "D1_E_PPS"))) |>
  select(-freq) |>
  group_by(time) |>
  mutate(vr = values/values[age=="TOTAL"&sex=="T"&indic_se=="MED_E_PPS" &isco08 == "TOTAL" & worktime == "TOTAL"]) |>
  group_by(isco08, age, sex, indic_se, time, geo, nace_r2) |>
  mutate(ptp = (values[worktime == "FT"] - values[worktime == "TOTAL"]) / (values[worktime == "FT"] - values[worktime == "PT"])) |>
  ungroup() |>
  filter( sex == "T" | (age == "TOTAL" & isco08 == "TOTAL" & indic_se == "MED_E_PPS" & worktime == "TOTAL" & nace_r2 == "B-S_X_O") ) |>
  filter( indic_se == "MED_E_PPS" | (age == "TOTAL" & isco08 == "TOTAL" & sex == "T" & worktime == "TOTAL" & nace_r2 == "B-S_X_O") ) |>
  filter( age == "TOTAL" | (indic_se == "MED_E_PPS" & isco08 == "TOTAL" & sex == "T" & worktime == "TOTAL" & nace_r2 == "B-S_X_O") ) |>
  filter( nace_r2 == "B-S_X_O" | (indic_se == "MED_E_PPS" & isco08 == "TOTAL" & sex == "T" & worktime == "TOTAL" & age == "TOTAL") )

earn_r <- earn.raw |>
  filter(worktime == "TOTAL") |>
  pivot_wider(id_cols = c(isco08, age, sex, indic_se, time, nace_r2), names_from = geo, values_from = vr) |>
  mutate(tt = "rel")

earn_p <- earn.raw |>
  filter(age == "TOTAL", isco08 == "TOTAL", indic_se == "MED_E_PPS", worktime == "TOTAL", sex %in% c("M", "F"), nace_r2 == "B-S_X_O") |>
  pivot_wider(id_cols = c(isco08, age, sex, indic_se, time), names_from = geo, values_from = ptp) |>
  mutate(tt = "pt", nace_r2 = "B-S_X_O")

earn <- earn.raw |>
  filter(age == "TOTAL", isco08 == "TOTAL", indic_se == "MED_E_PPS", sex == "T", worktime == "TOTAL", nace_r2 == "B-S_X_O") |>
  pivot_wider(id_cols = c(isco08, age, sex, indic_se, time), names_from = geo, values_from = values) |>
  mutate(tt = "ref", nace_r2 = "B-S_X_O") |>
  bind_rows(earn_p) |>
  bind_rows(earn_r |> filter(! (age == "TOTAL" & isco08 == "TOTAL" & indic_se == "MED_E_PPS" & sex == "T" & nace_r2 == "B-S_X_O") )) |>
  mutate(across(c(BE, DE), ~ ifelse(.x ==1, NA_real_, .x)),
         nace_r2 = factor(nace_r2, c("B-S_X_O", "B-N", "P-S", "B-F", "G-N")),
         tt= factor(tt, c("ref", "pt", "rel"))) |>
  arrange(tt, nace_r2) |>
  select(-time) |>
  mutate(
    label = case_when(
      tt == "ref" ~ "Salaire médian brut annuel (référence)",
      tt == "pt" & sex == "M" ~ "Part des hommes à temps partiel",
      tt == "pt" & sex == "F" ~ "Part des femmes à temps partiel",
      tt== "rel" & isco08 == "TOTAL" & age == "TOTAL" & sex == "T" & indic_se == "D1_E_PPS" ~ "Salaire du premier décile",
      tt== "rel" & isco08 == "TOTAL" & age == "TOTAL" & sex == "T" & indic_se == "D9_E_PPS" ~ "Salaire du dernier décile",
      tt== "rel" & isco08 == "TOTAL" & age == "TOTAL" & sex == "M" & indic_se == "MED_E_PPS" ~ "Salaire des hommes",
      tt== "rel" & isco08 == "TOTAL" & age == "TOTAL" & sex == "F" & indic_se == "MED_E_PPS" ~ "Salaire des femmes",
      tt== "rel" & isco08 == "TOTAL" & age == "Y_LT30" & sex == "T" & indic_se == "MED_E_PPS" ~ "Salaire des moins de 30 ans",
      tt== "rel" & isco08 == "TOTAL" & age == "Y_GE50" & sex == "T" & indic_se == "MED_E_PPS" ~ "Salaire des plus de 50 ans",
      tt== "rel" & isco08 == "OC1" & age == "TOTAL" & sex == "T" & indic_se == "MED_E_PPS" ~ "Salaire des *managers*",
      tt== "rel" & isco08 == "OC9" & age == "TOTAL" & sex == "T" & indic_se == "MED_E_PPS" ~ "Salaire des travailleurs manuels peu qualifiés",
      tt== "rel" & isco08 == "TOTAL" & age == "TOTAL" & sex == "T" & indic_se == "MED_E_PPS" & nace_r2 == "B-N" ~ "Marchand",
      tt== "rel" & isco08 == "TOTAL" & age == "TOTAL" & sex == "T" & indic_se == "MED_E_PPS" & nace_r2 == "P-S" ~ "Non marchand",
      tt== "rel" & isco08 == "TOTAL" & age == "TOTAL" & sex == "T" & indic_se == "MED_E_PPS" & nace_r2 == "B-F" ~ "Industrie",
      tt== "rel" & isco08 == "TOTAL" & age == "TOTAL" & sex == "T" & indic_se == "MED_E_PPS" & nace_r2 == "G-N" ~ "Services") )

return(earn)
