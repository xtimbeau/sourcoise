library(tidyverse)
library(ofce)

curl::curl_download("https://www.insee.fr/fr/statistiques/fichier/8068608/T_7101.xlsx",
                    "t7101.xlsx")
curl::curl_download("https://www.insee.fr/fr/statistiques/fichier/8068608/T_7102.xlsx",
                    "t7102.xlsx")

t7101 <- readxl::read_xlsx("t7101.xlsx", skip = 4, sheet = 2) |>
  rename(code = `...1`, lib = `...2`) |>
  mutate(direct = ifelse(lib=="Emplois", "E",
                         ifelse(lib == "Ressources", "R", NA_character_))) |>
  relocate(direct, .after = lib) |>
  fill(direct, .direction = "down") |>
  drop_na(code, lib) |>
  pivot_longer(cols = -c(code, lib, direct), names_to = "year") |>
  drop_na(value)

t7102 <- readxl::read_xlsx("t7102.xlsx", skip = 4, sheet = 2) |>
  rename(code = `...1`, lib = `...2`) |>
  mutate(direct = ifelse(lib=="Emplois", "E",
                         ifelse(lib == "Ressources", "R", NA_character_))) |>
  relocate(direct, .after = lib) |>
  fill(direct, .direction = "down") |>
  drop_na(code, lib) |>
  pivot_longer(cols = -c(code, lib, direct), names_to = "year") |>
  drop_na(value)

naa_e <- source_data("naa_e.r") |>
  filter(geo == "FR") |>
  transmute(year = year(time) |> as.character(), tsal)

snfei <- t7101 |>
  full_join( t7102, by = c("code", "direct", "year"),
             suffix = c("", ".ei") ) |>
  filter(year>=1995) |>
  mutate(
    value.ei = replace_na(value.ei, 0),
    snfei = value + value.ei ) |>
  select(code, year, direct, snfei) |>
  complete(code, year, direct, fill = list(snfei = 0)) |>
  mutate(snfei = ifelse(code == "B1G" & direct == "E", 0, snfei)) |>
  group_by(code, year) |>
  summarize(snfei = snfei[direct=="R"] - snfei[direct=="E"]) |>
  filter(code %in% c("B1G", "P51C", "D29", "D39", "D1", "D11", "D12", "D51")) |>
  pivot_wider(id_cols = c(year), names_from = code, values_from = snfei) |>
  left_join(naa_e, by = c("year")) |>
  mutate(
    van = B1G + P51C,
    imp = D29+D39+D51,
    tp = (van + D1 + imp)/van,
    tp_c = (van + D1*tsal + imp)/van,
    tis = -imp/van ) |>
  select(year, tp, tp_c, tis)  |>
  mutate(geo = "FR",
         champ = "snfei")

snf <- t7101 |>
  filter(year>=1995, code %in% c("B1G", "P51C", "D29", "D39", "D1", "D11", "D12", "D51"), !(code=="B1G"&direct=="R")) |>
  select(-direct, -lib) |>
  pivot_wider(names_from = code, values_from = value) |>
  left_join(naa_e, by = "year") |>
  mutate(
    van = B1G - P51C,
    imp = D29+D39+D51,
    tp = (van - D1 - imp)/van,
    tp_c = (van - D1*tsal - imp)/van,
    tis = imp/van ) |>
  select(year, tp,, tp_c, tis) |>
  mutate(geo = "FR",
         champ = "snf")

insee <- bind_rows(snf, snfei) |>
  mutate(time = ym(str_c(year, "-01")))

return(insee)
