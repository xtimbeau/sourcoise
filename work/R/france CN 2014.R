library(tidyverse)
library(insee)
library(ofce)

md <- tibble::tribble(
  ~naces, ~md, ~mdhi,  ~mdhifi, ~mdhfi,
  "AZ",   TRUE,  TRUE,   TRUE,   TRUE,
  "C1",   TRUE,  TRUE,   TRUE,   TRUE,
  "C2",   TRUE,  TRUE,   TRUE,   TRUE,
  "C3",   TRUE,  TRUE,   TRUE,   TRUE,
  "C4",   TRUE,  TRUE,   TRUE,   TRUE,
  "C5",   TRUE,  TRUE,   TRUE,   TRUE,
  "DE",   TRUE,  TRUE,   TRUE,   TRUE,
  "FZ",   TRUE,  TRUE,   TRUE,   TRUE,
  "GZ",   TRUE,  TRUE,   TRUE,   TRUE,
  "HZ",   TRUE,  TRUE,   TRUE,   TRUE,
  "IZ",   TRUE,  TRUE,   TRUE,   TRUE,
  "JZ",   TRUE,  TRUE,   TRUE,   TRUE,
  "KZ",   TRUE,  TRUE,   FALSE,  FALSE,
  "LZ",   TRUE,  FALSE,  FALSE,  TRUE,
  "MN",   TRUE,  TRUE,   TRUE,   TRUE,
  "OQ",   FALSE, FALSE,  FALSE,  FALSE,
  "RU",   TRUE,  TRUE,   TRUE,   TRUE )

curl::curl_download('https://www.insee.fr/fr/statistiques/fichier/7455994/T_6461_6462.xlsx',
                    destfile = "/tmp/T_6461_6462.xlsx")

T6461 <- readxl::read_xlsx("/tmp/T_6461_6462.xlsx", sheet = "T_6461", skip = 4) |>
  rename(nace = `...1`, nace_lbl = `...2`) |>
  drop_na(nace_lbl) |>
  pivot_longer(c(-nace,-nace_lbl), names_to = "time") |>
  mutate(time = ymd(time, truncated=2),
         value = 1000*value) |>
  arrange(time) |>
  rename(ccf = value) |>
  separate(nace, sep="_", into = c("level", "naces"))

idcn14 <- insee::get_idbank_list("CNA-2014-CPEB") |>
  filter(
    OPERATION %in% c("D1", "B1G", "D29", "D39", "B3G"),
    NATURE == "VALEUR_ABSOLUE",
    PRIX_REF == "VAL",
    str_detect(CNA_ACTIVITE, "^A17"))

b3g <- insee::get_idbank_list("CNA-2014-CSI") |>
  filter(OPERATION %in% c("B3N", "B3G"),
         SECT_INST == "S14AA",
         COMPTE == "EA") |>
  pull(idbank) |>
  insee::get_insee_idbank() |>
  transmute(time = ymd(TIME_PERIOD, truncated = 2),
            id = case_when(
              IDBANK == "010563062" ~ "B3G",
              IDBANK == "010563072" ~ "B3N" ),
            value = OBS_VALUE) |>
  pivot_wider(names_from = id, values_from = value) |>
  arrange(time) |>
  mutate(rccfei = (B3G-B3N)/B3G) |>
  fill(rccfei, .direction = "up") |>
  mutate(ccfei = B3G*rccfei)

cn14 <- idcn14 |>
  pull(idbank) |>
  insee::get_insee_idbank() |>
  left_join(idcn14 |> select(idbank, OPERATION, CNA_ACTIVITE), by=c("IDBANK"="idbank")) |>
  select(OPERATION, nace = CNA_ACTIVITE, time=DATE, OBS_VALUE) |>
  pivot_wider(names_from = OPERATION, values_from = OBS_VALUE) |>
  rename_with(tolower) |>
  mutate(rb3g = b3g/d1) |>
  arrange(time)

idemp <- "CNA-2014-EMPLOI" |>
  insee::get_idbank_list() |>
  filter(SECT_INST == "S10",
         CNA_ACTIVITE |> str_detect("^A17"),
         UNIT_MEASURE == "MILLIERS_ACTIFS_OCCUPES_PP",
         CNA_TYPE_EMP %in% c("E10", "E20", "E30"))

emp <- idemp |>
  pull(idbank) |>
  insee::get_insee_idbank() |>
  left_join(idemp |> select(idbank, CNA_TYPE_EMP, CNA_ACTIVITE), by=c("IDBANK"="idbank")) |>
  select(time = DATE, nace = CNA_ACTIVITE, sns = CNA_TYPE_EMP, value = OBS_VALUE) |>
  pivot_wider(names_from = sns, values_from = value) |>
  rename(emp = E10, emps = E20, empns = E30) |>
  drop_na(emp, emps, empns) |>
  arrange(time) |>
  complete(time, nace, fill = list(emps = 0, empns = 0)) |>
  mutate(empns = emp - emps)

cn14e <- cn14 |>
  left_join(emp, by = c("nace", "time")) |>
  mutate(rb3gpc = b3g/empns / (d1/emps),
         nab3g = is.na(b3g)) |>
  arrange(time) |>
  group_by(nace) |>
  fill(rb3gpc, .direction = "up") |>
  mutate(b3ne = d1/emps*empns,
         naces = str_split_i(nace,"-", 2)) |>
  mutate(b3ne = replace_na(b3ne, 0))

total_b3g <- cn14e |>
  group_by(time) |>
  summarize(b3ne = sum(b3ne)) |>
  left_join(b3g |> select(time, B3G, rccfei), by = "time") |>
  mutate(vv = b3ne/((1-rccfei)*B3G) )

cn14e2 <- cn14e |>
  left_join(total_b3g |> select(time, vv, rccfei), by = 'time') |>
  mutate(b3nec = b3ne/vv) |>
  left_join(T6461 |> select(time, naces, ccf), by = c("time", "naces")) |>
  mutate(across(c(ccf, d29, d39), ~.x/b1g, .names = "r{.col}"),
         across(c(ccf, d29, d39), ~is.na(.x), .names = "na{.col}")) |>
  fill(starts_with("r"), .direction = "up") |>
  ungroup() |>
  mutate(
    ccf = rccf * b1g,
    d29 = rd29 * b1g,
    d39 = rd39 * b1g  ) |>
  mutate(
    van = b1g - ccf - d29 - d39,
    msal = d1 + b3ne,
    msalc = d1 + b3nec) |>
  left_join(md, by = "naces")

# ca ressemble trop à la base 2020
# assets <- "https://www.insee.fr/fr/statistiques/fichier/7455994/DD_CNA_CAPITAL_FIXE.zip" |>
#   archive::archive_read(file="data.csv") |>
#   vroom::vroom()

cn14a <- cn14e2 |>
  filter(time<="2020-01-01") |>
  mutate(
    across(c(b3g), ~replace_na(.x, 0))) |>
  group_by(time) |>
  summarize(
    across(c(naccf, nad29, nad39, nab3g), any),
    across(c(b1g, ccf, van, msal, msalc, d29, d39, d1, b3g),
           ~sum(.x), .names = "{.col}_tb"),
    across(c(b1g, ccf, van, msal, msalc, d29, d39, d1, b3g),
           ~sum(.x[md]), .names = "{.col}_md"),
    across(c(b1g, ccf, van, msal, msalc, d29, d39, d1, b3g),
           ~sum(.x[mdhi]), .names = "{.col}_mdhi"),
    across(c(b1g, ccf, van, msal, msalc, d29, d39, d1, b3g),
           ~sum(.x[mdhifi]), .names = "{.col}_mdhifi"),
    across(c(b1g, ccf, van, msal, msalc, d29, d39, d1, b3g),
           ~sum(.x[mdhfi]), .names = "{.col}_mdhfi"),
    .groups = "drop") |>
  pivot_longer(-c(time, starts_with("na"))) |>
  separate(name, sep = "_", into = c("var", "champ")) |>
  pivot_wider(names_from = var, values_from = value) |>
  mutate(
    psal = msal / van , # par les effectifs
    psalc = msalc / van  ) # par les effectifs normalisé par le b3g des EI

return(cn14a)

# cn14a |> ggplot() + geom_line(aes(x=time, y=psal))+facet_wrap(vars(champ))
