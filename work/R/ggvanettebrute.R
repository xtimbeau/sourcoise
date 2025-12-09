ofce::init_qmd()

naq_full <- ofce::source_data("vaq.r")$naa |>
  filter(champ == "mdhi")

data <- naq_full |>
  mutate(year = year(time)) |>
  filter( year %in% c(1995, 2025)) |>
  select(year, geo, psalh, van, psalhb, vab) |>
  drop_na(psalh, van) |>
  pivot_wider(names_from = year, values_from = c(psalh,van, psalhb, vab)) |>
  mutate(geo = fct_reorder(geo, psalh_2025)) |>
  mutate(geo2c = countrycode(geo, "eurostat", "iso2c"))

data_int <- data |>
  full_join(naq_full |> select(time, psalh, geo), by = "geo") |>
  drop_na(psalh) |>
  group_by(geo) |>
  mutate(
    year = year(time),
    i = (year - 1995)/(2025 - 1995),
    psal_seg = psalh_2025 * i  + psalh_1995 * (1-i),
    x  = geo, xend = geo,
    y = lag(psal_seg),
    yend = psal_seg ) |>
  mutate(
    tooltip = glue("<b>{lbl(geo)}</b>
                     Part des salaires dans la VAN en {year} : {round(100*psalh,1)}%
                     (VA nette, cor. non sal., marchandes hors imm.)")) |>
  ungroup()


data_intb <- data |>
  mutate(geo = fct_reorder(geo, psalhb_2025)) |>
  full_join(naq_full |> select(time, psalhb, geo), by = "geo") |>
  drop_na(psalhb) |>
  group_by(geo) |>
  mutate(
    year = year(time),
    i = (year - 1995)/(2025 - 1995),
    psal_seg = psalhb_2025 * i  + psalhb_1995 * (1-i),
    x  = geo, xend = geo,
    y = lag(psal_seg),
    yend = psal_seg ) |>
  mutate(
    tooltip = glue("<b>{lbl(geo)}</b>
                     Part des salaires dans la VAB en {year} : {round(100*psalhb,1)}%
                     (VA brute, cor. non sal., marchandes hors imm.)")) |>
  ungroup()


return(list(data = data, int = data_int, intb = data_intb))
