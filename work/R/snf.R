library(tidyverse)
library(eurostat)
library(ofce)

marchand <- source_data('marchand.r')$marchand
pays <- c("DE", "FR", "IT", "ES", "NL", "BE", "IE", "AT", "FI", "PT", "EL", "SK", "LU", "LT", "HR", "SI", "LV", "EE", "CY", "MT", "EA20")
pays2 <- c("DE", "FR", "IT", "ES", "NL", "BE")
label_pays <- set_names(countrycode::countrycode(pays2, "eurostat", "country.name.fr"), pays2)

naa_e <- source_data("naa_e.r")

nasa.raw <- get_eurostat("nasa_10_nf_tr",
                         filters = list(na_item = c("B1G", "P51C", "B2A3N", "D51", "B1N", "D29", "D39", "D1"),
                                        geo = pays2, unit = "CP_MEUR",
                                        sector=c("S11"))) |>
  drop_na()

nasa <- nasa.raw |>
  pivot_wider(names_from = c(na_item, direct), values_from = values) |>
  left_join(naa_e, by = c("time", "geo")) |>
  mutate(
    van = B1G_PAID - P51C_PAID,
    tp = (van - D1_PAID  - D29_PAID + D39_RECV - D51_PAID)/van,
    tp_c = (van - D1_PAID * tsal  - D29_PAID + D39_RECV - D51_PAID)/van,
    tis = (D51_PAID + D29_PAID - D39_RECV)/van) |>
  mutate(geo = factor(geo, c("DE", "FR", "IT", "ES", "NL", "BE")))  |>
  select(geo, time, tp, tp_c, tis) |>
  drop_na(tp)

insee <- source_data("insee.r")

nasa <- left_join(
  nasa,
  insee |> filter(champ == "snfei") |> select(time, tp, tp_c, tis, geo),
  suffix = c("", ".alt"),
  by = c("geo", "time") ) |>
  mutate(
    tis.sav = tis,
    tp.sav = tp,
    tp_c.sav = tp_c,
    tis = ifelse(geo == "FR", tis.alt, tis),
    tp = ifelse(geo == "FR", tp.alt, tp),
    tp_c = ifelse(geo == "FR", tp_c.alt, tp_c))

ggplot(nasa |> filter(geo %in% pays2, time >= "1995-01-01")) +
  aes(x = time, y = tp, color = geo, group = geo) +
  geom_line(data = ~ rename(.x, geo2 = geo), aes(group = geo2),
            color = "gray80", linewidth = 0.2) +
  geom_ribbon(aes(ymin= tp, ymax = tp+tis, fill = geo),
              color = "transparent", alpha= 0.2, show.legend = FALSE ) +
  geom_line(show.legend = FALSE) +
  scale_color_pays(format = "eurostat") +
  facet_wrap(vars(geo), labeller = as_labeller(label_pays), ncol = 2) +
  scale_ofce_date() +
  theme_ofce()

ggplot(nasa |> filter(geo %in% pays2, time >= "1995-01-01")) +
  aes(x = time, y = tp_c, color = geo, group = geo) +
  geom_line(data = ~ rename(.x, geo2 = geo), aes(group = geo2),
            color = "gray80", linewidth = 0.2) +
  geom_ribbon(aes(ymin= tp_c, ymax = tp_c+tis, fill = geo),
              color = "transparent", alpha= 0.2, show.legend = FALSE ) +
  geom_line(show.legend = FALSE) +
  scale_color_pays(format = "eurostat") +
  facet_wrap(vars(geo), labeller = as_labeller(label_pays), ncol = 2) +
  scale_ofce_date() +
  theme_ofce()

