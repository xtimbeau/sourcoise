
library(eurostat)
library(tidyverse)
library(ggbump)
library(ofce)

ze <- c("DE", "FR", "IT", "ES", "NL", "BE", "IE", "AT", "FI", "PT", "EL", "SK", "LU", "LT", "HR", "SI", "LV", "EE", "CY", "MT", "EA20")
pays <- c("DE", "FR", "IT", "ES", "NL", "BE", "UK", "US")

sal <-  get_eurostat("earn_nt_net", filters = list(estruct = "NET", geo = pays, currency = "PPS", ecase = c("CPL_CH2_AW100_67", "CPL_CH2_AW100", "P1_NCH_AW100", "P1_CH2_AW67"))) |>
  mutate(
    adultes = case_when(
      str_detect(ecase, "CPL") ~ 2,
      str_detect(ecase, "P1") ~ 1 ),
    enfants = case_when(
      str_detect(ecase, "CH2") ~ 2,
      str_detect(ecase, "NCH") ~ 0),
    uc = case_match(adultes,
                    2 ~ 1.5,
                    1 ~ 1) +  enfants*0.3,
    netperuc = values/uc) |>
  group_by(time, ecase) |>
  mutate(rank  = rank(-netperuc)) |>
  group_by(time, geo) |>
  mutate(relperuc = netperuc/netperuc[ecase == "P1_NCH_AW100"]) |>
  ungroup()

ggplot(sal) +
  aes(x=time, y=rank, color = geo)+
  scale_y_reverse() +
  geom_bump() +
  geom_point(size = 4) +
  # scale_y_log10() +
  facet_wrap(vars(ecase)) +
  geom_text(aes(label = geo), size = 2, col = "white") +
  theme_ofce()

ggplot(sal) +
  aes(x=time, y=netperuc, color = ecase)+
  geom_bump() +
  geom_point(size = 1) +
  scale_y_log10() +
  facet_wrap(vars(geo)) +
  theme_ofce()

ggplot(sal) +
  aes(x=time, y=relperuc, color = ecase)+
  geom_bump() +
  geom_point(size = 1) +
  scale_y_log10() +
  facet_wrap(vars(geo)) +
  theme_ofce()

minwage <- OECD::get_dataset("OECD.ELS.SAE,DSD_EARNINGS@RMW,", "...A...") |>
  filter(UNIT_MEASURE == "USD_PPP") |>
  select(time = TIME_PERIOD, geo = REF_AREA, v = ObsValue) |>
  mutate(time = ym(str_c(time, "-01")), v = as.numeric(v)) |>
  drop_na()

ggplot(minwage |> filter(time >= "2000-01-01", geo %in% c("FRA", "USA", "DEU", "ITA", "GBR", "ESP"))) +
  aes(x = time, y = v, color = geo, group = geo) +
  geom_line()
