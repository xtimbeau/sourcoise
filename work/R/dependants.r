library(eurostat)
library(ofce)
library(tidyverse)

ofce::init_qmd()

pays2 <- c("DE", "FR", "IT", "ES", "NL", "BE")
label_pays <- set_names(countrycode::countrycode(pays2, "eurostat", "country.name.fr"), pays2)

pop <- get_eurostat("demo_pjan", filters = list(geo = pays2, sex = c("M", "F"))) |>
  mutate(pop = values/1000) |>
  mutate(nage = str_remove(age, "Y") |> as.numeric(),
         nage = ifelse(age ==  "Y_LT1", 0, nage),
         nage = ifelse(age == "Y_GE100", 100, nage)) |>
  filter(!is.na(nage)) |>
  mutate(gage = case_when(
    nage <= 14 ~ 'Y_LT14',
    between(nage, 15, 24) ~ "Y15-24",
    between(nage, 25, 54) ~ "Y25-54",
    between(nage, 55, 64) ~ "Y55-64",
    nage>=65 ~ "Y_GE65" )) |>
  group_by(geo, time, sex, gage) |>
  summarize(pop = sum(pop, na.rm=TRUE))|>
  ungroup()

proj <- get_eurostat("proj_23np", filters = list(geo = pays2, sex = c("M", "F"), unit = "PER", projection ="BSL")) |>
  mutate(pop = values/1000) |>
  mutate(nage = str_remove(age, "Y") |> as.numeric(),
         nage = ifelse(age ==  "Y_LT1", 0, nage),
         nage = ifelse(age == "Y_GE100", 100, nage)) |>
  filter(!is.na(nage)) |>
  mutate(gage = case_when(
    nage <= 14 ~ 'Y_LT14',
    between(nage, 15, 24) ~ "Y15-24",
    between(nage, 25, 54) ~ "Y25-54",
    between(nage, 55, 64) ~ "Y55-64",
    nage>=65 ~ "Y_GE65" )) |>
  group_by(geo, time, sex, gage) |>
  summarize(pop = sum(pop, na.rm=TRUE)) |>
  ungroup()

max_pop <- max(pop$time)
popj <- bind_rows(pop, proj |> filter(time>max_pop)) |>
  arrange(geo, time, sex) |>
  rename(age = gage)

active <- get_eurostat("lfsi_emp_a",
                       filters = list(geo = pays2,
                                      age = c("Y15-24", "Y25-54", "Y55-64"),
                                      unit = "THS_PER",
                                      sex = c("M", "F"),
                                      indic_em = "EMP_LFS")) |>
  mutate(emp = values) |>
  drop_na(emp)

minmin_active <- active |> group_by(geo) |> summarize(time=min(time)) |> summarize(time=max(time)) |> pull(time)

active <- active |>
  filter(time>=minmin_active)

full <- full_join(popj |> select(time, geo, sex, age, pop) |> filter(time>= min(active$time)),
                  active |> select(time, geo, sex, age, emp),
                  by = c("sex", "age", "geo", "time")) |>
  mutate(emp = ifelse(age %in% c("Y_LT14", "Y_GE65"), 0, emp)) |>
  mutate(age = factor(age, c("Y_LT14", "Y15-24", "Y25-54", "Y55-64", "Y_GE65"))) |>
  arrange(geo, time, sex, age) |>
  mutate(tact = emp/pop) |>
  group_by(geo, age, sex) |>
  arrange(time) |>
  fill(tact, .direction = "down") |>
  mutate(emp = ifelse(is.na(emp), pop*tact, emp)) |>
  group_by(geo, age) |>
  mutate(emp.alt = ifelse(time>="2024-01-01", pop*tact[sex=="M"], emp)) |>
  ungroup()

worktime <- get_eurostat("lfsa_epgaed",
                         filters  = list(sex = c("M", "F"),
                                         isced11 = "TOTAL",
                                         geo = pays2,
                                         age = c("Y15-24", "Y25-64", "Y55-64"),
                                         worktime = c("PT", "FT"))) |>
  select(-c(unit, freq, isced11)) |>
  rename(emp = values) |>
  pivot_wider(names_from = age, values_from = emp) |>
  mutate(`Y25-64` = `Y25-64` - `Y55-64`) |>
  rename(`Y25-54` = `Y25-64`) |>
  pivot_longer(cols = starts_with("Y"), names_to = "age", values_to = "emp")

hours <- get_eurostat("lfsa_ewhan2",
                      filters  = list(sex = c("M", "F"),
                                      nace_r2 = "TOTAL",
                                      geo = pays2,
                                      wstatus = "EMP",
                                      age = c("Y15-24", "Y25-54", "Y55-64"),
                                      worktime = c("PT", "FT")))|>
  select(-c(unit, freq, nace_r2, wstatus)) |>
  rename(hpw = values)

eqpt <- worktime |>
  full_join(hours, by = c("age","worktime", "geo", "time", "sex")) |>
  pivot_wider(names_from = worktime, values_from = c(emp, hpw)) |>
  mutate( eqpt = ( emp_PT * hpw_PT + emp_FT * hpw_FT) / hpw_FT /(emp_PT+ emp_FT) ) |>
  drop_na(eqpt) |>
  select(sex, age, geo, time, eqpt)

dep <- full |>
  left_join(eqpt, by = c("age", "geo", "time", "sex")) |>
  arrange(time) |>
  group_by(sex, geo, age) |>
  mutate(eqpt = ifelse(age %in% c("Y_LT14", "Y_GE65"), 1, eqpt)) |>
  fill(eqpt, .direction = "down") |>
  group_by(geo, time) |>
  summarise(
    depse = (sum(pop) - sum(pop[age=="Y_LT14"]) - sum(emp))/sum(emp*eqpt),
    dep = (sum(pop)-sum(emp))/sum(emp*eqpt),
    depse.alt = (sum(pop)- sum(pop[age=="Y_LT14"])-sum(emp.alt))/sum(emp.alt*eqpt),
    .groups = "drop") |>
  mutate(geo = factor(geo, c("DE", "FR", "IT", "ES", "NL", "BE")))

return(dep)

# ggplot(full |> filter(time<="2030-01-01", age != "Y_GE65")) +
#   aes(x=time, y=tact, color = age, linetype = sex) +
#   geom_line() +
#   scale_color_brewer(palette = "Blues") +
#   facet_wrap(vars(geo), ncol = 2) +
#   theme_ofce()
#
# ggplot(worktime |> filter(age == "Y25-54")) +
#   aes(x=time, y = hpw, color = worktime, linetype = sex) +
#   geom_line(show.legend = FALSE) +
#   theme_ofce(marquee = TRUE) +
#   facet_wrap(vars(geo), ncol = 2, labeller = lbl) +
#   ofce_caption(
#     source = "Eurostat demo_pjan pour la population, proj_23np pour les projections de population (baseline), lfsi_emp_a pour l'emploi par age et sexe",
#     note = "Le ratio de dépendance est le ratio entre les inactifs ou chômmeurs, de 0 à 100 ans rapporté aux actifs de 15 à 64 ans.
#   Le trait pointillé indique le ratio de dépendance si le taux d'emploi des femmes est égal à celui des hommes.")
#
# ggplot(dep |> filter(time<='2050-01-01')) +
#   geom_line(aes(x=time, y=depse, color = geo), show.legend = FALSE) +
#   geom_ribbon(aes(x=time, ymin=depse.alt, ymax = depse, fill = geo), color="transparent", alpha = 0.2, show.legend = FALSE) +
#   geom_line(aes(x=time, y=dep, color = geo), linetype="dashed", show.legend = FALSE) +
#   theme_ofce(marquee = TRUE) +
#   facet_wrap(vars(geo), ncol = 2, labeller = lbl) +
#   scale_color_pays(format = "eurostat") +
#   ofce_caption(
#     source = "Eurostat demo_pjan pour la population, proj_23np pour les projections de population (baseline), lfsi_emp_a pour l'emploi par age et sexe",
#     note = "Le ratio de dépendance est le ratio entre les inactifs ou chômmeurs, de 0 à 100 ans rapporté aux actifs de 15 à 64 ans.
#   Le trait pointillé indique le ratio de dépendance si le taux d'emploi des femmes est égal à celui des hommes.")
