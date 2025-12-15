library(tidyverse)
library(eurostat)
library(ofce)

pays <- source_data("nace.r")$pays2

label_pays <- set_names(countrycode::countrycode(pays, "eurostat", "country.name.fr"), pays)
nace <- source_data("nace.r")$nace
eq <- get_eurostat("namq_10_a10_e",
                   filters = list(na_item = "SAL_DC",
                                  geo = pays,
                                  unit = "THS_PER")) |>
  drop_na(values) |>
  mutate(s_adj = factor(s_adj, c("SCA", "SA", "CA", "NSA"))) |>
  group_by(across(-c(s_adj, values))) |>
  arrange(s_adj) |>
  summarize(values = first(values),
            s_adj = first(s_adj),
            .groups = "drop") |>
  select(geo, time, nace_r2, emp_sal = values)

d1 <- get_eurostat("namq_10_a10",
                   filters = list(na_item = c("D1", "D11", "D12"),
                                  geo = pays,
                                  unit = "CP_MEUR")) |>
  mutate(s_adj = factor(s_adj, c("SCA", "SA", "CA", "NSA"))) |>
  drop_na() |>
  group_by(across(-c(s_adj, values))) |>
  arrange(s_adj) |>
  summarize(values = first(values),
            s_adj = first(s_adj),
            .groups = "drop") |>
  select(geo, time, values, nace_r2, na_item) |>
  pivot_wider(names_from = na_item, values_from = values)

pc <- "namq_10_fcs" |>
  get_eurostat(
    filters = list(na_item = "P31_S14", unit = c("CP_MEUR", "CLV20_MEUR"), geo = pays)) |>
  drop_na(values) |>
  mutate(s_adj = factor(s_adj, c("SCA", "SA", "CA", "NSA"))) |>
  group_by(across(-c(s_adj, values))) |>
  arrange(s_adj) |>
  summarize(values = first(values),
            s_adj = first(s_adj),
            .groups = "drop") |>
  select(geo, time, values, unit) |>
  pivot_wider(names_from = unit, values_from = values) |>
  mutate(pc = CP_MEUR/CLV20_MEUR) |>
  select(geo, time, pc)

salaires <- eq |>
  left_join(d1, by = c("time", "geo", "nace_r2")) |>
  left_join(nace |> distinct(a10, .keep_all = TRUE) |> select(nace_r2 = a10, md=marchand, hifi, hi, hfi),
            by = "nace_r2") |>
  drop_na(md) |>
  group_by(geo, time) |>
  summarize(w = sum(D1)/sum(emp_sal),
            wb = sum(D11)/sum(emp_sal),
            w_md = sum(D1[md])/sum(emp_sal[md]),
            w_nmd = sum(D1[!md])/sum(emp_sal[!md]),
            w_hifi = sum(D1[md&hifi])/sum(emp_sal[md&hifi]),
            w_hi = sum(D1[md&hi])/sum(emp_sal[md&hi]),
            wb_md = sum(D11[md])/sum(emp_sal[md]),
            wb_nmd = sum(D11[!md])/sum(emp_sal[!md]),
            wb_hifi = sum(D11[md&hifi])/sum(emp_sal[md&hifi]),
            wb_hi = sum(D11[md&hi])/sum(emp_sal[md&hi]),
            .groups = "drop") |>
  left_join(pc, by = c("time", "geo")) |>
  group_by(geo) |>
  mutate(
    pc = pc/pc[time == "2023-01-01"],
    across(c(w, w_md, w_nmd, w_hifi, w_hi, wb, wb_md, wb_nmd, wb_hifi, wb_hi ),
           ~4*slider::slide_dbl(.x, .before = 3, .f = mean)/pc) ) |>
  ungroup() |>
  mutate(geo = factor(geo, pays))

return(salaires)
