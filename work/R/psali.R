ofce::init_qmd()

snfia <- source_data("snf_international.r") |>
  pluck("snfia") |>
  mutate(tooltip = glue(
    "<b>{label_geo}</b>
    {year(time)}
    Impôts net de subvention dans la VAN : {round(100*isp,1)}%
    Rémunérations dans la VAN : {round(psal*100,1)}%
    Profits nets (d'IS, taxes et subvention) dans la VAN : {round(tp*100,1)}%
    Dividendes nets dans la VAN : {round(div*100,1)}%")) |>
  mutate(geo = factor(geo, c("EA20", "US", "UK", "JP", "CA", "CH"))) |>
  filter(year(time)>=1995)

meta <- sourcoise::sourcoise_meta("snf_ratio.r")
date <- meta$data_date |> ymd_hms()
date <- stringr::str_glue("{day(date)}/{month(date)}/{year(date)}")
lbl_eurostat <- function(x) eurostat::label_eurostat(x, "geo",
                                                     lang = "fr",
                                                     custom_dic=c("EA20"= "Zone euro"))
psali <- ggplot(snfia) +
  aes(x=time, y=psal, col = geo) +
  geom_line_interactive(
    aes(tooltip = "Part des salaires dans la VAN"), show.legend=FALSE) +
  geom_point_interactive(
    aes(tooltip = tooltip, data_id = time, fill = geo),
    size = 1, stroke = 0.5, shape = 21, color = "white",
    hover_nearest = TRUE, show.legend=FALSE) +
  ylab("% de la VA") + xlab(NULL) +
  facet_wrap(vars(geo), ncol = 3, labeller = lbl_eurostat) +
  scale_ofce_date(
    labels = date1
  ) +
  scale_y_continuous(labels = scales::label_percent(1), limits = c(NA, 1)) +
  scale_color_pays(format = "eurostat") +
  ofce_caption(
    source = "Eurostat, comptes nationaux annuels (naidsa_10_nf_tr), téléchargés le {date},
 code à github.com/xtimbeau/travail/snf_international.r.",
    champ = "Sociétés non financières (S11, la définition peut varier d'un pays à l'autre)")

return(psali)
