ofce::init_qmd()
library(eurostat)

nace <- source_data("nace.r")$nace

m_a20 <- nace  |> pull(a20) |> unique()
m_a10 <- nace |> pull(a10) |> unique()

adj <- c("SCA", "SA", "CA", "NSA")

pays <- source_data("nace.r")$pays3

label_pays <- rlang::set_names(countrycode::countrycode(pays, "eurostat", "country.name.fr"), pays)

naq10_e <- source_data("naq10_e")$naq_e

emp_dom <- naq10_e |>
  group_by(geo) |>
  drop_na() |>
  summarize(deb=date_trim(min(time)), fin=date_trim(max(time)))

naa_a64 <- "nama_10_a64" |>
  get_eurostat(filters = list(unit = "CP_MEUR",
                              nace_r2  = m_a20,
                              na_item = c("B1G", "D1", "D11", "P51C", "D29X39"),
                              geo = pays) ) |>
  drop_na(values) |>
  pivot_wider(id_cols = c(nace_r2, geo, time),
              names_from =  na_item, values_from = values) |>
  left_join(nace |>
              select(a10, a20),
            by = c("nace_r2"= "a20")) |>
  group_by(a10, geo, time) |>
  summarise(across(c(B1G, D1, D11, P51C, D29X39), sum),
            .groups = "drop") |>
  rename(nace_r2 = a10) |>
  mutate(across(c(D1, D11, P51C, D29X39), ~ .x / B1G)) |>
  rename_with(tolower, c(D1, D11, P51C, D29X39)) |>
  cross_join(tibble(month = seq(1, 10, by=3))) |>
  mutate(time = ym(str_c(year(time), " ", month))) |>
  rename(B1Ga = B1G) |>
  select(-month)

sna_a64_dom <- naa_a64 |>
  group_by(geo) |>
  drop_na() |>
  summarize(deb=year(min(time)), fin=year(max(time)), .groups = "drop")

naq_a10 <- "namq_10_a10" |>
  get_eurostat(filters = list(unit = "CP_MEUR",
                              nace_r2  = m_a10,
                              na_item = c("B1G", "D1", "D11"),
                              geo = pays) ) |>
  select(na_item, geo, time, values, nace_r2, s_adj) |>
  mutate(s_adj = factor(s_adj, adj)) |>
  arrange(s_adj) |>
  group_by(geo, time, nace_r2, na_item) |>
  drop_na(values) |>
  summarize(across(c(values, s_adj), first),
            .groups = "drop") |>
  pivot_wider(id_cols = c(nace_r2, geo, time),
              names_from =  na_item, values_from = c(values, s_adj)) |>
  rename_with(~str_remove(.x, "values_"))

sna_b1g_dom <- naq_a10 |>
  group_by(geo) |>
  drop_na(B1G) |>
  summarize(deb=date_trim(min(time)), fin=date_trim(max(time)), .groups = "drop")

sna_d1_dom <- naq_a10 |>
  group_by(geo) |>
  drop_na(D1) |>
  summarize(deb=date_trim(min(time)), fin=date_trim(max(time)), .groups = "drop")

naq_a10 <- naq_a10 |>
  left_join(naq10_e, by = c("geo", "time", "nace_r2")) |>
  left_join(naa_a64, by = c("geo", "time", "nace_r2")) |>
  group_by(geo, nace_r2) |>
  arrange(time) |>
  mutate(d1 = D1/B1G) |>
  fill(p51c, d29x39, d1) |>
  ungroup() |>
  mutate(
    van = B1G*(1-p51c) - B1G * d29x39,
    vab = B1G,
    msa = d1 * B1G * tsal,
    msanc = d1 * B1G,
    ip = B1G * d29x39) |>
  left_join(nace |>
              distinct(a10, .keep_all = TRUE ) |>
              select(nace_r2 = a10, md=marchand, hi = hors_imm, hifi, hfi),
            by = "nace_r2")

naq_L <- naq_a10 |>
  filter(nace_r2 == "L") |>
  mutate(year = year(time),
         D29X39 = B1G * d29x39) |>
  group_by(geo, year) |>
  summarize(
    nq = n(),
    across(c(B1G, D1, D29X39), ~sum(.x, na.rm=TRUE)*4/nq),
    .groups = "drop") |>
  mutate(time = ym(str_c(year, "-01"))) |>
  select(-year) |>
  rename(B1Gq = B1G,
         D1q = D1,
         D29X39q = D29X39)

L68A <- "nama_10_a64" |>
  get_eurostat(filters = list(unit = "CP_MEUR",
                              nace_r2  = c("L", "L68A"),
                              na_item = c("B1G", "D1", "D11", "P51C", "D29X39"),
                              geo = pays) ) |>
  drop_na(values) |>
  pivot_wider(names_from = c(na_item, nace_r2), values_from = values) |>
  group_by(geo) |>
  mutate(
    rb1g = B1G_L68A/B1G_L,
    p51c = ifelse(is.na(P51C_L68A), P51C_L*rb1g, P51C_L68A),
    d1 = 0,
    d29x39 = ifelse(is.na(D29X39_L68A), D29X39_L*rb1g, D29X39_L68A) ) |>
  transmute(time, geo,
            rb1g,
            B1G = B1G_L - B1G_L68A,
            B1G_L,
            P51C = P51C_L - p51c,
            D1 = D1_L - d1,
            D29X39 = D29X39_L - d29x39)
L68A_dom <- L68A |>
  group_by(geo) |>
  drop_na(B1G) |>
  summarize(deb=year(min(time)), fin=year(max(time)), .groups = "drop")

L68A <- L68A |>
  right_join(naq_L, by = c("time", "geo")) |>
  arrange(geo, time) |>
  group_by(geo) |>
  mutate(rp51c = P51C/B1G,
         rd29 = D29X39/B1G,
         rb1g = B1G/B1Gq) |>
  fill(c(rp51c, rd29, rb1g)) |>
  mutate(
    nace_r2 = 'Lbis',
    B1G = rb1g * B1Gq,
    P51C = rp51c * B1G,
    D29X39 = rd29 * B1G,
    rB1G = B1G/B1Gq,
    rP51C = P51C/B1G,
    rD29X39 = D29X39/B1G )

L68Aq <- L68A |>
  mutate(year = year(time)) |>
  select(geo, year, rP51C, rD29X39, rB1G, nace_r2) |>
  left_join(naq_a10 |> filter(nace_r2 == "L") |> transmute(time, year=year(time), geo, B1G, msa, msanc),
            by = c("geo", "year")) |>
  transmute(
    nace_r2, time, geo,
    md = FALSE, hi = FALSE, hifi = FALSE, hfi = FALSE,
    him = TRUE,
    vab = B1G * rB1G,
    ip = rD29X39 * vab,
    van = vab - rP51C * vab - ip,
    msa, msanc)

naq_a10 <- naq_a10 |>
  mutate(him = hi&md) |>
  bind_rows(L68Aq)

mixte_fr <- source_data("france-melodi.r")$aggr |>
  select(time, champ, b3n) |>
  mutate(geo = "FR") |>
  cross_join(tibble(m = c(1,4,7,10))) |>
  mutate(time = ymd(str_c(year(time),"-", m, "-01")),
         b3n = b3n/4)

mixte_h <- source_data("mixte.r")$h
b3g_dom <- source_data("mixte.r")$dom

naq <- naq_a10 |>
  left_join(mixte_h, by = c("time", "geo", "nace_r2")) |>
  group_by(geo, time) |>
  mutate(
    tb = nace_r2 != "Lbis",
    across(c(b3gh, b3nh), ~ifelse(nace_r2 == "Lbis", .x[nace_r2 == "L"], .x) )) |>
  summarize(
    across(c(van, vab, msa, msanc, ip, b3nh), ~sum(.x[md]), .names = "{.col}_md"),
    across(c(van, vab, msa, msanc, ip, b3nh), ~sum(.x[hi&md]), .names = "{.col}_mdhi"),
    across(c(van, vab, msa, msanc, ip, b3nh), ~sum(.x[hifi&md]), .names = "{.col}_mdhifi"),
    across(c(van, vab, msa, msanc, ip, b3nh), ~sum(.x[hfi&md]), .names = "{.col}_mdhfi"),
    across(c(van, vab, msa, msanc, ip, b3nh), ~sum(.x[him]), .names = "{.col}_mdhim"),
    across(c(van, vab, msa, msanc, ip, b3nh), ~sum(.x[him&hfi]), .names = "{.col}_mdhimfi"),
    across(c(van, vab, msa, msanc, ip, b3nh), ~sum(.x[tb]), .names = "{.col}_tb"),
    .groups = "drop") |>
  pivot_longer(starts_with(c("van", "vab", "msa", "ip", "b3nh"))) |>
  separate(name, into = c("var", "champ"), sep = "_") |>
  pivot_wider(names_from = var, values_from = value) |>
  left_join(mixte_fr, by = c("time", "geo", "champ")) |>
  mutate(
    b3nh_src = b3nh,
    b3nh = ifelse(is.na(b3n), b3nh, b3n),
    rb3nh = b3nh/msanc) |>
  arrange(time) |>
  group_by(champ, geo) |>
  fill(rb3nh, .direction = "down") |>
  mutate(b3nh = rb3nh * msanc) |>
  mutate(
    psal = msa/van,
    psalb = msa/vab,
    psalnc = msanc/van,
    psalbnc = msanc/vab) |>
  filter(geo %in% pays, time >= "1995-01-01") |>
  mutate(geo = factor(geo,  pays))

naa <- naq |>
  mutate(year = year(time)) |>
  group_by(geo, year, champ) |>
  summarize(
    nq = n(),
    across(c(van, vab, msa, msanc, ip, b3nh), ~sum(.x)*4/nq),
    .groups = "drop") |>
  ungroup() |>
  mutate(
    psal = msa/van,
    psalb = msa/vab,
    psalnc = msanc/van,
    psalbnc = msanc/vab,
    psalh = (msanc+b3nh)/van,
    psalbh = (msanc+b3nh)/vab,
    time = ym(str_c(year, "-01"))) |>
  filter(geo %in% pays, time >= "1995-01-01") |>
  mutate(geo = factor(geo,  pays))

# check revenu mixte

mixte <- source_data("mixte.r")$a

cm <- naa |>
  left_join(mixte |> select(-year), by = c("time", "geo")) |>
  mutate(check = (msa - msanc)/b3g) |>
  filter(champ == "tb") |>
  select(geo, year, time, cm = check) |>
  mutate(cm = 0.88/cm) |>
  select(-time)

naa <- naa |>
  left_join(cm, by= c("geo", "year")) |>
  mutate(
    msam = msanc + cm * (msa - msanc),
    msah = msanc + b3nh,
    psalm = msam/van,
    psalh = msah/van)

na_tot <-  "nama_10_gdp" |>
  get_eurostat(
    filters = list(geo = pays,
                   na_item = c("D21", "D31"),
                   unit = "CP_MEUR")) |>
  drop_na() |>
  pivot_wider(names_from = na_item, values_from = values) |>
  transmute(geo, time, d2131 = D21 - D31)

max_y <- max(year(na_tot$time))

nq_tot <- get_eurostat("namq_10_gdp",
                       filters = list(geo = pays,
                                      na_item = c("D21X31"),
                                      unit = "CP_MEUR")) |>
  drop_na() |>
  mutate(s_adj = factor(s_adj, c("SCA", "SA", "CA", "NSA"))) |>
  arrange(s_adj) |>
  group_by(geo, time, na_item) |>
  summarize(values = first(values),
            s_adj = first(s_adj),
            .groups = "drop") |>
  filter(year(time) > max_y ) |>
  group_by(geo, na_item) |>
  summarize(values = mean(values, na.rm=TRUE) * 4,
            .groups = "drop") |>
  pivot_wider(names_from = na_item, values_from = values) |>
  transmute(geo, time = ym(str_c(max_y+1, "-01")), d2131 = D21X31) |>
  bind_rows(na_tot) |>
  arrange(geo, time)

d51 <- "nasa_10_nf_tr" |>
  get_eurostat(
    filters = list(na_item = c("B1G", "D51"), direct = "PAID",
                   geo = pays, unit = "CP_MEUR",
                   sector = c("S11", "S12"))) |>
  drop_na() |>
  pivot_wider(names_from = sector, values_from = values) |>
  mutate(tb = S11 + S12,
         md = S11 + S12,
         mdhi = S11+S12,
         mdhim = S11+S12,
         mdhimfi = S11,
         mdhifi = S11,
         mdhfi = S11) |>
  select(-S11, -S12) |>
  pivot_longer(
    c(tb, md, mdhi, mdhim, mdhimfi, mdhifi, mdhfi),
    names_to = "champ", values_to = "value") |>
  pivot_wider(names_from = na_item, values_from = value) |>
  select(geo, time, is=D51, B1Ga = B1G, champ) |>
  right_join(
    naa |> select(time, geo, B1G = vab, van, msa, ip, champ),
    by = c("time", "geo", "champ")) |>
  mutate(t2is = is/B1G,
         tis = is/(van-msa)) |>
  group_by(geo, champ) |>
  fill(tis, t2is) |>
  ungroup() |>
  transmute(
    geo, time, tis, t2is, champ,
    is = tis*(van-msa),
    is2 = t2is*B1G)

naa_ext <- naa |>
  select(geo, time, van, vab, msa, msam, msanc, msah, ip, champ) |>
  arrange(time, geo, champ) |>
  left_join(d51, by  = c("time", "geo", "champ")) |>
  mutate(psal = msa/van,
         psalh = msah/van,
         psalnc = msanc/van,
         psalm = msam/van,
         psalb = msa/vab,
         psalhb = msah/vab,
         psalncb = msanc/vab) |>
  filter(geo %in% pays, time >= "1995-01-01") |>
  mutate(geo = factor(geo, pays)) |>
  mutate(
    tp = (van - msa - is)/van,
    tpb = (van - msa )/van,
    tpm = (van - msam - is)/van,
    tpbm = (van - msam )/van,
    tph = (van - msah - is)/van,
    tpbh = (van - msah )/van  )

assets <- source_data("assets.r")$assets  |>
  filter(assets>0)

naa_ext2 <- naa_ext |>
  filter(year(time)<= max_y) |>
  left_join( assets, by =c("geo", "time", "champ") ) |>
  mutate(
    r = tp*van/assets,
    rb = tpb*van/assets,
    rbm = tpbm*van/assets,
    rm = tpm*van/assets,
    rbh = tpbh*van/assets,
    rh = tph*van/assets) |>
  arrange( desc(time), geo ) |>
  mutate(geo = factor(geo, pays))

return(list(naa = naa_ext, naaa = naa_ext2, naq_a10 = naq_a10,
            dom = list(emp = emp_dom, snq_b1g = sna_b1g_dom,
                       snq_d1 = sna_d1_dom, sna = sna_a64_dom, l68a = L68A_dom, b3g = b3g_dom)))
