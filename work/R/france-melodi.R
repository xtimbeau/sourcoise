library(tidyverse)
library(ofce)
library(melodi)

br <- tibble::tribble(
  ~nace_r2, ~md,    ~mdhi,  ~mdhfi,  ~nace_13,
  "A",      TRUE,   TRUE,   TRUE,  "A",
  "B_D_E",  TRUE,   TRUE,   TRUE,  "BTE",
  "C10T12", TRUE,   TRUE,   TRUE,  "BTE",
  "C13T15", TRUE,   TRUE,   TRUE,  "BTE",
  "C16T18", TRUE,   TRUE,   TRUE,  "BTE",
  "C19",    TRUE,   TRUE,   TRUE,  "BTE",
  "C20",    TRUE,   TRUE,   TRUE,  "BTE",
  "C21",    TRUE,   TRUE,   TRUE,  "BTE",
  "C22_23", TRUE,   TRUE,   TRUE,  "BTE",
  "C24_25", TRUE,   TRUE,   TRUE,  "BTE",
  "C26T28", TRUE,   TRUE,   TRUE,  "BTE",
  "C29_30", TRUE,   TRUE,   TRUE,  "BTE",
  "C31T33", TRUE,   TRUE,   TRUE,  "BTE",
  "F",      TRUE,   TRUE,   TRUE,  "F",
  "G",      TRUE,   TRUE,   TRUE,  "G",
  "H",      TRUE,   TRUE,   TRUE,  "H",
  "I",      TRUE,   TRUE,   TRUE,  "I",
  "J",      TRUE,   TRUE,   TRUE,  "J",
  "K",      TRUE,   TRUE,   FALSE,  "K",
  "L",      TRUE,   FALSE,  TRUE,  "L",
  "M_N",    TRUE,   TRUE,   TRUE,  "M_N",
  "OTQ",    FALSE,  FALSE,  FALSE,  "OTQ",
  "R",      TRUE,   TRUE,   TRUE,  "R",
  "S",      TRUE,   TRUE,   TRUE,  "S" )

br_alt <- c("_T", "OTQ", "K", "L")
br_plus <- unique(c(br$nace_13, "_T"))
codes <- vroom::vroom(pathify("/data/ACTIVITY.codes.csv"))
colnames(codes) <- c("nace_r2", "fr", "en")
codes <- codes |>
  bind_rows(tibble(code = "C", fr="Industrie manufacturière", en="Manufacturing"))

info_melodi <- function(data) {
  map(names(data |> select(-OBS_VALUE, -TIME_PERIOD)), ~count(data, across(all_of(.x))))
}

nace <- source_data("nace.r")$nace
a20 <- nace$a20
pays <- source_data("nace.r")$pays1

vahim <- source_data("vaq.r")$naa |>
  select(geo, time, van, champ) |>
  filter(champ %in% c("mdhim", "mdhi"), geo %in% pays) |>
  pivot_wider(names_from = champ, values_from = van) |>
  mutate(r = (mdhim-mdhi)/mdhi)

men <- "DD_CNA_CONSO_MENAGES_PRODUITS" |>
  melodi::get_all_data() |>
  filter(
    PRODUCT %in% c("HI_L68I0A_B2020", "HI_L68I0B_B2020", "HI_L68R1A_B2020", "HI_L68R1B_B2020"),
    UNIT_MEASURE == "XDC",
    TRANSFORMATION == "N",
    STO == "P31",
    REF_SECTOR == "S14",
    PRICES=="V") |>
  transmute(
    time = ymd(TIME_PERIOD, truncated=2),
    p31 = OBS_VALUE,
    PRODUCT = PRODUCT |> str_remove("HI_") |> str_remove("_B2020") ) |>
  pivot_wider(names_from = PRODUCT, values_from = p31) |>
  transmute(
    time, nace_r2 = "L",
    lymen = L68R1A+L68I0A+L68I0B+L68R1B,
    lymenimp = L68I0A+L68I0B,
    lymenree = L68R1A+L68R1B) |>
  arrange(desc(time))

men2 <- "DD_CNA_SUT" |>
  melodi::get_all_data() |>
  filter(STO=="P3",
         PRODUCT == "CPA_L",
         REF_SECTOR == "S1") |>
  arrange(TIME_PERIOD) |>
  transmute(
    time = ymd(TIME_PERIOD, truncated=2),
    p3l = OBS_VALUE) |>
  arrange(desc(time))

fr_tes_L <- "DD_CNA_SUT" |>
  melodi::get_all_data() |>
  filter(
    STO == "P2", PRICES=="V", UNIT_MEASURE=="XDC", PRODUCT=="CPA_L",
    ACTIVITY %in% br_alt) |>
  transmute(
    time = ymd(TIME_PERIOD, truncated=2),
    nace_r2 = ACTIVITY,
    p2l  = OBS_VALUE ) |>
  arrange(time)

fr_emp <- melodi::get_all_data("DD_CNA_BRANCHES") |>
  filter(REF_SECTOR == "S1",
         STO%in%c("SAL", "SELF", "EMP"),
         UNIT_MEASURE == "PS",
         TRANSFORMATION == "N",
         COUNTERPART_AREA == "W2",
         ACTIVITY %in% br_plus) |>
  transmute(time = ymd(TIME_PERIOD, truncated=2),
            value = OBS_VALUE,
            STO,
            nace_r2 = ACTIVITY) |>
  # filter(time>="1978-01-01") |>
  pivot_wider(names_from = STO, values_from = value) |>
  mutate(
    across(c(EMP, SELF, SAL), ~replace_na(.x, 0)) ) |>
  arrange(time, nace_r2)

fr_b23g <- melodi::get_all_data("DD_CNA_BRANCHES") |>
  filter(REF_SECTOR == "S1",
         STO%in%c("B2A3G", "B2G"),
         PRICES == "V",
         UNIT_MEASURE == "XDC",
         TRANSFORMATION == "N",
         COUNTERPART_AREA == "W0",
         ACCOUNTING_ENTRY == "B",
         ACTIVITY %in% br_plus) |>
  transmute(time = ymd(TIME_PERIOD, truncated=2),
            value = OBS_VALUE,
            STO,
            nace_r2 = ACTIVITY) |>
  pivot_wider(names_from = STO, values_from = value) |>
  mutate(b3g = B2A3G - B2G) |>
  arrange(time, nace_r2)

fr_d1 <- melodi::get_all_data("DD_CNA_BRANCHES") |>
  filter(REF_SECTOR == "S1",
         STO%in%c("D1"),
         PRICES == "V",
         UNIT_MEASURE == "XDC",
         TRANSFORMATION == "N",
         COUNTERPART_AREA == "W0",
         ACCOUNTING_ENTRY == "D",
         ACTIVITY %in% br_plus) |>
  transmute(time = ymd(TIME_PERIOD, truncated=2),
            value = OBS_VALUE,
            STO,
            nace_r2 = ACTIVITY) |>
  pivot_wider(names_from = STO, values_from = value) |>
  arrange(time, nace_r2)

fr_ind <- fr_emp |>
  full_join(fr_b23g, by = c("time", "nace_r2")) |>
  full_join(fr_d1, by = c("time", "nace_r2")) |>
  group_by(time) |>
  mutate(
    rb3g = b3g/b3g[nace_r2=="_T"],
    rpc = (b3g/SELF)/(D1[nace_r2=="_T"]/SAL[nace_r2=="_T"]),
    rpc = ifelse(rpc<0, NA, rpc)) |>
  group_by(nace_r2) |>
  fill(rpc, .direction="downup") |>
  group_by(time) |>
  mutate(b3g = rpc * D1[nace_r2=="_T"]/SAL[nace_r2=="_T"] * SELF,
         vv = sum(b3g[nace_r2!="_T"])/b3g[nace_r2 == "_T"]) |>
  mutate(b3g = ifelse(nace_r2=="_T", b3g, b3g/vv)) |>
  rename_with(tolower) |>
  ungroup()

fr_branches <- melodi::get_all_data("DD_CNA_BRANCHES") |>
  filter(REF_SECTOR == "S1",
         STO%in%c("B1G", "P51C", "D1", "D29X39", "B2A3G", "B2G"),
         PRICES == "V",
         UNIT_MEASURE == "XDC",
         TRANSFORMATION == "N",
         ACTIVITY %in% br_alt) |>
  transmute(time = ymd(TIME_PERIOD, truncated=2),
            value = OBS_VALUE,
            nace_r2 = ACTIVITY,
            na_item = STO,
            ca = COUNTERPART_AREA,
            ae = ACCOUNTING_ENTRY) |>
  arrange(time, na_item, nace_r2) |>
  pivot_wider(names_from = c(ae, ca) , values_from = value) |>
  rowwise() |>
  mutate(value = coalesce(B_W0, B_W2, D_W0, D_W2)) |>
  select(time, nace_r2, na_item, value) |>
  pivot_wider(names_from = na_item, values_from = value) |>
  rename_with(tolower) |>
  mutate(b3g = b2a3g - b2g) |>
  left_join(fr_tes_L, by = c("nace_r2", "time"))

# On prolonge pour 2024  b2g sur la valeur "total" (_T) et d29x30 sur le ratio b1g
# Sur le passé on prolonge p51c et d29x39 sur b1g et b2g sur NSAL/SAL

fr_branches_aug <- fr_branches |>
  arrange(time) |>
  select(-b3g) |>
  left_join(fr_ind |> select(time, nace_r2, b3g, sal, self), by = c("nace_r2", "time")) |>
  mutate(rp51c = p51c / b1g,
         rd29x39 = d29x39 / b1g) |>
  group_by(nace_r2) |>
  fill(rp51c, rd29x39, .direction = "updown") |>
  mutate(p51c = rp51c * b1g,
         d29x39 = rd29x39 * b1g) |>
  rename_with(tolower) |>
  ungroup()

assets <- "DD_CNA_PATRIMOINE_BRANCHES" |>
  melodi::get_all_data() |>
  filter(ACTIVITY %in% br_alt,
         STO == "LE", PRICES=="U", COUNTERPART_AREA == "W0",
         INSTR_ASSET %in% c("N1N", "N111N")) |>
  transmute(
    time = ymd(TIME_PERIOD, truncated=2),
    value = OBS_VALUE,
    nace_r2 = ACTIVITY,
    asset = INSTR_ASSET) |>
  arrange(desc(time), nace_r2, asset) |>
  pivot_wider(names_from = asset, values_from = value) |>
  mutate(across(c(N1N, N111N), ~replace_na(.x, 0)))

melodi <- fr_branches_aug |>
  left_join(assets, by = c("time", "nace_r2")) |>
  left_join(men, by = c("time", "nace_r2")) |>
  left_join(br, by = "nace_r2") |>
  rename_with(tolower) |>
  group_by(nace_r2) |>
  mutate(
    vab = b1g,
    van = b1g - p51c - d29x39,
    psal = (d1 * (1 + self/sal)) / van,
    psalm = (d1 + b3g*0.88) / van,
    tp = (van - d1 * (1 + self/sal) - d29x39) / van,
    tpm = (van - d1 - b3g * 0.88 - d29x39) / van,
    rb = (van - d1 * (1 + self/sal) - d29x39) / n1n,
    rpm = (van - d1 - b3g * 0.88  - d29x39) / n1n) |>
  ungroup()

# dvalL <- melodi |>
#   group_by(time) |>
#   summarize(dva = -sum(p2l)+sum(p2lf)) |>
#   mutate(nace_r2 = "L")
#
# dval <- melodi |>
#   filter(nace_r2 != "L") |>
#   mutate(dva = p2l - p2lf) |>
#   select(time, nace_r2, dva) |>
#   bind_rows(dvalL)

# melodi <- melodi |>
#   left_join(dval, by=c("time", "nace_r2")) |>
#   mutate(
#     rb2 = (van + dva - d1 * (1 + self/sal) - d29x39) / n1n)

melodi2 <- melodi |>
  group_by(time, nace_13) |>
  summarize(
    across(c(b1g, b3g, d1, d29x39, p51c, p2l, self, sal, n111n, n1n, lymen), sum),
    across(c(md, mdhi), first),
    .groups = "drop") |>
  mutate(
    vab = b1g,
    van = b1g - p51c - d29x39,
    psal = (d1 * (1 + self/sal)) / van,
    psalm = (d1 + b3g*0.8) / van,
    tp = (van - d1 * (1 + self/sal) ) / van,
    tpm = (van - d1 - b3g * 0.8) / van,
    rb = (van - d1 * (1 + self/sal)) / n1n,
    rpm = (van - d1 - b3g * 0.8  ) / n1n)

ssi <- melodi2 |> filter(time == "2024-01-01",md) |> select(time, van, nace_13, md) |> mutate(van=van/sum(van))

ccf_ei <- melodi::get_all_data("DD_CNA_AGREGATS") |>
  filter(REF_SECTOR == "S14AA", STO %in% c("B3G", "P51C", "B3N"), PRICES=="V") |>
  select(time = TIME_PERIOD, OBS_VALUE, STO) |>
  pivot_wider(names_from = STO, values_from = OBS_VALUE) |>
  mutate(time = ymd(time, truncated = 2)) |>
  arrange(time) |>
  rename_with(tolower) |>
  drop_na() |>
  mutate(ccfei = p51c/b3g)

melodi_m <- melodi |>
  group_by(time) |>
  summarize(
    across(c(b1g, b3g, d1, d29x39, p51c, p2l, self, sal, n111n, n1n, lymen, van, vab),
           ~.x[nace_r2=="_T"]-.x[nace_r2=="OTQ"], .names = "{.col}_md" ),
    across(c(b1g, b3g, d1, d29x39, p51c, p2l, self, sal, n111n, n1n, lymen, van, vab),
           ~.x[nace_r2=="_T"]-.x[nace_r2=="OTQ"]-.x[nace_r2=="L"], .names = "{.col}_mdhi" ),
    across(c(b1g, b3g, d1, d29x39, p51c, p2l, self, sal, n111n, n1n, lymen, van, vab),
           ~.x[nace_r2=="_T"]-.x[nace_r2=="OTQ"]-.x[nace_r2=="L"]-.x[nace_r2=="K"], .names = "{.col}_mdhifi" ),
    across(c(b1g, b3g, d1, d29x39, p51c, p2l, self, sal, n111n, n1n, lymen, van, vab),
           ~.x[nace_r2=="_T"]-.x[nace_r2=="OTQ"]-.x[nace_r2=="K"], .names = "{.col}_mdhfi" ),
    across(c(b1g, b3g, d1, d29x39, p51c, p2l, self, sal, n111n, n1n, lymen, van, vab),
           ~.x[nace_r2=="_T"], .names = "{.col}_tb" ) ) |>
  pivot_longer(cols = ends_with(c("md", "mdhi", "mdhifi", "mdhfi", "tb"))) |>
  separate(name, sep="_", into = c("var", "champ")) |>
  pivot_wider(names_from = var, values_from = value) |>
  left_join(ccf_ei |> select(time, ccfei), by = c("time")) |>
  group_by(champ) |>
  fill(ccfei, .direction = "up") |>
  group_by(champ) |>
  mutate(
    b3n = (1-ccfei)*b3g,
    msanc = d1,
    msa = d1 * (1 + self/sal),
    msam = d1 + b3n,
    psal = msa / van,
    tp = (van - msa ) / van,
    rb = (van - msa ) / n1n,
    psalm = msam / van,
    tp = (van - msa ) / van,
    rbm = (van - msam ) / n1n,
    dp2l = p2l - p2l[time=="1980-01-01"]/van[time=="1980-01-01"] * van,
    rb2 = (van + dp2l - msa ) / n1n,
    rbm2 = (van + dp2l - msam ) / n1n)

return(list(full = melodi, a20 = melodi2, aggr = melodi_m))
