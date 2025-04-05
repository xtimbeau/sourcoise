library(insee)
library(rsdmx)
# library(rwebstat)
library(httr)
library(jsonlite)
library(curl)
library(readxl)

get_series <- function(series_key, api_key, proxy_host = NULL, proxy_port = NULL) {
  url <- "https://webstat.banque-france.fr/api/explore/v2.1/catalog/datasets/observations/exports/json"
  params <- list(
    select = "time_period_end,obs_value",
    where = paste0("series_key='", series_key, "'")
  )
  if (!is.null(proxy_host) & !is.null(proxy_port)) {
    proxy <- use_proxy(proxy_host, proxy_port)
    response <- GET(url, query = params, add_headers(Authorization = paste("Apikey", api_key)), proxy)
  } else {
    response <- GET(url, query = params, add_headers(Authorization = paste("Apikey", api_key)))
  }
  if (status_code(response) == 200) {
    data <- content(response, as = "text", encoding = "UTF-8")
    df <- fromJSON(data, flatten = TRUE)
    df$time_period_end <- as.Date(df$time_period_end)
    return(df)
  } else {
    print(paste("Erreur lors de la requête :", status_code(response)))
    return(NULL)
  }
}

#IPC
IPC <- get_insee_idbank("001769682") |>
  mutate(IPC = as.numeric(OBS_VALUE), DATE = as_date(DATE))  |>
  filter(year(DATE) > 1995 & (month(DATE) %in% c(1, 4, 7, 10))) |>
  dplyr::select(DATE, IPC) |>
  arrange(DATE)

#Prix immobilier dans l'ancien
PxImmo <- get_insee_idbank("010567059") |>
  arrange(DATE) |>
  mutate(PxImmo = as.numeric(OBS_VALUE), DATE = as_date(DATE))  |>
  filter(year(DATE) > 1995)   |>
  left_join(IPC, by = "DATE") |>
  mutate(dIPC = IPC/ lag(IPC)) |>
  mutate(diff = (PxImmo / lag(PxImmo) - dIPC),
         PxImmo_r = cumprod(1 + c(first(PxImmo), diff[-1]))) |>
  select(DATE, PxImmo, diff, PxImmo_r)

#Taux des nouveaux crédit à l'habitat
TX_CRED_HAB <-get_series("MIR1.M.FR.B.A22.A.R.A.2250U6.EUR.N","5e76da132c6be7c6fff9c5ca3796c87077d32e78b3378ee753c1da1b") |>
  mutate(DATE=as_date(time_period_end), TX_CRED_HAB=obs_value) |>
  dplyr::select(DATE, TX_CRED_HAB) |>
  filter(month(DATE) %in% c(1,4,7,10)) |>
  arrange(DATE)

#Flux des nouveaux crédit à l'habitat
CRED_HAB <- get_series("MIR1.M.FR.B.A22.A.5.A.2254U6.EUR.N","5e76da132c6be7c6fff9c5ca3796c87077d32e78b3378ee753c1da1b") |>
  mutate(DATE=as_date(time_period_end), CRED_HAB=obs_value) |>
  dplyr::select(DATE, CRED_HAB) |>
  filter(month(DATE) %in% c(1,4,7,10)) |>
  arrange(DATE)

#Enquêtes
ENQ_DEM <- get_insee_idbank("001634699") |>
  arrange(DATE) |>
  mutate(ENQ_DEM = as.numeric(OBS_VALUE)+100, DATE = as_date(DATE))  |>
  filter(year(DATE) > 1995) |>
  dplyr::select(DATE, ENQ_DEM)

ENQ_PERSP <- get_insee_idbank("001634709") |>
  arrange(DATE) |>
  mutate(ENQ_PERSP = as.numeric(OBS_VALUE)+100, DATE = as_date(DATE))  |>
  filter(year(DATE) > 1995) |>
  dplyr::select(DATE, ENQ_PERSP)

#Mises en chantier
LOG_PERM1 <- get_insee_idbank("001718274") |>
  arrange(DATE) |>
  mutate(LOG_PERM1 = as.numeric(OBS_VALUE), DATE = as_date(DATE))  |>
  filter(year(DATE) > 1995 & month(DATE) %in% c(1,4,7,10))   |>
  dplyr::select(DATE, LOG_PERM1)

#Permis
LOG_PERM <- get_insee_idbank("001718162") |>
  arrange(DATE) |>
  mutate(LOG_PERM = as.numeric(OBS_VALUE), DATE = as_date(DATE))  |>
  filter(year(DATE) > 1995 & month(DATE) %in% c(1,4,7,10))   |>
  dplyr::select(DATE, LOG_PERM)

TX_CRED_HAB_r <- IPC |>
  left_join(TX_CRED_HAB, by="DATE") |>
  mutate(TX_CRED_HAB_r=TX_CRED_HAB-((IPC/lag(IPC,4)-1)*100))

# Indices de chiffres d'affaires
Constr <- get_insee_idbank("010540589") |>
  mutate(constr = as.numeric(OBS_VALUE), DATE = as_date(DATE))  |>
  filter(year(DATE) > 1998)   |>
  dplyr::select(DATE, constr) |>
  arrange(DATE)


TP <- get_insee_idbank("010540605") |>
  mutate(tp = as.numeric(OBS_VALUE), DATE = as_date(DATE))  |>
  filter(year(DATE) > 1998)   |>
  dplyr::select(DATE, tp) |>
  arrange(DATE)

Travaux <- get_insee_idbank("010540635") |>
  mutate(travaux = as.numeric(OBS_VALUE), DATE = as_date(DATE))  |>
  filter(year(DATE) > 1998)   |>
  dplyr::select(DATE, travaux) |>
  arrange(DATE)

nouv_proj <- get_insee_idbank("001587037") |>
  mutate(nv_proj = as.numeric(OBS_VALUE), DATE = as_date(DATE))  |>
  filter(year(DATE) > 1998)   |>
  dplyr::select(DATE, nv_proj) |>
  arrange(DATE)

mean_com <- mean(LOG_PERM1$LOG_PERM1)
mean_perm <- mean(LOG_PERM$LOG_PERM)

# transac <- readxl::read_excel("/Users/pierremadec/Downloads/transac.xlsx") |>
#   separate(DATE, into = c("annee", "trimestre"), sep = "-T", convert = TRUE) %>%
#   mutate(trimestre = as.numeric(trimestre)) |>
#   mutate(date = as.Date(paste(annee, trimestre * 3, "01", sep = "-")))

curl_download(
  url = "https://www.cgedd.fr/nombre-vente-maison-appartement-ancien.xls",
  destfile = "/tmp/nombre-vente-maison-appartement-ancien.xls")

transactions <- read_xls("/tmp/nombre-vente-maison-appartement-ancien.xls", sheet = 2, skip = 23) %>%
  select(1, 3) %>%
  setNames(c("date", "nombre_transactions")) %>%
  mutate(date = as.Date(date),
         nombre_transactions = as.numeric(nombre_transactions)) |>
  mutate(date = floor_date(date, "quarter")) |>
  group_by(date) |>
  summarize(t = mean(nombre_transactions, na.rm = TRUE)) |>
  drop_na() |>
  mutate(
    tooltip = str_c("<b>", year(date), " T", quarter(date), "</b><br>", round(t), "k transactions<br>",
                    "Ecart au max :", round(100*t/max(t, na.rm=TRUE)-100), "%<br>",
                    "Ecart au dernier :", round(-100*last(t)/t+100), "%")) |>
  filter(date>="1996-01-01")

# #Taux des nouveaux crédit à l'habitat
# TX_CRED_HAB <- read_delim("/Users/pierremadec/Downloads/WEBSTAT-observations-2024-09-12T09_49_17.897+02_00.csv", delim =  ";") |>
#   mutate(DATE=as_date(time_period_start), TX_CRED_HAB=obs_value) |>
#   dplyr::select(DATE, TX_CRED_HAB) |>
#   arrange(DATE)
#
# #Taux des nouveaux crédit à l'habitat
# CRED_HAB <- read_delim("/Users/pierremadec/Downloads/WEBSTAT-observations-2024-09-12T17_06_44.720+02_00.csv", delim =  ";") |>
#   mutate(DATE=as_date(time_period_start), CRED_HAB=obs_value) |>
#   dplyr::select(DATE, CRED_HAB)  |>
#   arrange(DATE)

ret <- list(LOG_PERM = LOG_PERM, LOG_PERM1 = LOG_PERM1, TX_CRED_HAB = TX_CRED_HAB,
            ENQ_DEM = ENQ_DEM, ENQ_PERSP = ENQ_PERSP, CRED_HAB = CRED_HAB)
return(ret)
