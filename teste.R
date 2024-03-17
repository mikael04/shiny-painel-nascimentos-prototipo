library(bigrquery)
library(dplyr)
library(dbplyr)
library(DBI)

projectid = "pdi-covid-basededados"

# Set your query
sql <- "SELECT * FROM `pdi-covid-basededados.sinasc.view_sinasc_a_partir_2020` LIMIT 10"

# Run the query; this returns a bq_table object that you can query further
tb <- bq_project_query(projectid, sql)

# Store the first 10 rows of the data in a tibble
sample <-bq_table_download(tb, n_max = 10)


df_sinasc <- bq_table_download(tb)

# Print the 10 rows of data
sample

projectid = "pdi-covid-basededados"

proj_name <- "`pdi-covid-basededados.sinasc.view_sinasc_a_partir_2020`"

# Set your query
sql <- paste0("SELECT DISTINCT __PDI_UF FROM ", proj_name)

con <- dbConnect(
  bigrquery::bigquery(),
  project = "pdi-covid-basededados",
  dataset = "sinasc"
)

view_sinasc_2020_2022 <- tbl(con, "view_sinasc_a_partir_2020")

ufs <- view_sinasc_2020_2022 |>
  dplyr::select(uf = `__PDI_UF`) |>
  dplyr::distinct(uf) |>
  dplyr::arrange(uf) |>
  dplyr::pull()

glimpse(view_sinasc_2020_2022)

# Run the query; this returns a bq_table object that you can query further
tb <- bq_project_query(projectid, sql)

ufs <- bq_table_download(tb) |>
  dplyr::pull()

ufs_f <- ufs |>
  dplyr::pull()

projectid = "pdi-covid-basededados"

proj_name <- "`pdi-covid-basededados.sinasc.view_sinasc_a_partir_2020` "

# Set your query
sql <- paste0("SELECT DISTINCT __PDI_DTNASC_ANO_MES FROM ", proj_name)

# Run the query; this returns a bq_table object that you can query further
tb <- bq_project_query(projectid, sql)

dtnasc <- bq_table_download(tb)

dtnasc <- dtnasc |>
  dplyr::mutate(year = substr(`__PDI_DTNASC_ANO_MES`, 1, 4),
                month = substr(`__PDI_DTNASC_ANO_MES`, 5, 6),
                date = lubridate::ym(`__PDI_DTNASC_ANO_MES`))

years_f <- dplyr::distinct(dtnasc, year) |> dplyr::arrange(year) |> dplyr::pull()
months_f <- dplyr::distinct(dtnasc, month) |> dplyr::arrange(month)  |> dplyr::pull()
date = dplyr::distinct(dtnasc, date) |> dplyr::arrange(date)  |> dplyr::pull()


cod_ibge_mun <- data.table::fread("data-raw/cod_ibge_mun.csv") |>
  dplyr::select(cod_ibge, nome_mun, nome_uf)

uf_cod_nome <- data.table::fread("data-raw/uf_cod_nome.csv")

cod_ibge_mun_ufs <- dplyr::inner_join(cod_ibge_mun, uf_cod_nome, by="nome_uf")


data.table::fwrite(cod_ibge_mun_ufs, "data-raw/cod_ibge_mun_ufs.csv")

## Idades das mães ----
# Set your query
sql <- paste0("SELECT DISTINCT IDADEMAE FROM ", proj_name)

# Run the query; this returns a bq_table object that you can query further
tb <- bq_project_query(projectid, sql)

idademae <- bq_table_download(tb)


## Escolaridade das mães ----
# Set your query
sql <- paste0("SELECT DISTINCT ESCMAE2010 FROM ", proj_name)

# Run the query; this returns a bq_table object that you can query further
tb <- bq_project_query(projectid, sql)

escmae2010 <- bq_table_download(tb)

## Mapa
con <- dbConnect(
  bigrquery::bigquery(),
  project = "pdi-covid-basededados",
  dataset = "sinasc"
)


df_sinasc_2020_2022 <- tbl(con, "view_sinasc_a_partir_2020")

df_sinasc_2020_2022_ufs_nasc <- df_sinasc_2020_2022 |>
  dplyr::group_by(`__PDI_UF`) |>
  dplyr::summarise(count = n()) |>
  dplyr::ungroup() |>
  dplyr::collect()


# df_ibge_mun <- data.table::fread("data-raw/ibge-dados-municipais.csv") |>
#   janitor::clean_names() |>
#   dplyr::select(uf, nome_uf, cod_mun = codigo_municipio_completo,
#                 nome_mun = nome_municipio) |>
#   dplyr::mutate(cod_mun_ = substr(as.character(cod_mun), 1, 6))
#   ## Escrevendo dados
#   data.table::fwrite("data-raw/ibge-dados-municipais-tratados.csv")

## Lembrando que precisa adicionar a sigla UF à alguns municípios
# df_mun <- data.table::fread("data-raw/cod_ibge_mun_ufs.csv")
#
# df_ibge_mun <- data.table::fread("data-raw/ibge-dados-municipais.csv") |>
#   janitor::clean_names() |>
#   dplyr::select(cod_uf = uf, nome_uf, cod_mun = codigo_municipio_completo,
#                 nome_mun = nome_municipio) |>
#   dplyr::mutate(cod_mun = as.integer(substr(as.character(cod_mun), 1, 6))) |>
#   dplyr::left_join(df_mun |> dplyr::select(cod_ibge, sigla_uf = uf),  by=c("cod_mun" = "cod_ibge"))
# ## Escrevendo dados
# data.table::fwrite(df_ibge_mun, "data-raw/ibge-dados-municipais-tratados.csv")
#
# df_ibge_uf <- data.table::fread("data-raw/ibge-dados-municipais-tratados.csv") |>
#   dplyr::group_by(cod_uf) |>
#   dplyr::distinct(cod_uf, .keep_all = T) |>
#   dplyr::select(cod_uf, sigla_uf, nome_uf) |>
#   dplyr::ungroup()
#
# ## Escrevendo dados
# data.table::fwrite(df_ibge_uf, "data-raw/ibge-dados-ufs-tratados.csv")











































































