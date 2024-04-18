library(bigrquery)
library(dplyr)
library(dbplyr)
library(DBI)

# Query da primeira versão ----
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

# Nova base ----

projectid = "pdi-covid-basededados"

proj_name <- "`pdi-covid-basededados.paineis.view_sinasc_tratamento_painel`"

# Consultas iniciais ----
## UFS ----

# Set your query
sql <- paste0("SELECT DISTINCT _UF FROM ", proj_name)

# Run the query; this returns a bq_table object that you can query further
tb <- bq_project_query(projectid, sql)

ufs_f <- bq_table_download(tb) |>
  dplyr::arrange(`_UF`) |>
  dplyr::pull()

## Datas ----

# Set your query
sql <- paste0("SELECT DISTINCT _ANONASC FROM ", proj_name)

# Run the query; this returns a bq_table object that you can query further
tb <- bq_project_query(projectid, sql)

anos_nasc <- bq_table_download(tb)

# Bigrquery ----
con <- dbConnect(
  bigrquery::bigquery(),
  project = "pdi-covid-basededados",
  dataset = "paineis"
)

df_sinasc <- tbl(con, "view_sinasc_tratamento_painel")

colnames <- colnames(df_sinasc)
df_colnames <- data.frame(colnames) |>
  dplyr::arrange(colnames)

## Contagem de nascimentos por estados e por data (ano_mes) -----
df_sinasc_ufs_nasc <- df_sinasc |>
  dplyr::mutate(uf = `_UF`, ano_nasc = `_ANONASC`) |>
  dplyr::group_by(uf, ano_nasc) |>
  dplyr::summarise(count = n()) |>
  dplyr::select(cod_uf = uf, ano_nasc, count) |>
  dplyr::ungroup() |>
  dplyr::collect()


df_sinasc_2020_2022_ufs_nasc <- df_sinasc |>
  dplyr::slice_sample(n = 1000) |>
  dplyr::collect()

## Consulta bivariada ----
### Usando a conexão recentemente feita com o big query
df_sinasc <- tbl(con, "view_sinasc_tratamento_painel")

## Fazer uma busca com os dados de das colunas APGAR1 e _FXETARIAMAE
### criando uma contagem do número de linhas com group_by e summarise
### E filtrando valores de `APGAR1` menores ou iguais a 10

df_sinasc_apgar_idade <- df_sinasc |>
  dplyr::select(`APGAR1`, `_FXETARIAMAE`) |>
  dplyr::mutate(`APGAR1` = as.numeric(`APGAR1`)) |>
  dplyr::group_by(`APGAR1`, `_FXETARIAMAE`) |>
  dplyr::summarise(count = n()) |>
  dplyr::ungroup() |>
  dplyr::select(apgar1 = `APGAR1`, idademae = `_FXETARIAMAE`, count) |>
  dplyr::filter(apgar1 <= 10) |>
  dplyr::collect() |>
  dplyr::arrange(apgar1, idademae)

df_sinasc_apgar_idade <- df_sinasc_apgar_idade |>
  dplyr::collect()

## Ordene o dataframe df_sinasc_apgar_idade por apgar1 e idademae
df_sinasc_apgar_idade <- df_sinasc_apgar_idade |>
  dplyr::arrange(apgar1, idademae)

library(dplyr)
library(tidyr)

output$histograma <- ggiraph::renderGirafe({
  ## Crie um gráfico de histograma do tipo ggiraph com os dados de df_sinasc_ufs_nasc_hist_filt
  ## usando os dados de ano_nasc e count como tooltip
  graph_hist <- df_sinasc_ufs_nasc_hist_filt |>
    ggplot2::ggplot() +
    ggplot2::labs(title = "Nascimentos por ano",
                  x = "",
                  y = "") +
    ggplot2::scale_y_continuous(labels = scales::unit_format(unit = "mil", scale = 1e-3))  +
    ggplot2::scale_x_discrete(breaks = seq(1996, 2024, 2))  +
    ggplot2::theme_minimal() +
    ggiraph::geom_bar_interactive(stat = "identity", width = 1,
                                  aes(x = ano_nasc, y = count, fill = "#ABA2D1",
                                      tooltip = paste0("Ano: ", ano_nasc, "<br> Nascimentos: ", count)))

  ggiraph::girafe(ggobj = graph_hist, width_svg = 6, height_svg = 4)
})

## Consultando ano de nascimento e escolaridade da mãe ----
# Bigrquery ----
con <- dbConnect(
  bigrquery::bigquery(),
  project = "pdi-covid-basededados",
  dataset = "paineis"
)
df_sinasc <- tbl(con, "view_sinasc_tratamento_painel")

colnames <- colnames(df_sinasc)
df_colnames <- data.frame(colnames) |>
  dplyr::arrange(colnames)

## Contagem de nascimentos por estados e por data (ano_mes) -----
df_sinasc_ufs_nasc <- df_sinasc |>
  dplyr::mutate(ESCMAE2010, ano_nasc = `_ANONASC`) |>
  dplyr::group_by(ESCMAE2010, ano_nasc) |>
  dplyr::summarise(count = n()) |>
  dplyr::select(esc_mae = ESCMAE2010, ano_nasc, count) |>
  dplyr::ungroup() |>
  dplyr::collect()

labels_esc <- data.table::fread("data-raw/labels.csv") |>
  dplyr::select(nivel, label, variavel) |>
  dplyr::filter(variavel == "escmae2010_sinasc") |>
  dplyr::mutate(nivel = as.character(nivel))

df_sinasc_ufs_nasc_esc <- df_sinasc_ufs_nasc |>
  dplyr::left_join(labels_esc, by = c("esc_mae" = "nivel")) |>
  dplyr::mutate(label = ifelse(is.na(label), "Inválido ou nulo", label)) |>
  dplyr::group_by(ano_nasc, label) |>
  dplyr::mutate(count = sum(count)) |>
  dplyr::distinct(ano_nasc, label, .keep_all = T) |>
  dplyr::arrange(ano_nasc, label) |>
  dplyr::select(ano_nasc, label, count) |>
  dplyr::ungroup()

df_sinasc_ufs_nasc_esc |>
  dplyr::arrange(factor(ano_nasc, levels = labels_esc$label))

ggplot(df_sinasc_ufs_nasc_esc, aes(x = ano_nasc, y = count, fill = label)) +
  geom_bar(stat = "identity") +
  labs(title = "Nascimentos por ano e escolaridade da mãe",
       x = "Ano de nascimento",
       y = "Número de nascimentos") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  # scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  scale_fill_discrete(labels_esc$label)


labels_esc$label <- as.factor(labels_esc$label, labels = labels_esc$label)

























fruits <- c("Apple", "Banana", "Lemon")
fruits_2 <- c("Apple", "Banana")

fruits_3 <- fruits[fruits %in% fruits_2]

print(fruits_3)















