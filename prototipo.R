library(shiny)
## Layout
library(bslib)
library(gridlayout)
## Gráficos
library(ggplot2)
## Manipulação de dados
library(dplyr)
library(dbplyr)
## Comunicação com o banco de dados
library(bigrquery)
library(DBI)

projectid = "pdi-covid-basededados"

proj_name <- "`pdi-covid-basededados.sinasc.view_sinasc_a_partir_2020`"

## UFS ----

# Set your query
sql <- paste0("SELECT DISTINCT __PDI_UF FROM ", proj_name)

# Run the query; this returns a bq_table object that you can query further
tb <- bq_project_query(projectid, sql)

ufs_f <- bq_table_download(tb) |>
  dplyr::arrange(`__PDI_UF`) |>
  dplyr::pull()

## Datas ----

# Set your query
sql <- paste0("SELECT DISTINCT __PDI_DTNASC_ANO_MES FROM ", proj_name)

# Run the query; this returns a bq_table object that you can query further
tb <- bq_project_query(projectid, sql)

dtnasc <- bq_table_download(tb)

dtnasc <- dtnasc |>
  dplyr::mutate(year = substr(`__PDI_DTNASC_ANO_MES`, 1, 4),
                month = substr(`__PDI_DTNASC_ANO_MES`, 5, 6),
                date = lubridate::ym(`__PDI_DTNASC_ANO_MES`),
                ym = format(as.Date(date), "%m-%Y"))

years_f <- dplyr::distinct(dtnasc, year) |> dplyr::arrange(year) |> dplyr::pull()
months_f <- dplyr::distinct(dtnasc, month) |> dplyr::arrange(month)  |> dplyr::pull()
# date = dplyr::distinct(dtnasc, date) |> dplyr::arrange(date)  |> dplyr::pull()
date_f <- dplyr::distinct(dtnasc, ym, .keep_all = T) |> dplyr::arrange(`__PDI_DTNASC_ANO_MES`)  |> dplyr::pull(ym)

cod_ibge_mun <- data.table::fread("data-raw/cod_ibge_mun_ufs.csv") |>
  dplyr::select(cod_ibge, nome_mun, uf, nome_uf)

## UI ----
ui <- page_sidebar(
  title = "Painel de monitoramento de Nascidos Vivos",
  sidebar = sidebar(
    width = 350,
    # varSelectInput(
    #   "uf", "Selecione a UF",
    #   ufs_f
    # ),
    shinyWidgets::pickerInput(
      label = "Local de registro",
      inputId = "loc_reg",
      choices = c("Nascidos vivos por residência", "Nascidos vivos por ocorrência")
    ),
    shinyWidgets::pickerInput(
      label = "Abrangência",
      inputId = "abrang",
      choices = c("País", "Região", "Unidade da federação", "Mesorregião",
                  "Microregião", "Macroregião de saúde", "Região de saúde",
                  "Município"),
      selected = "País",
    ),
    shinyWidgets::pickerInput(
      label = "Ano de referência",
      inputId = "year",
      choices = c("TODOS", years_f),
      selected = years_f[length(years_f)],
    ),
    uiOutput("extra_geoloc_1"),
    uiOutput("extra_geoloc_2"),
    # shinyWidgets::sliderTextInput(
    #   inputId = "date",
    #   label = "Escolha o Período:",
    #   choices = date_f,
    #   selected = date_f[c(1, length(date_f))]
    # )
    div(
      class="combine_vars",
      shinyWidgets::pickerInput(
        label = "Selecione uma variável",
        inputId = "var_sel_1",
        choices = c("Apgar no 1° minuto", "B", "C", "D")
      ),
      shinyWidgets::awesomeCheckbox(
        inputId = "cmp_var",
        label = "Cruzar variáveis",
        value = FALSE
      ),
      uiOutput("extra_var")
    ),
    div(
      class="botao-filtros",
      shinyWidgets::actionBttn(
        inputId = "addFilters",
        label = "Adicionar mais filtros",
        style = "jelly",
        color = "primary"
      )
    )
  ),
  ## Grid panel ----
  grid_container(
    layout = c(
      "area1  area2",
      "area3  area3"
    ),
    row_sizes = c(
      "1.03fr",
      "0.97fr"
    ),
    col_sizes = c(
      "1fr",
      "1fr"
    ),
    gap_size = "10px",
    grid_card(
      area = "area1",
      full_screen = TRUE,
      card_header("Mapa",
                  plotOutput("mapa_1"))
    ),
    grid_card(
      area = "area2",
      full_screen = TRUE,
      card_header("Área 2")
    ),
    grid_card(
      area = "area3",
      full_screen = TRUE,
      card_header("Área 3")
    )
  )
  # card(
  #
  # )
)

## Server ----
server <- function(session, input, output) {

  # Dados geográficos ----
  ## Municípios
  mun_sf <- sf::read_sf(here::here("data-raw/dados-espaciais/municipios_sf.shp"))
  ## UFs
  uf_sf <- sf::read_sf(here::here("data-raw/dados-espaciais/uf_sf.shp")) |>
    dplyr::select(cod_stt, geometry)
  ## Dados ibge municípios
  df_mun <- data.table::fread("data-raw/ibge-dados-municipais-tratados.csv")
  df_uf <- data.table::fread("data-raw/ibge-dados-ufs-tratados.csv")

  # Bigrquery ----
  con <- dbConnect(
    bigrquery::bigquery(),
    project = "pdi-covid-basededados",
    dataset = "sinasc"
  )

  df_sinasc_2020_2022 <- tbl(con, "view_sinasc_a_partir_2020")


  # Filtros reativos ----
  observeEvent(input$abrang, {
    if(input$abrang == "País"){
      output$extra_geoloc_1 <- NULL
      output$extra_geoloc_2 <- NULL
    }
    if(input$abrang == "Unidade da federação"){
      output$extra_geoloc_1 <- renderUI({
        shinyWidgets::pickerInput(
          label = "Selecione a UF",
          inputId = "uf",
          choices = c("TODOS", ufs_f),
          selected = "TODOS",
        )
      })
      output$extra_geoloc_2 <- NULL
    }

    if(input$abrang == "Município"){
      output$extra_geoloc_1 <- renderUI({
        shinyWidgets::pickerInput(
          label = "Selecione a UF",
          inputId = "uf",
          choices = c(ufs_f),
          # selected = " ",
          options = list(title = "choose here")
        )
      })
      output$extra_geoloc_2 <- renderUI({
        shinyWidgets::pickerInput(
          label = "Município",
          inputId = "mun",
          choices = c("Selecione a UF")
        )
      })
    }
  })
  observeEvent(input$uf,{
    if(input$uf != " "){
      muns_uf_sel <- cod_ibge_mun |>
        dplyr::filter(uf == input$uf) |>
        dplyr::arrange(nome_mun, .locale = "pt_BR") |>
        dplyr::pull(nome_mun)

      shinyWidgets::updatePickerInput(
        session = session, inputId = "mun",
        choices = muns_uf_sel
      )
    }
  })

  observeEvent(input$cmp_var, {
    if(input$cmp_var){
      output$extra_var <- renderUI({
        shinyWidgets::pickerInput(
          label = "Selecione uma variável para cruzamento",
          inputId = "var_sel_2",
          choices = c("A", "B", "C"),
        )
      })
    }else{
      output$extra_var <- NULL
    }
  })
  # Modal ----
  observeEvent(input$addFilters, {
    showModal(modalDialog(
      title = "Selecione os demais filtros",
      easyClose = FALSE,
      size = "l",
      grid_container(
        layout = c(
          "a1  a2",
          "a3  a4",
          "a5  a6",
          "a7  a8"
        ),
        row_sizes = c(
          "1.03fr",
          "1.03fr",
          "1.03fr",
          "1.03fr"
        ),
        col_sizes = c(
          "1fr"
        ),
        gap_size = "10px",
        grid_place(
          area = "a1",
          shinyWidgets::pickerInput(
            label = "Raça/cor",
            inputId = "raca_cor",
            choices = c("Todas", "Branca", "Preta", "Amarela", "Parda", "Indígena")
          )
        ),
        grid_place(
          area = "a2",
          shinyWidgets::pickerInput(
            label = "Sexo",
            inputId = "sexo",
            choices = c("Todos", "Feminino", "Masculino")
          )
        ),
        grid_place(
          area = "a3",
          shinyWidgets::pickerInput(
            label = "Grupo etário da mãe",
            inputId = "idade_mae",
            choices = c("Todos", "0 à 14", "15 à 19", "20 à 24", "25 à 29", "30 à 34",
                        "35 à 39", "40+")
          )
        ),
        grid_place(
          area = "a4",
          shinyWidgets::pickerInput(
            label = "Escolaridade da mãe",
            inputId = "esc_mae",
            choices = c("Todas", "Sem escolaridade", "Fundamental I (1ª a 4ª série)",
                        "Fundamental II (5ª a 8ª série)",  "Médio (antigo 2º Grau)",
                        "Superior incompleto", "Superior completo")
          )
        ),
        grid_place(
          area = "a5",
          shinyWidgets::pickerInput(
            label = "Mês que iniciou o pré-natal",
            inputId = "mes_prenat",
            choices = c("Todos", "1º mês", "2º mês", "3º mês", "4º mês", "5º mês",
                        "6º mês", "7º mês", "8º mês", "9º mês")
          )
        ),
        grid_place(
          area = "a6",
          shinyWidgets::pickerInput(
            label = "Tipo de parto",
            inputId = "tipo_parto",
            choices = c("Todos", "Cesáreo", "Vaginal")
          )
        ),
        grid_place(
          area = "a7",
          shinyWidgets::pickerInput(
            label = "Número de consultas pré-natal",
            inputId = "const_prenat",
            choices = c("Todos", "Nenhuma", "1 à 3", "4 à 6", "7 ou mais")
          )
        ),
        grid_place(
          area = "a8",
          shinyWidgets::pickerInput(
            label = "Grupo de Robson",
            inputId = "tprobson",
            choices = c("Todos", "1 à 4", "5", "6 à 10")
          )
        )
      ),
      footer = tagList(actionButton("filter", "Finalizar filtro"),
                       actionButton("close", "Fechar")))
    )
  })
  observeEvent(input$close, {
    removeModal()
  })
  # Gráficos iniciais ----

  ## Contagem de nascimentos por estados e por data (ano_mes)
  df_sinasc_2020_2022_ufs_nasc <- df_sinasc_2020_2022 |>
    dplyr::group_by(`__PDI_UF`, `__PDI_DTNASC_ANO_MES`) |>
    dplyr::summarise(count = n()) |>
    dplyr::select(cod_uf = `__PDI_UF`, ano_mes = `__PDI_DTNASC_ANO_MES`, count) |>
    dplyr::ungroup() |>
    dplyr::collect()


  ## Gráfico de barras ----
  df_ufs_nasc_geral <- df_sinasc_2020_2022_ufs_nasc |>
    dplyr::mutate(sigla_uf = cod_uf) |>
    dplyr::group_by(sigla_uf) |>
    dplyr::summarise(count = sum(count))

  ## Mapa ----

  ### Unindo com dados espaciais
  df_ufs_nasc_mapa <- df_ufs_nasc_geral |>
    dplyr::inner_join(df_uf |> dplyr::select(cod_uf, sigla_uf),
                      by=c("sigla_uf")) |>
    dplyr::inner_join(uf_sf, by=c("cod_uf" = "cod_stt")) |>
    dplyr::select(cod_uf, sigla_uf, count, geometry) |>
    sf::st_as_sf()

  df_ufs_nasc_mapa <- sf::st_transform(df_ufs_nasc_mapa, crs = '+proj=longlat
+datum=WGS84')

  output$mapa_1 <- renderPlot({
    ggplot_map <- ggplot(df_ufs_nasc_mapa) +
      geom_sf(aes(fill = count)) +
      theme_void() +
      scale_fill_gradient2()

    ## Saída
    ggplot_map
  })



  ## Série temporal ----

  ### Adicionando colunas de datas
  df_sinasc_tempo <- df_sinasc_2020_2022_ufs_nasc |>
    dplyr::mutate(year = substr(ano_mes, 1, 4),
                  month = substr(ano_mes, 5, 6),
                  date = lubridate::ym(ano_mes),
                  ym = format(as.Date(date), "%m-%Y"))


  # Gráficos reativos ----
}

shinyApp(ui, server)
