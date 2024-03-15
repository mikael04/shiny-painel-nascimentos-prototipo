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
library(shinipsum)

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
      class="botao-filtros",
      shinyWidgets::actionBttn(
        inputId = "addFilters",
        label = "Adicionar mais filtros",
        style = "jelly",
        color = "primary"
      )
    )
  ),
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
      card_header("Área 1",
                  shinipsum::random_ggplotly("bar"))
    ),
    grid_card(
      area = "area2",
      full_screen = TRUE,
      card_header("Área 2",
                  shinipsum::random_ggplotly("raster"))
    ),
    grid_card(
      area = "area3",
      full_screen = TRUE,
      card_header("Área 3",
                  shinipsum::random_ggplotly("line"))
    )
  )
  # card(
  #
  # )
)

## Server ----
server <- function(session, input, output) {

  ## Conexão com bigquery usando bigrquery
  con <- dbConnect(
    bigrquery::bigquery(),
    project = "pdi-covid-basededados",
    dataset = "sinasc"
  )

  view_sinasc_2020_2022 <- tbl(con, "view_sinasc_a_partir_2020")

  ## Recebendo dados dos filtros


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
  observeEvent(input$addFilters, {
    showModal(modalDialog(
      title = "Selecione os demais filtros",
      easyClose = FALSE,
      size = c("l"),
      layout = c(
        "area1  area2",
        "area3  area4"
      ),
      row_sizes = c(
        "0.97fr",
        "0.97fr"
        # "0.97fr",
        # "0.97fr",
      ),
      col_sizes = c(
        "1fr"
      ),
      gap_size = "0px",
      grid_place(
        area = "area1",
        shinyWidgets::pickerInput(
          label = "Raça/cor",
          inputId = "raca_cor",
          choices = c("Branca", "Preta", "Amarela", "Parda", "Indígena", "Todas")
        )
      ),
      grid_place(
        area = "area2",
        shinyWidgets::pickerInput(
          label = "Sexo",
          inputId = "sexo",
          choices = c("Feminino", "Masculino", "Todos")
        )
      ),
      grid_place(
        area = "area3",
        shinyWidgets::pickerInput(
          label = "Grupo etário da mãe",
          inputId = "idade_mae",
          choices = c("0 à 14", "15 à 19", "20 à 24", "25 à 29", "30 à 34",
                      "35 à 39", "40+", "Todos")
        )
      ),
      grid_place(
        area = "area4",
        shinyWidgets::pickerInput(
          label = "Escolaridade da mãe",
          inputId = "esc_mae",
          choices = c("Sem escolaridade", "Fundamental I (1ª a 4ª série)",
                      "Fundamental II (5ª a 8ª série)",  "Médio (antigo 2º Grau)",
                      "Superior incompleto", "Superior completo", "Todos")
        )
      ),
      # shinyWidgets::pickerInput(
      #   label = "Mês que iniciou o pré-natal",
      #   inputId = "mes_prenat",
      #   choices = c("3º mês", "3º mês", "3º mês", "3º mês", "3º mês", "3º mês",
      #               "3º mês", "3º mês", "Todos")
      # ),
      # shinyWidgets::pickerInput(
      #   label = "Número de consultas pré-natal",
      #   inputId = "const_prenat",
      #   choices = c("Nenhuma", "1 à 3", "4 à 6", "7 ou mais", "Todos")
      # ),
      # shinyWidgets::pickerInput(
      #   label = "Grupo de Robson",
      #   inputId = "tprobson",
      #   choices = c("1 à 4", "5", "6 à 10",  "Todos")
      # ),
      # shinyWidgets::pickerInput(
      #   label = "Tipo de parto",
      #   inputId = "tipo_parto",
      #   choices = c("Cesáreo", "Vaginal", "Todos")
      # ),
      footer = tagList(actionButton("filter", "Finalizar filtro"),
                       actionButton("close", "Fechar")))
    )
  })
  observeEvent(input$close, {
    removeModal()
  })
  ## Gráficos ----

}

shinyApp(ui, server)
