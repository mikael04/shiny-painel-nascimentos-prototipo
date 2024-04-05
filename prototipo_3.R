# Library ----
library(shiny)
## Layout
library(bslib)
library(gridlayout)
## Gráficos
library(ggplot2)
## Manipulação de dados
library(dplyr)
library(dbplyr)
library(tidyr)
## Comunicação com o banco de dados
library(bigrquery)
library(DBI)
## Pacote para tabelas
library(gt)
library(reactable)

## Definindo opções de idiomas para tabelas
options(reactable.language = reactableLang(
  pageSizeOptions = "\u663e\u793a {rows}",
  pageInfo = "{rowStart} \u81f3 {rowEnd} \u9879\u7ed3\u679c,\u5171 {rows} \u9879",
  pagePrevious = "\u4e0a\u9875",
  pageNext = "\u4e0b\u9875"
))

# nolint: line_length_linter.

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

regioes_f <- c("Norte", "Nordeste", "Sudeste", "Sul", "Centro-Oeste")

# df_ufs <- data.table::fread("data-raw/pop-2022-est-ibge.csv")

## Datas ----

# Set your query
sql <- paste0("SELECT DISTINCT _ANONASC FROM ", proj_name)

# Run the query; this returns a bq_table object that you can query further
tb <- bq_project_query(projectid, sql)

anos_nasc <- bq_table_download(tb) |>
  dplyr::collect()

years_f <- anos_nasc |> dplyr::arrange(anos_nasc) |>  dplyr::pull()

cod_ibge_mun <- data.table::fread("data-raw/cod_ibge_mun_ufs.csv") |>
  dplyr::select(cod_ibge, nome_mun, uf, nome_uf)

## Variáveis para seleção ----
var_nasc_vivo <- c(#"Anomalias congênitas",
  "Apgar 1o minuto", "Apgar 5o minuto",
  "Grupo de Robson", "Idade gestacional",
  "Peso ao nascer (OMS)",
  "Raça/cor", "Sexo", "Tipo de Parto")
var_mae_nasc <- c("Escolaridade da mãe", "Estado civil da mãe", "Raça/cor da mãe")

var_all <- c(var_nasc_vivo, var_mae_nasc)
# var_sel <- c("Anomalias congênitas", "Apgar 1o minuto", "Apgar 5o minuto",
#              "Grupo de Robson", "Idade gestacional",
#              "Peso ao nascer (OMS)",
#              "Raça/cor", "Sexo", "Tipo de Parto",
#              "Escolaridade da mãe", "Estado civil da mãe",
#              "Raça/cor da mãe"
#              )
nome_var_db <- c(#"CODANOMAL",
  "APGAR1", "APGAR5", "TPROBSON", "GESTACAO", "PESO_OMS",
  "RACACOR", "PARTO", "SEXO",
  "ESCMAE", "ESTCIVMAE", "RACACORMAE")
df_vars_filter <- data.frame(var_all, nome_var_db)

# UI ----
ui <- page_sidebar(
  ## CSS ----
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ),
  ## Título ----
  title = "Painel de monitoramento de Nascidos Vivos",
  ## Sidebar ----
  sidebar = sidebar(
    width = 350,
    # varSelectInput(
    #   "uf", "Selecione a UF",
    #   ufs_f
    # ),
    shinyWidgets::pickerInput(
      label = "Ano de referência",
      inputId = "year",
      choices = c("TODOS", years_f),
      selected = years_f[length(years_f)],
    ),
    shinyWidgets::pickerInput(
      label = "Abrangência",
      inputId = "abrang",
      choices = c("País", "Região", "Unidade da federação", "Mesorregião",
                  "Microregião", "Macroregião de saúde", "Região de saúde",
                  "Município"),
      selected = "País",
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
        label = "Nascimentos por",
        inputId = "var_sel_1",
        choices = list(
          `Variáveis de nascidos vivos` = var_nasc_vivo,
          `Variáveis da mãe do nascido` = var_mae_nasc
        )
      )
    ),
    div(
      class="botao-filtros",
      shinyWidgets::actionBttn(
        inputId = "applyFilters",
        label = "Aplicar",
        style = "jelly",
        color = "primary"
      )
    )
  ),
  ## Row panel ----
  page_navbar(
    # ### Univariada ----
    # nav_panel(
    #   "Univariada",
    #   div(
    #     class="row",
    #     div(
    #       class="col-6",
    #       bslib::card(
    #         # plotOutput("mapa_2")
    #         plotOutput("barras_1")
    #       )
    #     ),
    #     div(
    #       class="col-6",
    #       bslib::card(
    #         gt::gt_output("tabela_uf")
    #       )
    #     )
    #   )
    # ),
    ### Histograma ----
    nav_panel(
      "Nascimentos por área geográfica",
      bslib::card(
        card_header("Nascimentos", class="title-center"),
        div(
          class="row",
          bslib::card(
            layout_sidebar(
              sidebar = sidebar(
                id="sidebar_hist",
                title = "Filtre a UF ou região",
                position = "left", open = TRUE,
                shinyWidgets::pickerInput(
                  inputId = "filt_hist_reg_uf",
                  choices = list(
                    `Todos` = "TODOS",
                    `Regiões` = regioes_f,
                    `UFs` = ufs_f),
                  selected = "TODOS",
                ),
                shinyWidgets::pickerInput(
                  inputId = "filt_hist_racacor",
                  choices = c("Todas", "Branca", "Preta", "Amarela", "Parda", "Indígena"),
                  selected = "Todas",
                )
              ),
              shinycssloaders::withSpinner(ggiraph::girafeOutput("histograma")),
              border = FALSE
            )
          )
        )
      )
    )

  )
)

# Server ----
server <- function(session, input, output) {
  # Dados geográficos ----
  ## Municípios
  mun_sf <- sf::read_sf(here::here("data-raw/dados-espaciais/municipios_sf.shp"))
  ## UFs
  uf_sf <- sf::read_sf(here::here("data-raw/dados-espaciais/uf_sf.shp")) |>
    dplyr::select(cod_stt, geometry)
  ## Dados ibge municípios
  df_mun <- data.table::fread("data-raw/ibge-dados-municipais-tratados.csv")
  df_ufs <- data.table::fread("data-raw/ibge-ufs-pop-2022-est.csv")

  # Bigrquery ----
  con <- dbConnect(
    bigrquery::bigquery(),
    project = "pdi-covid-basededados",
    dataset = "paineis"
  )

  df_sinasc <- tbl(con, "view_sinasc_tratamento_painel")

  # Valores reativos ----
  start = reactiveValues(value = TRUE)

  # Filtros reativos ----
  ## Geolocalização ----
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
          choices = c("Idade da mãe", "Escolaridade da mãe", "C"),
        )
      })
    }else{
      output$extra_var <- NULL
    }
  })

  ## Dados iniciais ----

  # browser()

  ## Histograma c/ seletor ----
  ### Separando dados por filtros ----
  #### Com UF, será filtrado o df que faz o join
  df_ufs_f <- reactive({
    if(input$filt_hist_reg_uf == "TODOS"){
      df_ufs_f <- df_ufs
    }
    if(input$filt_hist_reg_uf %in% regioes_f){
      df_ufs_f <- df_ufs |>
        dplyr::filter(uf_reg == input$filt_hist_reg_uf)
    }
    if(input$filt_hist_reg_uf %in% ufs_f){
      df_ufs_f <- df_ufs |>
        dplyr::filter(uf_sigla == input$filt_hist_reg_uf)
    }
    df_ufs_f
    })

  #### Com racacor filtraremos a base original
  df_sinasc_f <- reactive({
    ## Contagem de nascimentos por estados e por data (ano_nasc)
    df_sinasc_ufs_nasc <- df_sinasc  |>
      dplyr::filter(RACACOR != "0" || RACACOR != "") |>
      dplyr::group_by(`_UF`, `_ANONASC`, RACACOR) |>
      dplyr::summarise(count = n()) |>
      dplyr::select(uf_cod = `_UF`, ano_nasc = `_ANONASC`, racacor = RACACOR, count) |>
      dplyr::ungroup() |>
      dplyr::collect()

    if(input$filt_hist_racacor == "Todas"){
      df_sinasc_ufs_nasc <- df_sinasc_ufs_nasc
    }
    if(input$filt_hist_racacor != "Todas"){
      df_sinasc_ufs_nasc <- df_sinasc_ufs_nasc |>
        dplyr::filter(raca_cor == input$filt_hist_racacor)
    }
    df_sinasc_ufs_nasc
  })


  # Criar um output shiny do tipo ggiraph com um gráfico de histograma
  ## Histograma output ----
  output$histograma <- ggiraph::renderGirafe({
    df_sinasc_ufs_nasc <- dplyr::inner_join(df_ufs_f(), df_sinasc_f(),
                                            by=c("uf_sigla" = "uf_cod"))
    # browser()

    browser()
    ## Crie um gráfico de histograma com os dados de df_sinasc_ufs_nasc, agrupando por ano_nasc
    ## e contando o número de linhas
    ## Se região, apresentar diferentes linhas por UF
    if(input$filt_hist_reg_uf %in% regioes_f){
      df_sinasc_ufs_nasc_hist <- df_sinasc_ufs_nasc |>
        dplyr::group_by(ano_nasc, uf_sigla) |>
        dplyr::summarise(count = sum(count)) |>
        dplyr::ungroup()

      # Crie um gráfico de histograma do tipo ggiraph com os dados de df_sinasc_ufs_nasc_hist
      # usando os dados de ano_nasc e count como tooltip
      graph_hist <- df_sinasc_ufs_nasc_hist |>
        ggplot2::ggplot() +
        ggplot2::labs(title = "Nascimentos por ano",
                      subtitle = "Agrupados por UF",
                      x = "",
                      y = "") +
        ggplot2::scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6))  +
        ggplot2::scale_x_discrete(breaks = seq(1996, 2024, 2))  +
        ggplot2::theme_minimal() +
        ggplot2::theme(legend.position = "none") +
        ggiraph::geom_line_interactive(width = 1, fill = "#ABA2D1",
                                      aes(x = ano_nasc, y = count, group = UF, tooltip = paste0("Ano: ", ano_nasc, "<br> Nascimentos: ", count)))
        # ggiraph::geom_bar_interactive(stat = "identity", width = 1, fill = "#ABA2D1",
        #                               aes(x = ano_nasc, y = count, tooltip = paste0("Ano: ", ano_nasc, "<br> Nascimentos: ", count)))


      ggiraph::girafe(ggobj = graph_hist, width_svg = 6, height_svg = 4)
    }
    ## Se não for região, apresentar diferentes linhas por UF selecionada + raca/cor
    if(input$filt_hist_reg_uf %in% ufs_f || input$filt_hist_reg_uf == "TODOS"){
      df_sinasc_ufs_nasc_hist <- df_sinasc_ufs_nasc |>
        dplyr::group_by(ano_nasc, racacor) |>
        dplyr::summarise(count = sum(count)) |>
        dplyr::ungroup()

      ## Adicionando labels

      labels <- data.table::fread("data-raw/labels.csv") |>
        dplyr::select(nivel, label, variavel)

      labels_racacor <- labels |>
        dplyr::filter(variavel == "RACACOR") |>
        dplyr::mutate(nivel = as.character(nivel))

      df_sinasc_ufs_nasc_hist <- inner_join(df_sinasc_ufs_nasc_hist, labels_racacor,
                                            by = c("racacor" = "nivel")) |>
        dplyr::select(-racacor, -variavel) |>
        dplyr::rename(racacor = label)

      # Crie um gráfico de histograma do tipo ggiraph com os dados de df_sinasc_ufs_nasc_hist
      # usando os dados de ano_nasc e count como tooltip
      graph_hist <- df_sinasc_ufs_nasc_hist |>
        ggplot2::ggplot() +
        ggplot2::labs(title = "Nascimentos por ano",
                      subtitle = "Agrupados por raça/cor",
                      x = "",
                      y = "") +
        ggplot2::theme_minimal() +
        ggplot2::scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6))  +
        ggplot2::scale_x_discrete(breaks = seq(1996, 2024, 2))  +
        ggiraph::geom_line_interactive(aes(x = ano_nasc, y = count, group = racacor, color = racacor,
                                           tooltip = paste0("Ano: ", ano_nasc, "<br> Raça/cor: ", racacor,
                                                            "<br> Nascimentos: ", count)))
        # ggplot2::geom_line(aes(x = ano_nasc, y = count, group = racacor, color = racacor))
      # ggiraph::geom_bar_interactive(stat = "identity", width = 1, fill = "#ABA2D1",
      #                               aes(x = ano_nasc, y = count, tooltip = paste0("Ano: ", ano_nasc, "<br> Nascimentos: ", count)))


      # graph_hist
      ggiraph::girafe(ggobj = graph_hist, width_svg = 6, height_svg = 4)
    }

  })
}

shinyApp(ui, server)
