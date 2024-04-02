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
    ### Univariada ----
    nav_panel(
      "Univariada",
      div(
        class="row",
        div(
          class="col-6",
          bslib::card(
            # plotOutput("mapa_2")
            plotOutput("barras_1")
          )
        ),
        div(
          class="col-6",
          bslib::card(
            gt::gt_output("tabela_uf")
          )
        )
      )
    ),
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
                  inputId = "filter_sidebar_hist",
                  choices = list(
                    `Todos` = "TODOS",
                    `Regiões` = regioes_f,
                    `UFs` = ufs_f),
                  selected = "TODOS",
                )
              ),
              ggiraph::girafeOutput("histograma"),
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

  # Univariada ----
  ## Gráficos iniciais ----

  # browser()
  ## Contagem de nascimentos por estados e por data (ano_nasc)
  df_sinasc_ufs_nasc <- df_sinasc  |>
    dplyr::group_by(`_UF`, `_ANONASC`) |>
    dplyr::summarise(count = n()) |>
    dplyr::select(uf_cod = `_UF`, ano_nasc = `_ANONASC`, count) |>
    dplyr::ungroup() |>
    dplyr::collect()

  df_sinasc_ufs_nasc <- dplyr::inner_join(df_ufs, df_sinasc_ufs_nasc,
                                          by=c("uf_sigla" = "uf_cod"))


  ### Gráfico de barras ----
  df_ufs_nasc_geral <- df_sinasc_ufs_nasc |>
    dplyr::group_by(uf_sigla) |>
    dplyr::mutate(count = sum(count)) |>
    dplyr::distinct(uf_sigla, .keep_all = T) |>
    dplyr::arrange(count) |>
    dplyr::ungroup() |>
    dplyr::select(-ano_nasc)

  ## Adicionando proporção em relação à população ----
  df_ufs_nasc_geral <- df_ufs_nasc_geral |>
    dplyr::mutate(taxa_nasc = count*1000/populacao) |>
    dplyr::arrange(taxa_nasc)

  order <- df_ufs_nasc_geral$uf_sigla

  ## Filtrando variável ----


  labels <- data.table::fread("data-raw/labels.csv") |>
    dplyr::select(nivel, label, variavel)

  observeEvent(input$var_sel_1, {
    # browser()
    var_sel_db <- df_vars_filter |>
      dplyr::filter(var_all == input$var_sel_1) |>
      dplyr::pull(nome_var_db)

    # browser()
    df_sinasc_var <- df_sinasc |>
      dplyr::select(!!as.name(var_sel_db)) |>
      dplyr::filter(!is.na(!!as.name(var_sel_db)) & !!as.name(var_sel_db) != "") |>
      dplyr::group_by(!!as.name(var_sel_db)) |>
      dplyr::summarise(count = n()) |>
      dplyr::ungroup() |>
      dplyr::collect()

    labels_var <- labels |>
      dplyr::filter(variavel == var_sel_db)

    ## Lidando com inválidos
    type_inv <- case_when(
      var_sel_db == "PARTO" ~ 9,
      var_sel_db == "PESO_OMS" ~ 0,
      var_sel_db == "SEXO" ~ 0,
      var_sel_db == "RACACOR" ~ 0,
      var_sel_db == "ESCMAE" ~ 0,
      var_sel_db == "ESTCIVMAE" ~ 0,
      var_sel_db == "RACACORMAE" ~ 0,
      # var_sel_db == "CODANOMAL" ~ 0,
      var_sel_db == "APGAR1" ~ 0,
      var_sel_db == "APGAR5" ~ 0,
      var_sel_db == "TPROBSON" ~ 0,
      var_sel_db == "GESTACAO" ~ 0
    )

    if(type_inv == 9){
      df_sinasc_var <- df_sinasc_var |>
        dplyr::filter(!!as.name(var_sel_db) != "9")
    }

    # df_sinasc_var_label <- df_sinasc_var |>
    #   dplyr::inner_join(labels_var, by=setNames(var_sel_db, "variavel"))

    # df_sinasc_var |>
    #     dplyr::inner_join(labels_var, join_by=c(var_sel_db,"variavel"))

    df_table <- merge(df_sinasc_var, labels_var, by.x = var_sel_db, by.y = "nivel") |>
      dplyr::mutate(perc = round(count/sum(count)*100,2)) |>
      dplyr::select(label, perc, count)
    ## Barras 1 ----
    output$barras_1 <- renderPlot({
      title_bar_graph <- case_when(
        var_sel_db == "PARTO" ~ "Tipo de parto",
        var_sel_db == "PESO_OMS" ~ "Peso categorizado (OMS)",
        var_sel_db == "SEXO" ~ "Sexo",
        var_sel_db == "RACACOR" ~ "Raça/cor",
        var_sel_db == "ESCMAE" ~ "Escolaridade da mãe",
        var_sel_db == "ESTCIVMAE" ~ "Estado civil da mãe",
        var_sel_db == "RACACORMAE" ~ "Raça/cor da mãe",
        var_sel_db == "CODANOMAL" ~ "Anomalias congênitas",
        var_sel_db == "APGAR1" ~ "Apgar 1o minuto",
        var_sel_db == "APGAR5" ~ "Apgar 5o minuto",
        var_sel_db == "TPROBSON" ~ "Grupo de Robson",
        var_sel_db == "GESTACAO" ~ "Idade gestacional"
      )

      ## Crie um gráfico de barras do tipo ggiraph com os dados de df_table
      graph_barras <- df_table |>
        ggplot2::ggplot() +
        ggplot2::geom_bar(aes(x = label, y = perc, fill = "#ABA2D1"),
                          stat = "identity") +
        ggplot2::labs(title = paste0("Nascimentos por ", title_bar_graph),
                      subtitle = "Em proporção (%)",
                      x = "",
                      y = "") +
        ggplot2::theme_minimal() +
        ggplot2::scale_fill_identity()

      graph_barras
    })
    ## Tabela ----
    output$tabela_uf <- gt::render_gt({
      # browser()
      tabela_nasc <- df_table |>
        gt::gt(locale = "pt") |>
        gt::cols_label(
          label = gt::md(paste0(input$var_sel_1)),
          perc = gt::md("Percentual (%)"),
          count = gt::md("Total")
        ) |>
        gt::opt_interactive(use_compact_mode = TRUE,
                            use_search = TRUE)
      # gt::cols_align(align = "center", columns = nanoplots)

      tabela_nasc
    })

  })



  # browser()
  ## Histograma c/ seletor ----
  # browser()
  ## Crie um gráfico de histograma com os dados de df_sinasc_ufs_nasc, agrupando por ano_nasc
  ## e contando o número de linhas
  df_sinasc_ufs_nasc_hist <- df_sinasc_ufs_nasc |>
    dplyr::group_by(ano_nasc) |>
    dplyr::summarise(count = sum(count)) |>
    dplyr::ungroup()
  # Criar um output shiny do tipo ggiraph com um gráfico de histograma
  output$histograma <- ggiraph::renderGirafe({
    # Crie um gráfico de histograma do tipo ggiraph com os dados de df_sinasc_ufs_nasc_hist
    # usando os dados de ano_nasc e count como tooltip
    graph_hist <- df_sinasc_ufs_nasc_hist |>
      ggplot2::ggplot() +
      ggplot2::labs(title = "Nascimentos por ano",
                    x = "",
                    y = "") +
      ggplot2::scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6))  +
      ggplot2::scale_x_discrete(breaks = seq(1996, 2024, 2))  +
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "none") +
      ggiraph::geom_bar_interactive(stat = "identity", width = 1, fill = "#ABA2D1",
                                    aes(x = ano_nasc, y = count, tooltip = paste0("Ano: ", ano_nasc, "<br> Nascimentos: ", count)))

    ggiraph::girafe(ggobj = graph_hist, width_svg = 6, height_svg = 4)
  })


  ## Gráficos reativos ----

  ### Histograma ----
  observeEvent(input$filter_sidebar_hist, {
    if(input$filter_sidebar_hist != "TODOS"){
      # browser()
      df_sinasc_ufs_nasc_hist_filt <- df_sinasc_ufs_nasc |>
        dplyr::filter(uf_sigla == input$filter_sidebar_hist) |>
        dplyr::group_by(ano_nasc) |>
        dplyr::summarise(count = sum(count)) |>
        dplyr::ungroup()
      ## Criar um update output para o gráfico de histograma
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
          ggplot2::theme(legend.position = "none") +
          ggiraph::geom_bar_interactive(stat = "identity", width = 1,
                                        aes(x = ano_nasc, y = count, fill = "#ABA2D1",
                                            tooltip = paste0("Ano: ", ano_nasc, "<br> Nascimentos: ", count)))

        ggiraph::girafe(ggobj = graph_hist, width_svg = 6, height_svg = 4)
      })
    }
  })

  ### Gráfico e tabela ----
  ### Reativos à filtro lateral

  observeEvent(input$applyFilters, {
    #### Filtro ano ----
    ano_sel <- input$year

    df_sinasc_filt <- df_sinasc |>
      dplyr::filter(ano_nasc == ano_sel)

    #### País ----
    if(input$abrang == "País"){
      df_sinasc_filt <- df_sinasc_filt
      ### Unindo com dados espaciais
      df_sinasc_filt_mapa <- df_sinasc_filt |>
        dplyr::inner_join(uf_sf, by=c("_UF" = "cod_stt")) |>
        sf::st_as_sf() |>
        dplyr::select(uf_cod, uf_sigla, taxa_nasc, geometry)

      df_ufs_nasc_mapa <- sf::st_transform(df_ufs_nasc_mapa, crs = '+proj=longlat
+datum=WGS84')

      ## Criar um update output para o gráfico de mapa
      output$mapa_2 <- renderPlot({
        ## Crie um gráfico de mapa com os dados de df_sinasc_ufs_nasc_filt
        ggplot_map <- ggplot(df_sinasc_ufs_nasc_filt) +
          geom_sf(aes(fill = taxa_nasc)) +
          theme_void() +
          scale_fill_gradient2()

        ## Saída
        ggplot_map
      })

    }
    #### UF ----
    if(input$abrang == "Unidade da federação"){
      if(input$uf == "TODOS"){
        df_sinasc_filt <- df_sinasc_filt
      }else{
        df_sinasc_filt <- df_sinasc_filt |>
          dplyr::filter(`_UF` == input$uf)
      }
      ### Unindo com dados espaciais
      df_sinasc_filt_mapa <- df_sinasc_filt |>
        dplyr::inner_join(uf_sf, by=c("_UF" = "cod_stt")) |>
        sf::st_as_sf() |>
        dplyr::select(uf_sigla = `_UF`, taxa_nasc, geometry)

      df_ufs_nasc_mapa <- sf::st_transform(df_ufs_nasc_mapa, crs = '+proj=longlat
+datum=WGS84')

      ## Criar um update output para o gráfico de mapa
      output$mapa_2 <- renderPlot({
        ## Crie um gráfico de mapa com os dados de df_sinasc_ufs_nasc_filt
        ggplot_map <- ggplot(df_sinasc_filt_mapa) +
          geom_sf(aes(fill = taxa_nasc)) +
          theme_void() +
          scale_fill_gradient2()

        ## Saída
        ggplot_map
      })
    }
    #### Município ----
    if(input$abrang == "Município"){
      if(input$mun == "Selecione a UF"){
        df_sinasc_filt <- df_sinasc_filt
      }else{
        df_sinasc_filt <- df_sinasc_filt |>
          dplyr::filter(cod_mun == input$mun)
      }

      ### Unindo com dados espaciais
      df_sinasc_filt_mapa <- df_sinasc_filt |>
        dplyr::inner_join(mun_sf, by=c("_CODMUNRES" = "cod")) |>
        sf::st_as_sf() |>
        dplyr::select(uf_cod, uf_sigla, taxa_nasc, geometry)

      df_ufs_nasc_mapa <- sf::st_transform(df_ufs_nasc_mapa, crs = '+proj=longlat
+datum=WGS84')

      ## Criar um update output para o gráfico de mapa
      output$mapa_2 <- renderPlot({
        ## Crie um gráfico de mapa com os dados de df_sinasc_ufs_nasc_filt
        ggplot_map <- ggplot(df_sinasc_filt_mapa) +
          geom_sf(aes(fill = taxa_nasc)) +
          theme_void() +
          scale_fill_gradient2()

        ## Saída
        ggplot_map
      })
    }
  })


}

shinyApp(ui, server)
