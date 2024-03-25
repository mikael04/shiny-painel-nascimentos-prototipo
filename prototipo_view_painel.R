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
        label = "Mais filtros",
        style = "jelly",
        color = "primary"
      ),
      shinyWidgets::actionBttn(
        inputId = "applyFilters",
        label = "Aplicar",
        style = "jelly",
        color = "primary"
      )
    )
  ),
  ## Grid panel ----
  # grid_container(
  #   layout = c(
  #     "area1  area2",
  #     "area3  area3"
  #   ),
  #   row_sizes = c(
  #     "1.03fr",
  #     "0.97fr"
  #   ),
  #   col_sizes = c(
  #     "1fr",
  #     "1fr"
  #   ),
  #   gap_size = "10px",
  #   grid_card(
  #     area = "area1",
  #     scroll = TRUE,
  #     full_screen = TRUE,
  #     card_header(
  #                 plotOutput("mapa_1"))
  #   ),
  #   grid_card(
  #     area = "area2",
  #     scroll = FALSE,
  #     full_screen = TRUE,
  #     card_header(
  #                 plotOutput("geral_1"))
  #   ),
  #   grid_card(
  #     area = "area3",
  #     full_screen = TRUE,
  #     card_header(
  #                 plotOutput("tempo_1"))
  #   )
  # )
  # card(
  #
  # )
  ## Row panel ----
  page_navbar(
    nav_panel(
      "Univariada 1",
      div(
        class="row",
        div(
          class="col-6",
          bslib::card(
            plotOutput("mapa_1")
          )
        ),
        div(
          class="col-6",
          bslib::card(
            plotOutput("geral_1")
          )
        )
      ),
      div(
        class="row",
        bslib::card(
          # bslib::card_header("Filtre por UF ou região"),
          layout_sidebar(
            sidebar = sidebar(
              id="first_graph_sidebar",
              title = "Filtre a UF ou região",
              position = "left", open = TRUE,
              shinyWidgets::pickerInput(
                label = "Selecione a UF",
                inputId = "first_graph_sidebar",
                choices = c("TODOS", ufs_f),
                selected = "TODOS",
              )
            ),
            plotOutput("tempo_1"),
            border = FALSE
          )
        ),
      )
    ),

    nav_panel(
      "Univariada 2",
      div(
        class="row",
        div(
          class="col-6",
          bslib::card(
            plotOutput("mapa_2")
          )
        ),
        div(
          class="col-6",
          bslib::card(
            gt::gt_output("tabela_uf")
          )
        )
      ),
      div(
        class="row",
        bslib::card(
          layout_sidebar(
            sidebar = sidebar(
              id="sidebar_hist",
              title = "Filtre a UF ou região",
              position = "left", open = TRUE,
              shinyWidgets::pickerInput(
                label = "Selecione a UF",
                inputId = "filter_sidebar_hist",
                choices = c("TODOS", ufs_f),
                selected = "TODOS",
              )
            ),
            ggiraph::girafeOutput("histograma"),
            border = FALSE
          )
        )
      )
    ),
    nav_panel(
      "Bivariada",
      bslib::card(
        card_header("Apgar no 1° minuto por idade da mãe", class="title-center"),
        gt::gt_output("tabela_bivariada")
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
  # Univariada 1 -----
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

  output$geral_1 <- renderPlot({
    ggplot(df_ufs_nasc_geral) +
      geom_col(aes(x = uf_sigla, y = taxa_nasc), fill = "#ABA2D1") +
      theme_minimal() +
      scale_x_discrete(limits = order) +
      # scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6),
      #                    # limits=c(0, 1500000)
      #                    ) +
      labs(
        title = "Taxa de Nascimentos por mil nas UF",
        subtitle = "Nascimentos de 1996 à 2022",
        y = "",
        x = " "
      ) +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.major.x = element_blank()
      )
  })

  ### Mapa ----

  ### Unindo com dados espaciais
  df_ufs_nasc_mapa <- df_ufs_nasc_geral |>
    dplyr::inner_join(uf_sf, by=c("uf_cod" = "cod_stt")) |>
    dplyr::select(uf_cod, uf_sigla, taxa_nasc, geometry) |>
    sf::st_as_sf()

  df_ufs_nasc_mapa <- sf::st_transform(df_ufs_nasc_mapa, crs = '+proj=longlat
+datum=WGS84')

  output$mapa_1 <- renderPlot({
    ggplot_map <- ggplot(df_ufs_nasc_mapa) +
      geom_sf(aes(fill = taxa_nasc)) +
      theme_void() +
      scale_fill_gradient2() +
      ggtitle("Taxa de nascimento por UF")

    ## Saída
    ggplot_map
  })

  ### Série temporal ----

  ### Adicionando colunas de datas
  df_sinasc_tempo <- df_sinasc_ufs_nasc |>
    dplyr::group_by(ano_nasc) |>
    dplyr::mutate(count = sum(count)) |>
    dplyr::distinct(ano_nasc, .keep_all = T) |>
    dplyr::ungroup() |>
    dplyr::select(-uf_nome, -uf_sigla,  -uf_cod, -populacao) |>
    dplyr::arrange(ano_nasc) |>
    dplyr::mutate(ano_nasc = lubridate::as_date(paste0(ano_nasc, "-01-01")))


  axis_x <- seq(from = lubridate::ymd(min(df_sinasc_tempo$ano_nasc)),
                to = lubridate::ymd(max(df_sinasc_tempo$ano_nasc)),
                by = "2 years")

  output$tempo_1 <- renderPlot({
    ggplot(df_sinasc_tempo) +
      geom_line(aes(x = ano_nasc, y = count)) +
      theme_minimal() +
      scale_y_continuous(labels = scales::unit_format(unit = "M", scale = 1e-6),
                         limits=c(0, 4000000)) +
      scale_x_date(breaks = axis_x, labels = scales::date_format("%Y")) +
      labs(
        title = "Nascimentos por ano",
        y = "",
        x = " "
      ) +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.major.x = element_blank()
      )
  })

  # Univariada 2 ----

  ## Mapa 2 ----
  output$mapa_2 <- renderPlot({
    ggplot_map <- ggplot(df_ufs_nasc_mapa) +
      geom_sf(aes(fill = taxa_nasc)) +
      theme_void() +
      scale_fill_gradient2()

    ## Saída
    ggplot_map
  })

  ## Tabela UF ----
  output$tabela_uf <- gt::render_gt({
    # browser()
    tabela_nasc <- df_ufs_nasc_geral |>
      dplyr::select(uf_nome, taxa_nasc, count, populacao) |>
      gt::gt(locale = "pt") |>
      gt::cols_label(
        uf_nome = gt::md("UF"),
        taxa_nasc = gt::md("Taxa de nascimentos"),
        populacao = gt::md("População"),
        count = gt::md("Total")
      ) |>
      gt::opt_interactive(use_compact_mode = TRUE,
                          use_search = TRUE)
      # gt::cols_align(align = "center", columns = nanoplots)

    tabela_nasc
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
      ggiraph::geom_bar_interactive(stat = "identity", width = 1,
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
          ggiraph::geom_bar_interactive(stat = "identity", width = 1,
                                        aes(x = ano_nasc, y = count, tooltip = paste0("Ano: ", ano_nasc, "<br> Nascimentos: ", count)))

        ggiraph::girafe(ggobj = graph_hist, width_svg = 6, height_svg = 4)
      })
    }

  })



  # Bivariada ----
  ## Fazer uma busca com os dados de das colunas APGAR1 e IDADEMAE criando uma contagem do número de linhas com group_by e summarise
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


  ## Fazer um pivot_wider usando a coluna idade_mae como coluna e count como valor
  df_sinasc_apgar_idade_wide <- df_sinasc_apgar_idade |>
    tidyr::pivot_wider(names_from = idademae, values_from = count) |>
    dplyr::mutate(across(everything(), ~replace_na(., 0))) |>
    dplyr::mutate(across(everything(), as.integer))

  # browser()
  ## Tabela bivariada ----
  ## Criar uma tabela gt de output com os dados de df_sinasc_apgar_idade_wide
  output$tabela_bivariada <- gt::render_gt({
    tabela_bivariada <- df_sinasc_apgar_idade_wide |>
      gt::gt(locale = "pt") |>
      gt::cols_label(
        apgar1 = gt::md("Apgar no 1° minuto"),
      ) |>
      gt::cols_move(
        columns = `Menor de 14 anos`,
        after = apgar1
      ) |>
      gt::opt_interactive(use_compact_mode = TRUE)
    tabela_bivariada
  })


}

shinyApp(ui, server)
