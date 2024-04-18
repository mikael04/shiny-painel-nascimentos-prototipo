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

years_f <- c(2020, 2021, 2022)

## Variáveis para seleção ----
var_nasc_vivo <- c(#"Anomalias congênitas",
    "Apgar 1o minuto", "Apgar 5o minuto",
    "Grupo de Robson", "Idade gestacional",
    "Peso ao nascer (OMS)",
    "Raça/cor", "Sexo", "Tipo de Parto")
var_mae_nasc <- c("Escolaridade da mãe", "Estado civil da mãe", "Raça/cor da mãe")

var_all <- c(var_nasc_vivo, var_mae_nasc)

# UI ----
ui <- page_navbar(
    ## CSS ----
    tags$head(
        tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
    ),
    ## Título ----
    div(
        class = "title",
        "Painel de monitoramento de Nascidos Vivos"
    ),
    ## Row panel ----

    theme = bs_theme(version = 5),

    ### Univariada ----
    nav_panel(
        "Univariada",
        class = "body",
        # sidebar = sidebar(
        #     width = 350,
        #     # varSelectInput(
        #     #   "uf", "Selecione a UF",
        #     #   ufs_f
        #     # ),
        #     shinyWidgets::pickerInput(
        #         label = "Ano de referência",
        #         inputId = "year",
        #         choices = c("TODOS", years_f),
        #         selected = years_f[length(years_f)],
        #     ),
        #     shinyWidgets::pickerInput(
        #         label = "Abrangência",
        #         inputId = "abrang",
        #         choices = c("País", "Região", "Unidade da federação", "Mesorregião",
        #                     "Microregião", "Macroregião de saúde", "Região de saúde",
        #                     "Município"),
        #         selected = "País",
        #     ),
        #     # uiOutput("extra_geoloc_1"),
        #     # uiOutput("extra_geoloc_2"),
        #     # shinyWidgets::sliderTextInput(
        #     #   inputId = "date",
        #     #   label = "Escolha o Período:",
        #     #   choices = date_f,
        #     #   selected = date_f[c(1, length(date_f))]
        #     # )
        #     div(
        #         class="combine_vars",
        #         shinyWidgets::pickerInput(
        #             label = "Nascimentos por",
        #             inputId = "var_sel_1",
        #             choices = list(
        #                 `Variáveis de nascidos vivos` = var_nasc_vivo,
        #                 `Variáveis da mãe do nascido` = var_mae_nasc
        #             )
        #         )
        #     ),
        #     div(
        #         class="botao-filtros",
        #         shinyWidgets::actionBttn(
        #             inputId = "applyFilters",
        #             label = "Aplicar",
        #             style = "jelly",
        #             color = "primary"
        #         )
        #     )
        # )
    ),
    nav_panel(
        "Bivariada",
    )
)

# Server ----
server <- function(session, input, output) {
}

shinyApp(ui, server)
