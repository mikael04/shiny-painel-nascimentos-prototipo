#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

ufs_f <- c("AC", "AL", "AM", "AP", "BA", "CE", "DF", "ES", "GO", "MA", "MG", "MS",
           "MT", "PA", "PB", "PE", "PI", "PR", "RJ", "RN", "RO", "RR", "RS", "SC",
           "SE", "SP", "TO")

regioes_f <- c("Norte", "Nordeste", "Centro-Oeste", "Sudeste", "Sul")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            selectInput('selReg', "Selecione a região",
                        choices = list(`Regiões` = regioes_f,
                                       `UFs` = ufs_f)),

            shinyWidgets::pickerInput('selUF', "Selecione a UF",
                                      choices = list(`Regiões` = regioes_f,
                                                     `UFs` = ufs_f),
                                      options = list(`actions-box` = TRUE),
                                      multiple = F)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

}

# Run the application
shinyApp(ui = ui, server = server)
