library(shiny)
library(bslib)

ui <- function() {
    page_navbar(
        id = "main_nav",
        nav_panel("first", "First content",
                  nav_panel("first_1", "First content"),
                  nav_panel("first_2", "First content")),
        nav_panel("second", "Second content"),
        # Dropdown with card
        nav_menu(
            title = "Card",
            nav_item(
                card(
                    card_title("Hey it's a card!"),
                    card_body("And it has a body!"),
                    class = "border-0",
                    style = css(width = "25rem")
                )
            )
        ),
        # Dropdown with navset card
        nav_menu(
            title = "Nav Card",
            nav_item(
                navset_card_pill(
                    id = "inner_nav",
                    nav_panel("in_first", "Inner first content"),
                    nav_panel("in_second", "Inner second content")
                ) |>
                    tagAppendAttributes(
                        # Remove the border from the card
                        class = "border-0",
                        # Force the card to be at least 25rem wide
                        style = css(min_width = "25rem"),
                        # Prevent the menu from closing when clicking on the card tab
                        onclick = "event.stopPropagation();"
                    )
            ),
            nav_item(
                navset_card_pill(
                    id = "inner_nav_2",
                    nav_panel("in_first_2", "Inner first content"),
                    nav_panel("in_second_2", "Inner second content")
                ) |> |>
                    tagAppendAttributes(
                        # Remove the border from the card
                        class = "border-0",
                        # Force the card to be at least 25rem wide
                        style = css(min_width = "25rem"),
                        # Prevent the menu from closing when clicking on the card tab
                        onclick = "event.stopPropagation();"
                    )
            )
        ),
        # Remove the vertical padding from the nav card dropdown menu
        footer = tags$style(HTML(
            '[data-value="Nav Card"] ~ .dropdown-menu { --bs-dropdown-padding-y: 0; }'
        ))
    )
}

server <- function(input, output) {
    # These observers are broken because of the nested navset card
    observe(cli::cli_inform("{.strong main:} {input$main_nav}"))
    observe(cli::cli_inform("{.strong inner:} {input$inner_nav}"))
}

shinyApp(ui, server)
