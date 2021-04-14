#' slot_analysis UI Function
#'
#' @description A shiny Module for analyzing the history of your current game.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
slot_analysisUI <- function(id) {
  ns <- NS(id)
  tagList(fluidRow(
    tags$h1("slotR: analyzing your game",
            style = 'background-color: #292c2f; color: #cd3700; text-align: center; padding-left: 15px')
  ),
  # analysis
  fluidRow(tabsetPanel(
    tabPanel(title = "credit trend",
             plotOutput(outputId = ns("credit_plot"))),
    tabPanel(title = "game data",
             DT::dataTableOutput(outputId = ns("game_data")))
  )))
}

#' slot_analysis Server Function
#'
#' @param game_data get a game data table as a reactive expression
#'
#' @noRd
#'
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
slot_analysis_server <- function(id, game_data) {
  moduleServer(id,
               function(input, output, session) {
                 ns <- session$ns
                 
                 game_tbl <- reactive(
                   game_data() %>%
                     dplyr::mutate(dplyr::across(
                       dplyr::starts_with("reel"),
                       ~ stringr::str_replace(
                         string = .x,
                         pattern = "R",
                         replacement = '<img src = "www/R_logo.png" width = "32px"></img>'
                       )
                     )) %>%
                     dplyr::mutate(dplyr::across(
                       dplyr::starts_with("reel"),
                       ~ stringr::str_replace(
                         string = .x,
                         pattern = "P",
                         replacement = '<img src = "www/Python_logo.png" width = "32px"></img>'
                       )
                     )) %>%
                     dplyr::mutate(dplyr::across(
                       dplyr::starts_with("reel"),
                       ~ stringr::str_replace(
                         string = .x,
                         pattern = "A",
                         replacement = '<img src = "www/hex-analysis.png" width = "32px"></img>'
                       )
                     )) %>%
                     dplyr::mutate(dplyr::across(
                       dplyr::starts_with("reel"),
                       ~ stringr::str_replace(
                         string = .x,
                         pattern = "I",
                         replacement = '<img src = "www/hex-insight.png" width = "32px"></img>'
                       )
                     )) %>%
                     dplyr::mutate(dplyr::across(
                       dplyr::starts_with("reel"),
                       ~ stringr::str_replace(
                         string = .x,
                         pattern = "L",
                         replacement = '<img src = "www/hex-package.png" width = "32px"></img>'
                       )
                     )) %>%
                     dplyr::mutate(dplyr::across(
                       dplyr::starts_with("reel"),
                       ~ stringr::str_replace(
                         string = .x,
                         pattern = "S",
                         replacement = '<img src = "www/hex-shiny_app.png" width = "32px"></img>'
                       )
                     )) %>%
                     dplyr::mutate(dplyr::across(
                       dplyr::starts_with("reel"),
                       ~ stringr::str_replace(
                         string = .x,
                         pattern = "N",
                         replacement = '<img src = "www/hex-NA.png" width = "32px"></img>'
                       )
                     ))
                 )
                 
                 output$game_data <- DT::renderDataTable(game_tbl(),
                                                         options = list(dom = 'p',
                                                                        columnDefs = list(
                                                                          list(targets = 1:5, class = "dt-center")
                                                                        )),
                                                         escape = FALSE)
                 
                 output$credit_plot <- renderPlot({
                   ggplot2::ggplot(game_data(),
                                   ggplot2::aes(x = 0:(nrow(game_data(
                                   )) - 1), y = credit)) +
                     ggplot2::geom_line(size = 1, color = "darkred") +
                     ggplot2::ggtitle("your credit trend") +
                     ggplot2::xlab("game round") +
                     ggplot2::ylab("") +
                     ggplot2::theme_dark()
                 })
               })
}
