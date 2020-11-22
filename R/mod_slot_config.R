#' slot_config UI Function
#'
#' @description A shiny Module used to configure the slot reels
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
slot_configUI <- function(id) {
  ns <- NS(id)
  tagList(fluidRow(
    tags$h1("inside slotR",
            style = 'background-color: #292c2f; color: #cd3700; text-align: center; padding-left: 15px')
  ),
  # configuration
  fluidRow(sidebarLayout(
    sidebarPanel(
      sliderInput(
        ns("Rs"),
        label = tags$img(src = "www/R_logo.png", width = "32px"),
        min = 1,
        max = 3,
        value = 1,
        step = 1
      ),
      sliderInput(
        ns("Ps"),
        label = tags$img(src = "www/Python_logo.png", width = "32px"),
        min = 1,
        max = 3,
        value = 1,
        step = 1
      ),
      sliderInput(
        ns("hex"),
        label = tags$img(src = "www/hexes.png", width = "118px"),
        min = 1,
        max = 5,
        value = 3,
        step = 1
      ),
      sliderInput(
        ns("NAs"),
        label = tags$img(src = "www/hex-NA.png", width = "32px"),
        min = 1,
        max = 5,
        value = 4,
        step = 1
      )
    ),
    mainPanel(
      tags$h4("slotR probabilities"),
      DT::dataTableOutput(outputId = ns("probs")),
      br(),
      fluidRow(
        actionButton(
          inputId = ns("configuredReel"),
          label = "configured reel",
          icon = icon("cog")
        ),
        actionButton(
          inputId = ns("chgReel"),
          label = "change the reel",
          icon = icon("sliders-h")
        )
      )
    )
  )))
}

#' slot_config Server Function
#'
#' @return reel reactive character vector with as many stops as configured
#'
#' @noRd
slot_config_server <- function(id) {
  moduleServer(id,
               function(input, output, session) {
                 ns <- session$ns
                 # probs reactive
                 reel_stops <- reactive({
                   (input$Rs + input$Ps + input$NAs + 4 * input$hex)
                 })
                 slot_combos <- reactive({
                   reel_stops() * reel_stops() * reel_stops()
                 })
                 probs_tbl <- reactive({
                   ptbl <-
                     tibble::tibble(
                       symbol = c(
                         '<img src = "www/R_logo.png" width = "32px"></img>',
                         '<img src = "www/Python_logo.png" width = "32px"></img>',
                         '<img src = "www/hex-analysis.png" width = "32px"></img>',
                         '<img src = "www/hex-insight.png" width = "32px"></img>',
                         '<img src = "www/hex-package.png" width = "32px"></img>',
                         '<img src = "www/hex-shiny_app.png" width = "32px"></img>',
                         '<img src = "www/hex-NA.png" width = "32px"></img>'
                       ),
                       x0 = c((reel_stops() - input$Rs) ^ 3 / slot_combos(),
                              (reel_stops() - input$Ps) ^ 3 / slot_combos(),
                              (reel_stops() - input$hex) ^ 3 / slot_combos(),
                              (reel_stops() - input$hex) ^ 3 / slot_combos(),
                              (reel_stops() - input$hex) ^ 3 / slot_combos(),
                              (reel_stops() - input$hex) ^ 3 / slot_combos(),
                              (reel_stops() - input$NAs) ^ 3 / slot_combos()
                       ),
                       x1 = c(
                         input$Rs * (reel_stops() - input$Rs) ^ 2 / slot_combos(),
                         input$Ps * (reel_stops() - input$Ps) ^
                           2 / slot_combos(),
                         input$hex * (reel_stops() - input$hex) ^
                           2 / slot_combos(),
                         input$hex * (reel_stops() - input$hex) ^
                           2 / slot_combos(),
                         input$hex * (reel_stops() - input$hex) ^
                           2 / slot_combos(),
                         input$hex * (reel_stops() - input$hex) ^
                           2 / slot_combos(),
                         input$hex * (reel_stops() - input$NAs) ^
                           2 / slot_combos()
                       ),
                       x2 = c(
                         input$Rs ^ 2 * (reel_stops() - input$Rs) / slot_combos(),
                         input$Ps ^ 2 * (reel_stops() - input$Ps) /
                           slot_combos(),
                         input$hex ^ 2 * (reel_stops() - input$hex) /
                           slot_combos(),
                         input$hex ^ 2 * (reel_stops() - input$hex) /
                           slot_combos(),
                         input$hex ^ 2 * (reel_stops() - input$hex) /
                           slot_combos(),
                         input$hex ^ 2 * (reel_stops() - input$hex) /
                           slot_combos(),
                         input$NAs ^ 2 * (reel_stops() - input$NAs) /
                           slot_combos()
                       ),
                       x3 = c(
                         input$Rs ^ 3 / slot_combos(),
                         input$Ps ^ 3 / slot_combos(),
                         input$hex ^ 3 / slot_combos(),
                         input$hex ^ 3 / slot_combos(),
                         input$hex ^ 3 / slot_combos(),
                         input$hex ^ 3 / slot_combos(),
                         input$NAs ^ 3 / slot_combos()
                       )
                     )
                   dplyr::mutate_if(ptbl, .predicate = is.numeric, .funs = round, 4)
                 })
                 
                 # show probabilities
                 output$probs <- DT::renderDataTable(probs_tbl(),
                                                     options = list(dom = 't',
                                                                    columnDefs = list(
                                                                      list(targets = "_all", class = "dt-center")
                                                                    )),
                                                     escape = FALSE)
                 
                 
                 # store and return configured reel
                 reel <- eventReactive(input$chgReel, {
                   rep(
                     x = c("R", "I", "A", "L", "S", "N", "P"),
                     times = c(
                       input$Rs,
                       input$hex,
                       input$hex,
                       input$hex,
                       input$hex,
                       input$NAs,
                       input$Ps
                     )
                   )
                 }, ignoreNULL = FALSE)
                 
                 observeEvent(input$configuredReel, {
                   reel_display <-
                     tibble::tibble(
                       symbol = c(
                         '<img src = "www/R_logo.png" width = "32px"></img>',
                         '<img src = "www/Python_logo.png" width = "32px"></img>',
                         '<img src = "www/hex-analysis.png" width = "32px"></img>',
                         '<img src = "www/hex-insight.png" width = "32px"></img>',
                         '<img src = "www/hex-package.png" width = "32px"></img>',
                         '<img src = "www/hex-shiny_app.png" width = "32px"></img>',
                         '<img src = "www/hex-NA.png" width = "32px"></img>'
                       ),
                       stops = c(
                         sum(stringr::str_count(
                           string = reel(), pattern = "R"
                         )),
                         sum(stringr::str_count(
                           string = reel(), pattern = "P"
                         )),
                         sum(stringr::str_count(
                           string = reel(), pattern = "A"
                         )),
                         sum(stringr::str_count(
                           string = reel(), pattern = "I"
                         )),
                         sum(stringr::str_count(
                           string = reel(), pattern = "L"
                         )),
                         sum(stringr::str_count(
                           string = reel(), pattern = "S"
                         )),
                         sum(stringr::str_count(
                           string = reel(), pattern = "N"
                         ))
                       )
                     )
                   
                   showModal(
                     modalDialog(
                       title = "current reel configuration",
                       footer = modalButton("OK"),
                       helpText("all 3 reels now have the following quantities of stops."),
                       DT::renderDataTable(
                         reel_display,
                         options = list(dom = 't',
                                        columnDefs =
                                          list(
                                            list(targets = 2, class = "dt-center")
                                          )),
                         escape = FALSE
                       )
                     )
                   )
                 })
                 
                 observeEvent(input$chgReel, {
                   reel_display <-
                     tibble::tibble(
                       symbol = c(
                         '<img src = "www/R_logo.png" width = "32px"></img>',
                         '<img src = "www/Python_logo.png" width = "32px"></img>',
                         '<img src = "www/hex-analysis.png" width = "32px"></img>',
                         '<img src = "www/hex-insight.png" width = "32px"></img>',
                         '<img src = "www/hex-package.png" width = "32px"></img>',
                         '<img src = "www/hex-shiny_app.png" width = "32px"></img>',
                         '<img src = "www/hex-NA.png" width = "32px"></img>'
                       ),
                       stops = c(
                         input$Rs,
                         input$Ps,
                         input$hex,
                         input$hex,
                         input$hex,
                         input$hex,
                         input$NAs
                       )
                     )
                   
                   showModal(
                     modalDialog(
                       title = "reel configuration changed", 
                       footer = modalButton("OK"),
                       helpText("all 3 reels now have the following quantities of stops."),
                       DT::renderDataTable(
                         reel_display,
                         options = list(dom = 't',
                                        columnDefs =
                                          list(
                                            list(targets = 2, class = "dt-center")
                                          )),
                         escape = FALSE
                       )
                     )
                   )
                 })
                 
                 output$reel_txt <- renderText(paste(reel(), "-"))
                 
                 return(reel)
               })
}
