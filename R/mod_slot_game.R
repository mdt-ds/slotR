#' slot_game UI Function
#'
#' @description A shiny Module for playing slot machine with an R theme.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
slot_gameUI <- function(id) {
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    shinyalert::useShinyalert(),
    # Application title
    fluidRow(
      h1("slot machine R",
         style = 'background-color: #292c2f; color: #cd3700; text-align: center; padding-left: 15px'),
      DT::dataTableOutput(outputId = ns("reels")),
      br()
    ),
    br(),
    
    fluidRow(
      column(
        offset = 1,
        width = 3,
        style = "text-align: center",
        wellPanel(
          h3("CREDIT", style = "text-align:center; font-weight: bold"),
          br(),
          span(id = "spanC",
            textOutput(outputId = ns("credit")),
            style = "color: blue;
                        background-color: lavender;
                        text-align:center;
                        font-size: 48px"
          ),
          actionButton(
            inputId = ns("ask"),
            label = "ask for credit",
            icon = icon("dollar"),
            disabled = TRUE,
            width = "100%"
          )
          
        )
      ),
      column(
        offset = 1,
        width = 3,
        style = "text-align:center",
        wellPanel(
          h3("BET", style = "font-weight: bold"),
          br(),
          span(id = "spanB",
            textOutput(outputId = ns("bet")),
            style = "color: red;
                        background-color: lavender;
                        text-align:center;
                        font-size: 48px"
          ),
          fluidRow(
            actionButton(
              inputId = ns("betH"),
              label = "",
              icon = icon("angle-up"),
              style = "align: center",
              width = "40%"
            ),
            actionButton(
              inputId = ns("betL"),
              label = "",
              icon = icon("angle-down"),
              style = "align: center",
              width = "40%"
            )
          ),
          actionButton(
            inputId = ns("all_in"),
            label = "all in",
            icon = icon("angle-double-up"),
            style = "align: center; background-color: #ee4000",
            width = "85%"
          )
        )
      ),
      column(
        width = 4,
        style = "text-align:center",
        actionButton(
          inputId = ns("spin"),
          label = "SPIN R",
          width = "100%",
          style = "color: white;
                       background-color: #CD2626;
                       font-family: 'Lucida Console';
                       font-size: large;
                       position: relative;
                       left: 3%;
                       height: 250px;
                       width: 70%;
                       text-align:center;
                       text-indent: -2px;
                       border-radius: 6px;
                       border-width: 2px"
        )
      )
    )
  )
}

#' slot_game Server Function
#'
#' @param reel get a character vector of slot symbols as reactive expression
#'
#' @return game_data table as reactive expression
#'  containing historic data from current game
#'
#' @noRd
slot_game_server <- function(id, reel) {
  moduleServer(id,
               function(input, output, session) {
                 ns <- session$ns
                 
                 money <- reactiveValues()
                 money$credit <- 300
                 money$bet <- 30
                 money$debt <- 0
                 
                 
                 game_data <- reactiveValues()
                 game_data$tbl <- data.frame(
                   credit = 300 ,
                   bet = 0,
                   reel1 = "-",
                   reel2 = "-",
                   reel3 = "-"
                 )
                 
                 set.seed(as.integer(Sys.time()))
                 reels <- eventReactive(input$spin, {
                   replicate(3, sample(reel(), size = 1))
                 }, ignoreNULL = FALSE)
                 
                 reels_tbl <- reactive({
                   reels_tbl <-
                     tibble::tibble(
                       reel1 = symbols$img[symbols$id == reels()[1]],
                       reel2 = symbols$img[symbols$id == reels()[2]],
                       reel3 = symbols$img[symbols$id == reels()[3]]
                     )
                 })
                 
                 
                 observeEvent(input$ask, {
                   if (money$debt == 0) {
                     shinyalert::shinyalert(
                       title = "debt alert",
                       text = "you're getting into debt \n for the first time!",
                       type = "warning",
                       size = "xs"
                     )
                     money$credit <- 100
                     money$debt <- money$debt + 1
                     shinyjs::enable("spin")
                     shinyjs::disable("ask")
                   } else if (money$debt == 1) {
                     shinyalert::shinyalert(
                       title = "debt alert",
                       text = "you're getting into debt \n for the second time!",
                       type = "warning",
                       size = "xs"
                     )
                     money$credit <- 50
                     money$debt <- money$debt + 1
                     shinyjs::enable("spin")
                     shinyjs::disable("ask")
                   } else if (money$debt == 2) {
                     shinyalert::shinyalert(
                       title = "no more credit",
                       text = "if you don't learn Python \n you can't get credit anymore!",
                       type = "error", 
                       size = "xs"
                     )
                     shinyjs::disable("ask")
                     shinyjs::disable("all_in")
                     shinyjs::disable("betH")
                     shinyjs::disable("betL")
                   }
                 })
                 
                 observeEvent(input$betH, {
                   if (money$bet < money$credit) {
                     money$bet <- money$bet + 10
                   } else {
                     shinyalert::shinyalert(
                       title = "bet upper limit",
                       text = "you cannot bet more than your actual credit",
                       type = "warning",
                       size = "xs"
                     )
                   }
                 })
                 
                 observeEvent(input$betL, {
                   if (money$bet > 10) {
                     money$bet <- money$bet - 10
                   } else {
                     shinyalert::shinyalert(
                       title = "bet lower limit",
                       text = "you cannot bet less than 10",
                       type = "warning",
                       size = "xs"
                     )
                   }
                 })
                 
                 observeEvent(input$all_in, {
                   if (money$credit > 0) {
                     money$bet <- money$credit
                   } else {
                     shinyalert::shinyalert(
                       title = "no money to bet",
                       text = "ask for credit!",
                       type = "warning",
                       size = "xs"
                     )
                   }
                 })
                 
                 observeEvent(input$spin, {
                   if (money$credit >= 10) {
                     # read the pay table and update CREDIT
                     x <- dplyr::pull(
                       dplyr::filter(
                         pay_tbl,
                         reel1 == reels()[1],
                         reel2 == reels()[2],
                         reel3 == reels()[3]
                       ),
                       pay
                     )
                     pay <-  x * money$bet
                     if ((money$credit + pay) <= 0) {
                       money$credit = 0
                       shinyalert::shinyalert(title = "no money, \n no game",
                                              type = "warning",
                                              size = "xs")
                       shinyjs::disable("spin")
                       shinyjs::enable("ask")
                     } else {
                       money$credit <- money$credit + pay
                       if (money$credit >= 30000) {
                         shinyalert::shinyalert(title = "you win",
                                                text = "you aRe so lucky! \n or you aRe setting probabilities right ...", 
                                                type = "success", 
                                                imageUrl = "www/R_logo.png",
                                                size = "xs")
                         shinyjs::disable("spin")
                         shinyjs::disable("ask")
                         shinyjs::disable("all_in")
                         shinyjs::disable("betH")
                         shinyjs::disable("betL")
                       }
                     }
                     # collecting game data
                     game_data$tbl <- rbind(
                       game_data$tbl,
                       data.frame(
                         credit = money$credit,
                         bet = money$bet,
                         reel1 = reels()[1],
                         reel2 = reels()[2],
                         reel3 = reels()[3]
                       )
                     )
                     # set BET to 10% of credit
                     money$bet <- round(money$credit * 0.1,-2)
                     if (money$bet == 0) {
                       money$bet <- 10
                     }
                   }
                 })
                 
                 output$reels <- DT::renderDataTable(reels_tbl(),
                                                     options = list(dom = 't',
                                                                    columnDefs = list(
                                                                      list(targets = 1:3, class = "dt-center")
                                                                    )),
                                                     escape = FALSE)
                 
                 output$credit <- shiny::renderText({
                   money$credit
                 })
                 output$bet <- shiny::renderText({
                   money$bet
                 })
                 
                 return(reactive(game_data$tbl))
                 
               })
}
