#' slot_game UI Function
#'
#' @description A shiny Module for playing slot machine with an R theme.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
slot_gameUI <- function(id){
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    # Application title
    fluidRow(
      h1("slot machine R", 
         style='background-color: #292c2f; color: #cd3700; text-align: center; padding-left: 15px'),
      DT::dataTableOutput(outputId = ns("reels")),
      br()
    ),
    br(),
    
    fluidRow(
      column(offset = 1, width = 3, style = "text-align: center",
             wellPanel(
             h3("CREDIT", style = "text-align:center; font-weight: bold"),
             br(),
             span(textOutput(outputId = ns("credit")), 
                  style = "color: blue;
                        background-color: lavender;
                        text-align:center;
                        font-size: 48px"),
             actionButton(inputId = ns("ask"), label = "ask for credit", 
                          icon = icon("dollar"), disabled = TRUE, width = "100%")
             
      )),
      column(offset = 1, width = 3, style = "text-align:center",
             wellPanel(
             h3("BET", style = "font-weight: bold"),
             br(),
             span(textOutput(outputId = ns("bet")), 
                  style = "color: red;
                        background-color: lavender;
                        text-align:center;
                        font-size: 48px"),
             fluidRow(
             actionButton(inputId = ns("betH"), label = "",
                          icon = icon("angle-up"), 
                          style = "align: center", width = "40%"),
             actionButton(inputId = ns("betL"), label = "", 
                          icon = icon("angle-down"), 
                          style = "align: center", width = "40%")
             ),
             actionButton(inputId = ns("all_in"), label = "all in",
                          icon = icon("angle-double-up"), 
                          style = "align: center; background-color: #ee4000", 
                          width = "85%")
            )),
      column(width = 4, style = "text-align:center",
             actionButton(inputId = ns("spin"), label = "SPIN R", 
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
                       border-width: 2px")
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
slot_game_server <- function(id, reel){
  moduleServer(id, 
               function(input, output, session){
  ns <- session$ns
 
  money <- reactiveValues()
  money$credit <- 3000
  money$bet <- 300
  money$debt <- 0
  
  
  game_data <- reactiveValues()
  game_data$tbl <- data.frame(credit = 3000 , bet = 0, 
                              reel1 = "-", reel2 = "-", reel3 = "-")
  
  
  reels <- reactive({
    if (!input$spin) {
      c("N", "N", "N")
    } else if (input$spin) {
      replicate(3, sample(reel(), size = 1))
      }
  })
  
  reels_tbl <- reactive({
    reels_tbl <- tibble::tibble(reel1 = symbols$img[symbols$id == reels()[1]], 
                        reel2 = symbols$img[symbols$id == reels()[2]], 
                        reel3 = symbols$img[symbols$id == reels()[3]])
  })
  
  
  observeEvent(input$ask, {
    if (money$debt == 0) {
      shinyjs::alert("you're getting into debt!")
      money$credit <- 1000
      money$debt <- money$debt + 1
      shinyjs::enable("spin")
      shinyjs::disable("ask")
      } else if (money$debt == 1){
      shinyjs::alert("you're getting into debt!")
      money$credit <- 500
      money$debt <- money$debt + 1
      shinyjs::enable("spin")
      shinyjs::disable("ask")
    } else if (money$debt == 2){
      shinyjs::alert("if you don't learn Python you can't get credit anymore!")
      shinyjs::disable("ask")
    }
  })
  
  observeEvent(input$betH, {
    if (money$bet < money$credit) {
      money$bet <- money$bet + 100 
    } else {
      shinyjs::alert("you cannot bet more than your actual credit!")
    }
    
  })
  
  observeEvent(input$betL, {
    if (money$bet > 100) {
      money$bet <- money$bet - 100 
    } else {
      shinyjs::alert(text = "you cannot bet less than 100!")
    }
    
  })
  
  observeEvent(input$all_in, {
    if (money$credit > 0) {
      money$bet <- money$credit 
    } else {
      shinyjs::alert(text = "no money, no game!")
    }
    
  })
  
  observeEvent(input$spin, {
    if (money$credit >= 50) {
      # read the pay table and update CREDIT
      x <- dplyr::pull(dplyr::filter(pay_tbl, 
                              reel1 == reels()[1], reel2 == reels()[2], reel3 == reels()[3]),
                       pay)
      pay <-  x * money$bet
      if ((money$credit + pay) < 0) {
        money$credit = 0
      } else {
        money$credit <- money$credit + pay
      }
      # collecting game data
      game_data$tbl <- rbind(game_data$tbl, 
                             data.frame(credit = money$credit,
                                        bet = money$bet,
                                        reel1 = reels()[1], 
                                        reel2 = reels()[2], 
                                        reel3 = reels()[3]))
      # set BET to 10% of credit
      money$bet <- round(money$credit * 0.1, -2)
      if (money$bet==0) {money$bet <- 100}
      } else {
      shinyjs::alert("no money, no game!")
      shinyjs::disable("spin")
      shinyjs::enable("ask")
    }
  })
  
  output$reels <- DT::renderDataTable(reels_tbl(), 
                                      options = list(dom = 't', 
                                                     columnDefs=list(list(targets=1:3, class="dt-center"))),
                                      escape = FALSE)
  
  output$credit <- shiny::renderText({money$credit})
  output$bet <- shiny::renderText({money$bet})
  
  return(reactive(game_data$tbl))
  
})}