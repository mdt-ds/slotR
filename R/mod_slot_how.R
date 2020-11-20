#' slot_how UI Function
#'
#' @description A shiny Module for providing minimal instruction.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd 
#'
#' @importFrom shiny NS tagList 
slot_howUI <- function(id){
  ns <- NS(id)
  tagList(
    fluidRow(
    tags$h1("slotR: how to", 
            style='background-color: #292c2f; color: #cd3700; text-align: center; padding-left: 15px'),
    ),
    # tabset
      fluidRow(tabsetPanel(
        tabPanel(title = "how to ...",
                 includeMarkdown(
                   system.file(
                     "app/md/howto.md", 
                     package = "slotR" 
                   )
        )),
        tabPanel(title = "payout table",
                 DT::dataTableOutput(outputId = ns("payout"))
                  ),
        tabPanel(title = "about",
                 includeMarkdown(
                   system.file(
                     "app/md/about.md", 
                     package = "slotR" 
                   )
                 )
      ))
  )
)}

#' slot_how Server Function
#'
#' @noRd 
slot_how_server <- function(id){
  moduleServer(id, 
               function(input, output, session){
                 ns <- session$ns
  
  # display the pay table
  output$payout <- DT::renderDataTable(payout, 
                          options = list(dom = 't', 
                                         columnDefs=list(list(targets=1, class="dt-left"))),
                          escape = FALSE)

})}
    

