#' The application User-Interface
#' 
#' @param request Internal parameter for `{shiny}`. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # List the first level UI elements here 
    tags$head(tags$style(type = 'text/css','.navbar { margin-bottom: 0px; }'),
              includeCSS(app_sys("app/www/footer_mdtds.css"))),
    fluidPage(
    navbarPage(title = "slotR: Slot Machine R", windowTitle = "slotR",
               inverse = TRUE, 
               footer = tagList(includeHTML(app_sys("app/www/include_footer.html")),
                         includeScript(path = app_sys("app/js/myscript.js"))),
      tabPanel("play", 
               slot_gameUI("slot_game_0")),
      tabPanel("config",
               slot_configUI("slot_config_0")),
      tabPanel("analyze",
               slot_analysisUI("slot_track_0")),
      tabPanel("how to",
               slot_howUI("slot_how_0"))
    )
  )
)}

#' Add external Resources to the Application
#' 
#' This function is internally used to add external 
#' resources inside the Shiny application. 
#' 
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function(){
  
  add_resource_path(
    'www', system.file('app/www', package = 'slotR')
  )
 
  tags$head(
      bundle_resources(
      path = app_sys('app/www'),
      app_title = 'slotR'
      )
  )

}

