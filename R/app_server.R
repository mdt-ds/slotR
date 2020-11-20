#' The application server-side
#' 
#' @param input,output,session Internal parameters for {shiny}. 
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function( input, output, session ) {
  # List the first level callModules here
  reel <- slot_config_server(id = "slot_config_0")
  game_tbl <- slot_game_server(id = "slot_game_0", reel = reel)
  slot_analysis_server(id = "slot_track_0", game_tbl)
  slot_how_server(id = "slot_how_0")
}
