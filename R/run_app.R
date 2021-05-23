#' Run the slotR Shiny Application
#'
#' @description slotR: a slot machine game of chance inspired by R.
#'
#' @param R NULL parameter to pass CMD check
#' 
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
run_app <- function(R) {
  with_golem_options(
    app = shinyApp(
      ui = app_ui, 
      server = app_server
    ), 
    golem_opts = list(R=NULL)
  )
}
