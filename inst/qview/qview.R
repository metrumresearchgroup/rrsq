#' queueViewOutput
#' @export
queueViewOutput <- function(id) {
  ns <- NS(id)
  tableOutput(ns("queue"))
}


#' queueView
#' @export
queueView <- function(input, output, session) {
  output$queue <- renderTable(
    {
      queueObj <- rrsq::RSimpleQueue$new()
      # Check null, error, etc.

      return(
        rrsq::jobs_to_df(queueObj$get_jobs())
      )
    }
  )
}
