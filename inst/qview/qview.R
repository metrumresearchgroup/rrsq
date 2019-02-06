library(shiny)
library(tidyverse)
library(DT)

action_button_creator <- function(index) {
  return(
    as.character(
      actionButton(
        as.character(index),
        glue::glue("index{index}"),
        onclick = 'Shiny.onInputChange(\"select_button\",  this.id)'
      )
    )
  )
}

#' queueViewOutput
#' @export
queueViewOutput <- function(id) {

  ns <- NS(id)

  DT::dataTableOutput(ns("queue_table"))

}

#' queueView
#' @export
queueView <- function(input, output, session, queue_obj = rrsq::RSimpleQueue$new()) {
  queue_obj = rrsq::RSimpleQueue$new()

  .rv <- reactiveValues()
  .rv$queue_data <- rrsq::jobs_to_df(queue_obj$get_jobs()) %>% dplyr::arrange(desc(id))

  observe({
    invalidateLater(1000)
    new_data <- rrsq::jobs_to_df(queue_obj$get_jobs()) %>% dplyr::arrange(desc(id))

    if (
      !isTRUE(all_equal(.rv$queue_data, new_data))
      ) {
      .rv$queue_data <- new_data
    }
  })

  output$queue_table  <- DT::renderDT(
    {
      .rv$queue_data %>% mutate(button = map_chr(1:n(), action_button_creator))
    },
    escape = FALSE
  )

  selected_index <- eventReactive(input$select_button, {
    input$select_button
  })

  observeEvent(selected_index, {
    index <- as.numeric(selected_index())
    selected_row <- .rv$queue_data() %>% slice(index)
    queue_obj$cancel_job(selected_row$id)
  })

  output$cancel_confirmation <- renderText({
    index <- as.numeric(selected_index())
    selected_row <- .rv$queue_data() %>% slice(index)
    return(glue::glue("cancelling: {selected_row$id}"))
  })
}


## Testing ####



ui <- fluidPage(
  queueViewOutput("q"),

  textOutput("cancel_confirmation")
)

server <- function(input, output, session) {
  whatDoesThisVariableDo <- callModule(queueView, "q", queue_obj = rrsq::RSimpleQueue$new())
}

shinyApp(ui = ui, server = server)
