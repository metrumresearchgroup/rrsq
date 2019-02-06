library(shiny)
library(tidyverse)
library(DT)

action_button_creator <- function(index) {
  return(
    as.character(
      actionButton(
        as.character(index),
        glue::glue("Cancel Job"),
        onclick = 'Shiny.onInputChange(\"select_button\",  this.id)'
      )
    )
  )
}

#' queueViewOutput
#' @export
queueViewOutput <- function(id) {

  ns <- NS(id)
  tagList(
    DT::dataTableOutput(ns("queue_table")),
    textOutput(ns("cancel_confirmation"))
  )
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
      .rv$queue_data %>% mutate(cancel_button = map_chr(1:n(), action_button_creator)) %>% dplyr::select(cancel_button, everything())
    },
    escape = FALSE
  )

  selected_index <- eventReactive(input$select_button, {
    # browser()
    input$select_button$id
  })

  observeEvent(selected_index, {
    index <- as.numeric(selected_index())
    selected_row <- .rv$queue_data() %>% slice(index)
    queue_obj$cancel_job(selected_row$id)
  })

  output$cancel_confirmation <- renderText({
    index <- as.numeric(selected_index())
    # selected_row <- .rv$queue_data() %>% slice(index)
    # return(glue::glue("cancelling: {selected_row$id}"))
    return(glue::glue("message{lubridate::now()}!"))
  })
}


## Testing ####



ui <- fluidPage(
  queueViewOutput("q")


)

server <- function(input, output, session) {
  whatDoesThisVariableDo <- callModule(queueView, "q", queue_obj = rrsq::RSimpleQueue$new())
}

shinyApp(ui = ui, server = server)
