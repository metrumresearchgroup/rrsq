library(shiny)
library(tidyverse)
library(DT)


#' queueViewOutput
#' @export
queueViewOutput <- function(id) {
  ns <- NS(id)
  tableOutput(ns("queue"))
}

#' #' queueButtonInput
#' #' @export
#' queueButtonInput <- function(id, label, job_id) {
#'   ns <- NS(id)
#'   actionButton(
#'     inputId = ns(id),
#'     label = label,
#'     icon = icon("stop-circle"),
#'     job_id = job_id
#'   )
#' }

#' queueView
#' @export
queueView <- function(input, output, session, queue_obj = rrsq::RSimpleQueue$new()) {
  output$queue <- renderTable(
    {
      invalidateLater(1000, session)

      queueViewFrame <- rrsq::jobs_to_df(queue_obj$get_jobs())

      return(
        queueViewFrame
      )
    }
  )
}


## Testing ####

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

ui <- fluidPage(
  queueViewOutput("q"),
  DT::dataTableOutput("test_table"),
  textOutput("cancel_confirmation")
)

server <- function(input, output, session) {
  # whatDoesThisVariableDo <- callModule(queueView, "q", queue_obj = rrsq::RSimpleQueue$new())

  queue_obj = rrsq::RSimpleQueue$new()

  queue_data <- reactive({
    #tibble(jobID = c("asdfasdf", "weqwerqwerqwer"))
    rrsq::jobs_to_df(queue_obj$get_jobs())
  })

  output$test_table  <- DT::renderDT(
    {
      queue_data() %>% mutate(button = map_chr(1:n(), action_button_creator))
    },
    escape = FALSE
  )

  selected_index <- eventReactive(input$select_button, {
    input$select_button
  })

  output$cancel_confirmation <- renderText({
    index <- as.numeric(selected_index())
    selected_row <- queue_data() %>% slice(index)
    return(glue::glue("cancelling: {selected_row$id}"))
  })


}

shinyApp(ui = ui, server = server)
