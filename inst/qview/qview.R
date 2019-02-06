library(shiny)
library(tidyverse)
library(DT)

action_button_creator <- function(index, .ns) {
  return(
    as.character(
      actionButton(
        as.character(index),
        glue::glue("index{index}"),
        # onclick = 'Shiny.onInputChange(\"select_button\",  this.id)'
        onclick = paste0('Shiny.onInputChange(\"', .ns("select_button"), '\",  this.id)')
      )
    )
  )
}


######## UI ########

#' queueViewOutput
#' @export
queueViewOutput <- function(id) {

  ns <- NS(id)
  tagList(
    textOutput(ns("message")),
    actionButton(ns("sampleButton"),"Debug Now"),
    DT::dataTableOutput(ns("queue_table"))
  )
}


###`~~~~~~~~~~~~~~~~~~~~` SERVER ########

#' queueView
#' @export
queueView <- function(input, output, session, queue_obj = rrsq::RSimpleQueue$new()) {

  ns <- session$ns

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
      .rv$queue_data %>%
        mutate(cancel_button = map_chr(1:n(), action_button_creator, .ns = ns)) %>%
        dplyr::select(cancel_button, everything())
    },
    escape = FALSE
  )

  selected_index <- eventReactive(input$select_button, {
    return(input$select_button)
  })

  output$message <- renderText({
    index <- as.numeric(selected_index())
    selected_row <- .rv$queue_data %>% slice(index)
    return(glue::glue("Output Message: {selected_row$id}"))
  })

  observeEvent(selected_index, {
    index <- as.numeric(selected_index())
    selected_row <- .rv$queue_data %>% slice(index)
    # browser()
    print("We made it, boys.")
    # queue_obj$cancel_job(selected_row$id)
  })

  observeEvent(input$sampleButton, {
    print("Debugging.")
    browser()
  })
}


## Testing ####



ui <- fluidPage(
  queueViewOutput("id_i_provided")


)

server <- function(input, output, session) {
  whatDoesThisVariableDo <- callModule(queueView, "id_i_provided", queue_obj = rrsq::RSimpleQueue$new())
}

shinyApp(ui = ui, server = server)
