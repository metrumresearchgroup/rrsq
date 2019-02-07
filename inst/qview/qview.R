library(shiny)
library(tidyverse)
library(DT)

## Support Functions ####

#' Function to get the user, or a mock user when the user isn't available.
whoami <- function(session, .mock_user = "UNKNOWN") {
  if (!is.null(session$user)) {
    return(session$user)
  }
  return(.mock_user)
}

#' Function to create a cancel button to be rendered in a Data Table.
#' @param index The index of the Data Table row that the button will be placed in.
#' @param .ns The namespace function that should be applied to the button's ID.
#' @details
#' The function returns the cancel button as raw HTML with a JavaScript statement
#' embedded. Using raw HTML + JavaScript allows us to embed the button directly into
#' a DataTable object.
cancel_button_creator <- function(index, .ns) {
  return(
    as.character(
      actionButton(
        as.character(index),
        glue::glue("Cancel"),
        onclick = paste0('Shiny.onInputChange(\"', .ns("cancel_button"), '\",  this.id)')
      )
    )
  )
}

## UI ####

#' queueViewOutput - Data Table displaying the jobs currently in an R Simple Queue.
#' @param id The Shiny ID to be associated with this object.
#' @importFrom DT dataTableOutput
#' @export
queueViewOutput <- function(id) {
  ns <- NS(id)
  tagList(
    DT::dataTableOutput(ns("queue_table"))
  )
}

## Server ####

#' queueView - Server-side function to call with callModule()
#' @param input Shiny input
#' @param output Shiny output
#' @param session Shiny session
#' @param queue_obj An rrsq::RSimpleQueue object that has already been initialized with $new.
#' @param refresh_interval The frequency with which the UI should poll the queue and update, in milliseconds.
#' @importFrom rrsq::RSimpleQueue
#' @export
queueView <- function(
    input,
    output,
    session,
    queue_obj = rrsq::RSimpleQueue$new(),
    .user,
    .refresh_interval = 3000
  ) {

  # Grab the namespace function used in the UI object.
  # We will need this for creating the individual buttons in the list.
  ns <- session$ns

  print("Starting.")
  print(glue::glue("User: {.user}"))
  # browser()

  # Get an RSQ object, which contains the methods needed for interacting with the Queue.
  queue_obj = rrsq::RSimpleQueue$new()

  # Initialize our data frame, "queue_data".
  # We pull the source data with queue_obj$get_jobs, then convert it to a data frame and
  #   sort the contents by ID (descending). We then assign this to as a component of a
  #   reactiveValue for easy-access in other reactive contexts.
  .rv <- reactiveValues()
  .rv$queue_data <-
    rrsq::jobs_to_df(queue_obj$get_jobs()) %>%
    dplyr::filter(user == .user) %>%
    dplyr::arrange(desc(id))

  .return_values <- reactiveValues()

  # Create an observe event that re-runs (via invalidation) every 1000 milliseconds.
  # In this section, we grab an updated version of our data frame and compare it
  #   to the one currently in use. If any differences are found, we upate the data
  #   frame in use. This prevents the UI from constantly refreshing every time it
  #   checks for new data.
  observe({
    invalidateLater(.refresh_interval)
    new_data <-
      rrsq::jobs_to_df(queue_obj$get_jobs()) %>%
      dplyr::filter(user == .user) %>%
      dplyr::arrange(desc(id))

    if (
      !isTRUE(all_equal(.rv$queue_data, new_data))
      ) {
      .rv$queue_data <- new_data
    }
  })

  # Create the DataTable (DT) object to be rendered.
  #   Using the data in our reactiveValue (.rv$queue_data), we render a Data Table object.
  #   While doing so, we add a column of buttons to the Data Table with the help of the
  #   function "cancel_button_creator". "cancel_button_creator" needs our namespacing
  #   function in order to properly namespace the buttons with this module's UI namespace.
  output$queue_table  <- DT::renderDT(
    {

      if(nrow(.rv$queue_data) > 0) {
        cancel_button_column <- map_chr(
          1:nrow(.rv$queue_data),
          cancel_button_creator,
          .ns = ns
        )
      } else {
        cancel_button_column <- list()
      }

      .rv$queue_data %>%
        mutate(cancel_button = cancel_button_column) %>%
        dplyr::select(cancel_button, everything())
    },
    escape = FALSE,
    selection = "single"
  )



  # Create an eventReactive for when Cancel buttons are pushed.
  # This eventReactive triggers whenever a Cancel button is pushed. All Cancel buttons are
  #   treated as as if they had the id ns("cancel_button") (this is set up in
  #   cancel_button_creator). It saves the index of the row where the button was clicked.
  cancel_index <- eventReactive(input$cancel_button, {
    return(input$cancel_button)
  })

  # Submit the cancellation request
  # Whenever the cancel_index changes (after a cancel button is clicked), we get use the
  #   index to grab the relevant row from the table, from which we grab the job ID to be
  #   cancelled. We then use the queue_obj to submit the cancellation request.
  observe({
    index <- as.numeric(cancel_index())
    selected_row <- .rv$queue_data %>% slice(index)
    queue_obj$cancel_job(selected_row$id)
    print(glue::glue("Cancelling:{selected_row$id}"))
  })

  # Create an eventReactive for when arow is selected.
  selected_index <- eventReactive(input$queue_table_rows_selected, {
    return(input$queue_table_rows_selected)
  })

  # When a row is clicked, update the .return_values$selected_row reactiveValue,
  # which we return to the calling application.
  observe({
    index <- selected_index()
    if(is.numeric(index)) {
      .return_values$selected_row <- .rv$queue_data %>% slice(index)
    }
  })

  # Return all return values to the calling application.
  return(.return_values)
}


## Testing ####

ui <- fluidPage(
  tableOutput("row"),
  queueViewOutput("made_up_id")

)

server <- function(input, output, session) {
  returns <- callModule(
    queueView,
    "made_up_id",
    queue_obj = rrsq::RSimpleQueue$new(),
    #.user = whoami(session = session, .mock_user = "Professor Pouch")
    .user = whoami(session = session, .mock_user = "Sheersa")
  )

  output$row <- renderTable(returns$selected_row)
}

shinyApp(ui = ui, server = server)
