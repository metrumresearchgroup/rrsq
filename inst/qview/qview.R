library(shiny)
library(tidyverse)
library(DT)

## Support Functions ####

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
        onclick = paste0('Shiny.onInputChange(\"', .ns("select_button"), '\",  this.id)')
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
#' @importFrom rrsq::RSimpleQueue
#' @export
queueView <- function(
    input,
    output,
    session,
    queue_obj = rrsq::RSimpleQueue$new(),
    .user
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

  # Create an observe event that re-runs (via invalidation) every 1000 milliseconds.
  # In this section, we grab an updated version of our data frame and compare it
  #   to the one currently in use. If any differences are found, we upate the data
  #   frame in use. This prevents the UI from constantly refreshing every time it
  #   checks for new data.
  observe({
    invalidateLater(1000)
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
      .rv$queue_data %>%
        mutate(cancel_button = map_chr(1:n(), cancel_button_creator, .ns = ns)) %>%
        dplyr::select(cancel_button, everything())
    },
    escape = FALSE
  )

  # Create an eventReactive for when Cancel buttons are pushed.
  # This eventReactive triggers whenever a Cancel button is pushed. All Cancel buttons are
  #   treated as as if they had the id ns("select_button") (this is set up in
  #   cancel_button_creator). It saves the index of the row where the button was clicked.
  selected_index <- eventReactive(input$select_button, {
    return(input$select_button)
  })

  # Submit the cancellation request
  # Whenever the selected_index changes (after a cancel button is clicked), we get use the
  #   index to grab the relevant row from the table, from which we grab the job ID to be
  #   cancelled. We then use the queue_obj to submit the cancellation request.
  observe({
    index <- as.numeric(selected_index())
    selected_row <- .rv$queue_data %>% slice(index)
    queue_obj$cancel_job(selected_row$id)
    print(glue::glue("Cancelling:{selected_row$id}"))
  })
}


## Testing ####

ui <- fluidPage(
  queueViewOutput("made_up_id")
)

server <- function(input, output, session) {
  print("server1")
  # browser()
  print(session$user)
  print("server2")
  whatDoesThisVariableDo <- callModule(
    queueView,
    "made_up_id",
    queue_obj = rrsq::RSimpleQueue$new(),
    .user = whoami(session = session, .mock_user = "Professor Pouch")
  )



}

shinyApp(ui = ui, server = server)
