globalVariables(names = c("id", "user", "cancel_button", "."))

## Support Functions ####

#' Function to get the user, or a mock user when the user isn't available.
#' @param session The shiny session object.
#' @param .mock_user The "default" username to use if one is not available in session$user.
#' @export
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
#'
cancel_button_creator <- function(index, .ns) {
  return(
    as.character(
      shiny::actionButton(
        glue::glue("cancel_button_{as.character(index)}"),
        glue::glue("Cancel"),
        row_index = index,
        onclick = paste0('Shiny.onInputChange(\"', .ns("cancel_button"), '\",  this.attributes.row_index.value)') ## When clicked, set input$cancel_button equal to the index. This is a detectable change.
      )
    )
  )
}

#' Function to create special, user-defined buttons to be rendered in a Data Table.
#' @param .button_id The ID to use for the button. Will not be namespaced to the module.
#' @param .button_label The label to display on the button.
#' @param index The index of the Data Table row that the button will be placed in.
any_button_creator <- function(.button_id, .button_label, index) {
  return(
    as.character(
      shiny::actionButton(
        glue::glue("{.button_id}_index_{as.character(index)}"),
        glue::glue("{.button_label}"),
        row_index = index,
        onclick = glue::glue('Shiny.onInputChange(\"{.button_id}\", this.attributes.row_index.value)') # Don't use .ns with button_id here because button_id is defined in the calling application. #input$
      )
    )
  )
}

#' Function to grab a dataframe of the jobs in the queue for the given user.
#' @param queue_obj The RSimpleQueue object currently in use.
#' @param .user The user as a character string -- only this user's jobs will be returned.
#' @importFrom dplyr filter arrange desc
#' @importFrom purrr is_null
get_queue_data <- function(queue_obj, .user) {
  return_value <- NULL

  jobs_df <- jobs_to_df(
    queue_obj$get_jobs()
  )

  if(!purrr::is_null(jobs_df)) {
    filtered_df <-
      dplyr::arrange(jobs_df, dplyr::desc(id)) %>%
      dplyr::filter(user == .user)
    if(nrow(filtered_df) > 0) {
      return_value <- filtered_df
    }
  }
  return(return_value)
}

## UI ####

#' queueViewOutput - Data Table displaying the jobs currently in an R Simple Queue.
#' @param id The Shiny ID to be associated with this object.
#' @importFrom DT dataTableOutput
#' @export
queueViewOutput <- function(id) {
  ns <- shiny::NS(id)
  shiny::tagList(
    DT::dataTableOutput(ns("queue_table"))
  )
}

## Server ####

#' queueView - Server-side function to call with callModule()
#' @param input Shiny input
#' @param output Shiny output
#' @param session Shiny session
#' @param user The current user of the shiny application. Used for filtering which jobs are visible.
#' @param queue_obj An RSimpleQueue object that has already been initialized with $new.
#' @param refresh_interval The frequency with which the UI should poll the queue and update, in milliseconds.
#' @param button_ids (Optional) A vector of Shiny ids (strings) to use for custom buttons added to the table.
#' @param button_labels (Optional) A vector of Shiny labels (strings) to use for custom buttons added to the table. Must correspond to button_ids.
#' @importFrom dplyr all_equal
#' @importFrom shiny reactiveValues invalidateLater
#' @importFrom purrr is_null
#' @export
queueView <- function(
  input,
  output,
  session,
  user,
  queue_obj = RSimpleQueue$new(),
  refresh_interval = 3000,
  button_ids = NULL,
  button_labels = NULL
) {

  # Grab the namespace function used in the UI object.
  # We will need this for creating the individual buttons in the list.
  ns <- session$ns

  # Initialize our data frame, "queue_data".
  # Pull the source data with a helper function, which converts it to a data frame and
  #   sorts the contents by ID (descending). We then assign this to as a component of a
  #   reactiveValue for easy-access in other reactive contexts.
  .rv <- shiny::reactiveValues()
  .rv$queue_data <- get_queue_data(queue_obj, user)

  # Initialize our return values, which we will use later to feed results from the
  #   module into the calling Shiny App.
  .return_values <- shiny::reactiveValues()
  shiny::observe(
    .return_values$rows <- .rv$queue_data
  )

  # Create an observe event that re-runs (via invalidation) every <refresh_interval> milliseconds.
  # In this section, we grab an updated version of our data frame and compare it
  #   to the one currently in use. If any differences are found, we upate the data
  #   frame in use. This prevents the UI from constantly refreshing every time it
  #   checks for new data.
  shiny::observe({
    shiny::invalidateLater(refresh_interval)
    new_data <- get_queue_data(queue_obj, user)

    if(purrr::is_null(.rv$queue_data) || purrr::is_null(new_data)) {
      .rv$queue_data <- new_data
    } else if (!isTRUE(dplyr::all_equal(.rv$queue_data, new_data))) {
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

      shiny::validate(
        shiny::need(nrow(.rv$queue_data) > 0, "There are no jobs in the queue for the current user.")
      )

      # cancel_button_creator <- function(index, .ns) {
      cancel_button_column <- purrr::map_chr(
        seq_len(nrow(.rv$queue_data)),
        cancel_button_creator,
        .ns = ns
      )

      #any_button_creator <- function(.button_id, .button_label, index, .ns) {
      other_button_columns <-
        purrr::map2(button_ids, button_labels, .f = function(.x, .y) {
          purrr::map_chr(
            seq_len(nrow(.rv$queue_data)),
            any_button_creator,
            # index = .
            .button_id = .x,
            .button_label = .y
      )}) %>% purrr::set_names(button_labels)

      .rv$queue_data %>%
        dplyr::mutate(cancel_button = cancel_button_column) %>%
        dplyr::select(cancel_button, dplyr::everything()) %>%
        dplyr::bind_cols(other_button_columns, .)

    },
    escape = FALSE,
    selection = "single"
  )

  # Create an eventReactive for when Cancel buttons are pushed.
  # This eventReactive triggers whenever a Cancel button is pushed. All Cancel buttons are
  #   treated as as if they had the id cancel_button (this is set up in
  #   cancel_button_creator). It saves the index of the row where the button was clicked.
  cancel_index <- shiny::eventReactive(input$cancel_button, {
    return(input$cancel_button)
  })

  # Submit the cancellation request
  # Whenever the cancel_index changes (after a cancel button is clicked), we get use the
  #   index to grab the relevant row from the table, from which we grab the job ID to be
  #   cancelled. We then use the queue_obj to submit the cancellation request.
  shiny::observe({
    # browser()
    index <- as.numeric(cancel_index())
    selected_row <- .rv$queue_data %>% dplyr::slice(index)
    queue_obj$cancel_job(selected_row$id)
    print(glue::glue("Cancelling:{selected_row$id}"))
  })

  # Create an eventReactive for when arow is selected.
  selected_index <- shiny::eventReactive(input$queue_table_rows_selected, {
    return(input$queue_table_rows_selected)
  })

  # When a row is clicked, update the .return_values$selected_row reactiveValue,
  # which we return to the calling application.
  shiny::observe({
    index <- selected_index()
    if(is.numeric(index)) {
      .return_values$highlighted_row <- .rv$queue_data %>% dplyr::slice(index)
    }
  })

  # Return all return values to the calling application.
  return(.return_values)
}

