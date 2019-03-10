
<!-- README.md is generated from README.Rmd. Please edit that file -->

# RRSQ

#### A simple interface for interacting with an [R Simple Queue](https://github.com/metrumresearchgroup/rsq) using R.

## Getting Started

Note: This guide assumes you already have an
[RSQ](https://github.com/metrumresearchgroup/rsq) instance running
locally on your computer.

Simply install the package and create an RSimpleQueue object to get
rolling.

``` r
devtools::install_github("https://github.com/metrumresearchgroup/rrsq")

queue <- RSimpleQueue$new()
```

From here, you can use several functions to interact with the queue.

``` r
## Submits a job to be run on the queue.
jobject1 <- queue$submit_job(
  user = "Dreznel",
  context = "Some job context",
  work_dir = getwd(),
  rscript_path = system.file("inst/qview/long_sample.R", package = "rrsq")
)

## Retrieves a single job with the given ID from the queue.
queue$get_job(jobject1$id)

## Retrieves all jobs from the queue. Combine with rrsq::jobs_to_df(queue$get_jobs()) to get a dataframe of the jobs.
queue$get_jobs()

## Cancels the job with the given ID, assuming its status is "QUEUED" when the request is sent.
queue$cancel_job(jobject1$id) 
```

## Environment variables

Secrets or other information set as environment variables can be passed
via the `.envs` arg for the `submit_job` method.

``` r
rsq$submit_job(
        ...,
        .envs = c("<example_env_name>")
      )
```

On your machine setting environment variables to secret values, or if
they are set in the Renviron can be captured by name and passed to the
execution context

    jobject1 <- queue$submit_job(
      user = "devin",
      context = "super secret job",
      work_dir = getwd(),
      rscript_path = "script_with_secrets.R",
      .envs = c("ENV_SECRET", "ANOTHER_SECRET"))
    )

In the above example, the environment variables `ENV_SECRET` and
`ANOTHER_SECRET` will also be set to the same values in the queue.

## Shiny module

Additionally, RRSQ includes a Shiny module to easily create a viewable
list of each job in the queue for the current user.

``` r
ui <- shiny::fluidPage(

  ## Will display a searchable table of jobs on the queue for the current user.
  rrsq::queueViewOutput("any_id_you_want"),

  ## Will display the row currently highlighted in the table.
  tableOutput("row")
)

server <- function(input, output, session) {

  ## Customizable buttons can optionally be added to the table.
  button_ids = c("X_button", "A_button", "B_button", "C_button")
  button_labels = c("X", "A", "B", "C")

  rrsq_object <- rrsq::RSimpleQueue$new()
  user <- rrsq::whoami(session = session, .mock_user = "Dreznel")

  module_return_reactive_values <- shiny::callModule(
    rrsq::queueView,
    "any_id_you_want",
    queue_obj = rrsq_object,
    button_ids = button_ids,
    button_labels = button_labels,
    .user = user,
    .refresh_interval = 3000
  )

  ## Highlighted row can be acccessed via the return value of the module.
  ## Currently, only one row can be highlighted at a time.
  output$row <- shiny::renderTable(module_return_reactive_values$highlighted_rows)

  ## Buttons added to the table can be observed by the IDs you have assigned them.
  ## The input value of the button will be assigned to the index of the row it was
  ## clicked from.
  observeEvent(input$X_button, {
    x_job_id <- dplyr::slice(module_return_reactive_values$rows, as.numeric(input$X_button))
    showNotification(glue::glue("X Clicked for job id {x_job_id}!"))
    print(dplyr::slice(module_return_reactive_values$rows, as.numeric(input$X_button)))
  })

  observeEvent(input$A_button, {
    a_index <- input$A_button
    showNotification(glue::glue("A Clicked for row {a_index}"))
  })
}

shiny::shinyApp(ui = ui, server = server)
```
