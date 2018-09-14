library(tidyverse)

get_results <- function() {
  fake_results <-
    list(
      list(
        ID = 1L,
        Status = "COMPLETED",
        RunDetails = list(
          queue_time = "0001-01-01T00:00:00Z",
          start_time = "2018-09-06T16:40:34.674778Z",
          end_time = "2018-09-06T16:40:34.851214Z"
        ),
        Context = "interesting job1",
        Rscript = list(
          r_path = "R",
          work_dir = "/Users/devin/go/src/github.com/metrumresearchgroup/rsq/scratch/cmd",
          rscript_path = "/Users/devin/go/src/github.com/metrumresearchgroup/rsq/scratch/cmd/add-list.R",
          renv = list(R_LIBS_SITE = "test")
        ),
        Result = list(output = "success!",
                      exit_code = 0L),
        User = "Ola"
      ),
      list(
        ID = 2L,
        Status = "COMPLETED",
        RunDetails = list(
          queue_time = "0001-01-01T00:00:00Z",
          start_time = "2018-09-06T16:45:33.88106Z",
          end_time = "2018-09-06T16:45:34.046362Z"
        ),
        Context = "interesting job1",
        Rscript = list(
          r_path = "R",
          work_dir = "/Users/devin/go/src/github.com/metrumresearchgroup/rsq/scratch/cmd",
          rscript_path = "/Users/devin/go/src/github.com/metrumresearchgroup/rsq/scratch/cmd/add.R",
          renv = list(R_LIBS_SITE = "test")
        ),
        Result = list(output = "success!",
                      exit_code = 0L),
        User = "Ola"
      ),
      list(
        ID = 3L,
        Status = "RUNNING",
        RunDetails = list(
          queue_time = "0001-01-01T00:00:00Z",
          start_time = "2018-09-06T16:45:58.94665Z",
          end_time = "2018-09-06T16:45:59.099218Z"
        ),
        Context = "interesting job1",
        Rscript = list(
          r_path = "R",
          work_dir = "/Users/devin/go/src/github.com/metrumresearchgroup/rsq/scratch/cmd",
          rscript_path = "/Users/devin/go/src/github.com/metrumresearchgroup/rsq/scratch/cmd/add.R",
          renv = list(R_LIBS_SITE = "test")
        ),
        Result = list(output = "[1] 4\n",
                      exit_code = 0L),
        User = "Ola"
      )
     )
      return(fake_results)
}

results <-  get_results() %>%
  map_df(function(.j) {
    data_frame(
      ID = .j$ID,
      STATUS = .j$Status,
      CONTEXT = .j$Context,
      Result = .j$Result$output
    )
})

results

library(shiny)
library(shinytoastr)

ui <- fluidPage(
  useToastr(),
  actionButton("submit_analysis", "submit dat analysis"),
  actionButton("mock_done", "mock done")
)

server <- function(input, output, session) {
  startup <- TRUE
  analysis_results <- reactiveValues()
  analysis_results$non_complete <- NULL
  observeEvent(input$submit_analysis, {
    non_complete_ids <- get_results()  %>%
      map_df(function(.j) {
        data_frame(
          ID = .j$ID,
          STATUS = .j$Status,
          CONTEXT = .j$Context,
          Result = .j$Result$output
        )
      })    %>% filter(STATUS != "COMPLETED") %>% pull(ID)
    if (length(non_complete_ids)) {
      analysis_results$non_complete <- non_complete_ids
      #rv$check_job_status <- rv$check_job_status + 1
    }
  })

  observeEvent(input$mock_done, {
      analysis_results$non_complete <- NULL
  })
  observe({
    # Re-execute this reactive expression after 1000 milliseconds
    if (!is.null(analysis_results$non_complete) && length(analysis_results$non_complete)) {
      # replace with comparison logic
      print("still monitoring...")
      invalidateLater(250, session)
    } else {
      if (startup) {
        startup <<- FALSE
      } else {
        toastr_success("analysis completed!", position = "bottom-right" )
        print("completed monitoring, we're done")
      }
    }
  })
}

shinyApp(ui = ui, server = server)

# observe({
#   analysis_results$non_complete
#   # compare non_complete against the last time we checked
#   # if any were non-complete last time and not present now, alert user
#
#   # if any non_complete, recheck after some interval
#   # be careful that this isn't constantly retriggering if no work queued/running
# })
