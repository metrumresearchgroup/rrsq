to_datetime <- function(.x) {
  as.POSIXct(strptime(.x, format = "%Y-%m-%dT%H:%M%OS",tz = "UTC"))
}
job_to_df <- function(.j) {
  data.frame(
    id = .j$id,
    status = .j$status,
    context = .j$context,
    user = .j$user,
    queue_time = .j$run_details$queue_time,
    start_time = .j$run_details$start_time,
    end_time = .j$run_details$end_time,
    exit_code = .j$result$exit_code,
    output = .j$result$output
  )
}

#' convert list of jobs returned from db to a tidy dataframe
#' @param .jl job list
#' @details
#' current drops some of the run details that shouldn't matter to the user
#' such as the script execution location
#' @export
jobs_to_df <- function(.jl){
  # only single job, lets convert it to a list so retains same list-like nature
  if ("id" %in% names(.jl)) {
    .jl <- list(.jl)
  }
    result <- purrr::map_dfr(.jl, job_to_df)
    result$queue_time <- to_datetime(result$queue_time)
    result$start_time <- to_datetime(result$start_time)
    result$end_time <- to_datetime(result$end_time)
    result$duration <- time_difference(result$end_time, result$start_time, units = "secs")
    return(result)
}
