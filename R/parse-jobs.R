#' Function to format an object into a datetime.
#' @param .x An object to convert to a datetime.
#' @export
to_datetime <- function(.x) {
  as.POSIXct(strptime(.x, format = "%Y-%m-%dT%H:%M%OS",tz = "UTC"))
}

#' convert a single job object into a dataframe.
#' @param .j A single job object.
#' @export
job_to_df <- function(.j) {
  data.frame(
    id = .j$id,
    status = .j$status,
    context = .j$context,
    user = .j$user,
    queue_time = .j$run_details$queue_time %||% NA,
    start_time = .j$run_details$start_time %||% NA,
    end_time = .j$run_details$end_time %||% NA,
    exit_code = .j$result$exit_code,
    output = .j$result$output %||% NA, stringsAsFactors = FALSE)
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
    result <- map_dfr(.jl, job_to_df)
    result$queue_time <- to_datetime(result$queue_time)
    result$start_time <- to_datetime(result$start_time)
    result$end_time <- to_datetime(result$end_time)
    result$duration <- time_difference(result$end_time, result$start_time, units = "secs")
    return(result)
}

#' don't use purrr as requires dplyr for map_dfr
#' @param .x A list to iterate over.
#' @param .f A function to apply to each element in the list.
#' @param ... Additional arguments to be supplied to .f
#' @export
#' @importFrom data.table rbindlist
map_dfr <- function(.x, .f, ...) {
  data.table::rbindlist(lapply(.x, .f, ...),
                        use.names = TRUE,
                        fill = TRUE)
}
