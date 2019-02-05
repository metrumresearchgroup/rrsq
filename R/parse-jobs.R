to_datetime <- function(.x) {
  as.POSIXct(strptime(.x, format = "%Y-%m-%dT%H:%M%OS",tz = "UTC"))
}
job_to_df <- function(.j) {
  data.frame(
    id = .j$ID,
    status = .j$Status,
    context = .j$Context,
    user = .j$User,
    queue_time = .j$RunDetails$queue_time %||% NA,
    start_time = .j$RunDetails$start_time %||% NA,
    end_time = .j$RunDetails$end_time %||% NA,
    exit_code = .j$Result$exit_code,
    output = .j$Result$output %||% NA, stringsAsFactors = FALSE)
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

# don't use purrr as requires dplyr for map_dfr
map_dfr <- function(.x, .f, ...) {
  data.table::rbindlist(lapply(.x, .f, ...),
                        use.names = TRUE,
                        fill = TRUE)
}
