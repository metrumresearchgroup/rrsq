#' convert list of jobs returned from db to a tidy dataframe
#' @param .jl job list
#' @details
#' current drops some of the run details that shouldn't matter to the user
#' such as the script execution location
#' @export
jobs_to_df <- function(.jl){
  # only single job, lets convert it to a list so retains same list-like nature
  if ("ID" %in% names(.jl)) {
    .jl <- list(.jl)
  }
    result <- purrr::map_dfr(.jl, function(.j) {
      details <- dplyr::as_data_frame(.j$RunDetails)
      result <- dplyr::as_data_frame(.j$Result)
      .j$Result <- NULL
      .j$RunDetails <- NULL
      .j$Rscript <- NULL
      dplyr::as_data_frame(.j) %>% dplyr::bind_cols(details, result) %>%
        dplyr::mutate_at(dplyr::vars(dplyr::ends_with("_time")), as.POSIXct) %>%
        dplyr::mutate(duration = time_difference(end_time, start_time, units = "secs"))
    })
    names(result) <- toupper(names(result))
    return(result)
}
