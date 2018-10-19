#' Default value for `NULL`.
#'
#' This infix function makes it easy to replace `NULL`s with a
#' default value. It's inspired by the way that Ruby's or operation (`||`)
#' works.
#'
#' @param x,y If `x` is NULL, will return `y`; otherwise returns
#'   `x`.
#' @export
#' @name null-default
#' @examples
#' 1 %||% 2
#' NULL %||% 2
`%||%` <- function(x, y) {
  if (is.null(x)) {
    y
  } else {
    x
  }
}


safe_get <- purrr::safely(httr::GET)
safe_put <- purrr::safely(httr::PUT)

parse_response <- function(resp) {
  if (!is.null(resp)) {
    return(jsonlite::fromJSON(rawToChar(resp$content), simplifyDataFrame = F))
  }
  return(NULL)
}

#' calculate the time difference
#' @param start_time the start time
#' @param end_time the end time
#' @param units units to calculate, defaults to secs
#' @export
time_difference <- function(end_time, start_time, units = "secs") {
  as.numeric(difftime(end_time, start_time, units = units))
}
