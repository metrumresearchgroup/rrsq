# plumbing # ----

# Starting out by modeling this roughly on the Bridge API SDK.
# This is very redundant, but let's see where this goes.

#TODO: add option checking?

#' CONSTANTS
#'
#' Helper object to store constants such as endpoint addresses and such.
CONSTANTS <- list(
  ENDPOINTS <- list(
    JOB = "job",
    JOBS = "jobs",
    VERSION = "version",
    ADDRESS = "address",
    JOB_CANCEL = "job/cancel"
  ),
  ERRORS = list(
    UNAUTHORIZED = "rsq_unauthorized",
    SERVER = "rsq_server_error"
  )
)


#' get_api_root
#'
#' function to retrieve the API root (might not apply to RSQ)
#'
#' @param root the root to use, if not the default
get_api_root <- function(root = Sys.getenv("RSQ_API_ROOT")) {
  if(is.null(root) || root == "") {
    return("")
  } else {
    return(root)
  }
}

#' get_api_host
#'
#' function to get the API host.
#'
#' @param host the host to use, if not the default.
get_api_host <- function(host = Sys.getenv("RSQ_API_HOST")) {
  if(is.null(host) || host == "") {
    return("http://localhost")
  } else {
    return(host)
  }
}

#' get_api_port
#'
#' function to get the API port
#'
#' @param port the port to use, if not the default
get_api_port <- function(port = as.numeric(Sys.getenv("RSQ_API_PORT"))) {
  if(is.null(port) || port == "") {
    return("")
  } else {
    return(port)
  }
}

#' set_default_env_vars
#'
#' dev function to set the environment variables to the "classic" defaults
set_default_env_vars <- function() {
  Sys.setenv(
    RSQ_API_ROOT="",
    RSQ_API_HOST="http://localhost",
    RSQ_API_PORT=8950
  )

  return(Sys.getenv(c("RSQ_API_ROOT", "RSQ_API_HOST", "RSQ_API_PORT")))
}


#' get_endpoint_url
#'
#' function to get the usable URL to an endpoint
#'
#' @param endpoint the endpoint to hit (string). Should not include leading/trailing /'s but should include inner /'s.
#' @param host the host URL, including the http/https scheme.
#' @param port the port, not including the colon
#' @param root the API root, with no trailing slashes.
get_endpoint_url <- function(
  endpoint,
  host = get_api_host(),
  port = get_api_port(),
  root = get_api_root()
) {
  if( is.null(port) || port == "" ) {
    port_string <- ""
  } else {
    port_string <- glue::glue(":{port}")
  }

  if(is.null(root) || root == "") {
    root_string <- ""
  } else if(root == "/") {
    root_string <- "/"
  } else {
    root_string = glue::glue("/{root}")
  }

  url <- glue::glue("{host}{port_string}{root_string}/{endpoint}")
  return(url)
}


#Stealing from bapi:
check_response <- function(resp) {
  if (httr::status_code(resp) == 400) {
    rlang::abort(httr::http_status(resp)$message,
                 "bad_request",
                 resp = resp)
  }
  if (httr::status_code(resp) == 401) {
    rlang::abort(httr::http_status(resp)$message,
                 CONSTANTS$ERRORS$UNAUTHORIZED,
                 resp = resp)
  }
  if (httr::status_code(resp) == 500) {
    # the bridge api returns {error: msg} style content responses
    # on internal server error
    rlang::abort(httr::content(resp)$error,
                 CONSTANTS$ERRORS$SERVER,
                 resp = resp)
  }
  if (httr::status_code(resp) != 200) {
    rlang::abort(httr::http_status(resp)$message,
                 CONSTANTS$ERRORS$SERVER,
                 resp = resp)
  }
}


#' submit a Get request to the RSQ API
#'
#' @param endpoint_url the full API URL for the endpoint to receive the GET request
#' @param ... query parameters to be sent with the API request.
#' @return the response from the API.
api_get <- function(
  endpoint_url,
  ...
) {
  # TODO: do some verification of type, etc.

  # Taking directly from bapisdk... I don't think we need query params, but why not?
  # to prevent encoding make sure to set as is
  # we also don't want any NULL query params so compact them
  # away, otherwise will get errors like:
  # Calling 'structure(NULL, *)' is deprecated, as NULL cannot have attributes.
  # Consider 'structure(list(), *)'
  # instead.Calling 'structure(NULL, *)' is deprecated, as NULL cannot have attributes.
  # in addition, query params have trouble encoding some types
  query_params <- lapply(
    purrr::compact(rlang::list2(...)),
    function(.x) {
      if (rlang::is_bare_numeric(.x)) {
        return(.x)
      }
      return(I(.x))
    }
  )

  resp <- httr::GET(
    url = endpoint_url,
    config = NULL,
    query = query_params
  )

  check_response(resp)

  return(extract_json(resp))
}

#' extract_json
#' function to pull JSON out of a response object.
#' @param response the response object, which should have a $content field that is just raw JSON.
#' @param ... additional arguments to fromJSON.
#'
extract_json <- function(response, ..., simplify_df = FALSE) {
  return(
    jsonlite::fromJSON(rawToChar(response$content), ..., simplifyDataFrame = simplify_df)
  )
}

# end plumbing # ----
