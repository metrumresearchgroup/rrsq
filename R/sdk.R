# plumbing # ----

# Starting out by modeling this roughly on the Bridge API SDK.
# This is very redundant, but let's see where this goes.

#TODO: add option checking?

#' CONSTANTS
#'
#' Helper object to store constants such as endpoint addresses and such.
CONSTANTS <- list(
  ENDPOINTS = list(
    JOB = "job",
    JOBS = "jobs",
    VERSION = "version",
    ADDRESS = "address",
    JOB_CANCEL = "job/cancel"
  ),
  ERRORS = list(
    UNAUTHORIZED = "rsq_unauthorized",
    SERVER = "rsq_server_error"
  ),
  CLASSES = list(
    API_CONFIG = "rsq_api_config"
  ),
  JOB_STATUS = list(
    QUEUED = "QUEUED",
    RUNNING = "RUNNING",
    COMPLETED = "COMPLETED",
    ERROR = "ERROR"
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


# #' get_endpoint_url_direct
# #'
# #' function to get the usable URL to an endpoint
# #'
# #' @param endpoint the endpoint to hit (string). Should not include leading/trailing /'s but should include inner /'s.
# #' @param host the host URL, including the http/https scheme.
# #' @param port the port, not including the colon
# #' @param root the API root, with no trailing slashes.
# get_endpoint_url_direct <- function(
#   endpoint,
#   host = get_api_host(),
#   port = get_api_port(),
#   root = get_api_root()
# ) {
#   if( is.null(port) || port == "" ) {
#     port_string <- ""
#   } else {
#     port_string <- glue::glue(":{port}")
#   }
#
#   if(is.null(root) || root == "") {
#     root_string <- ""
#   } else if(root == "/") {
#     root_string <- "/"
#   } else {
#     root_string = glue::glue("/{root}")
#   }
#
#   url <- glue::glue("{host}{port_string}{root_string}/{endpoint}")
#   return(url)
# }

#' get_endpoint_url
#'
#' function to get the usable URL to an endpoint
#'
#' @param api_config an API Config object. Should be validated prior to passing into this function.
#' @param endpoint the endpoint to hit (string). Should not include leading/trailing /'s but should include inner /'s.
get_endpoint_url <- function(
  api_config,
  endpoint
) {
  host = api_config$host
  port = api_config$port
  root = api_config$root

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
#' @return the response content from the API.
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

api_post <- function(
  endpoint_url,
  body,
  ...
) {
  # json_body <- jsonlite::toJSON(body)

  query_params <- lapply(
    purrr::compact(rlang::list2(...)),
    function(.x) {
      if (rlang::is_bare_numeric(.x)) {
        return(.x)
      }
      return(I(.x))
    }
  )

  resp <- httr::POST(
    url = endpoint_url,
    body = body,
    encode = "application/json",
    config = NULL
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

#' create_api_config
#'
#' function to create a top level "API config" object, which stores basic, root-level information
#' about the specific RSQ API to be queried. The API config object can be used to construct endpoints,
#' as well as to store authorization information (if this becomes applicable in the future)
#'
#' @param host the hostname of server hosting the desired RSQ API. Should include the HTTP/HTTPS schema.
#' @param port the port on which the API his hosted
#' @param root the root of the API, without leading or trailing slashes.
#' @return an object of class "rsq_api_config" (set by CONSTANTS$CLASSES$API_CONFIG) that can be passed
#' into other functions to facilitate the submission of API queries.
create_api_config <- function(
  host = get_api_host(),
  port = get_api_port(),
  root = get_api_root()
) {
  config_obj <- list(
    # scheme = scheme,
    host = host,
    port = port,
    root = root
  )

  class(config_obj) <- CONSTANTS$CLASSES$API_CONFIG
  return(config_obj)
}

#' check_api_config
#'
#' verifies that the given api_config object is a valid api_config object by ensuring that it has
#' the proper class attached to it.
#'
#' @param api_config an api_config object
#' @return
#' returns true if the api_config object is valid, otherwise throws an error with rlang::abort
check_api_config <- function(api_config) {
  if(!inherits(api_config, CONSTANTS$CLASSES$API_CONFIG)) {
    rlang::abort(glue::glue("expected api_config object to have class {CONSTANTS$CLASSES$API_CONFIG}. Please construct and/or set using `create_api_config`."))
  } else {
    return(TRUE)
  }
}

#' check_valid_status
#'
#' verifies that all statuses in a vector of statuses can be recognized by RSQ.
#'
#' @param statuses a case-sensitive character vector of statuses.
#' @return TRUE if all statuses in statuses can be recognized by RSQ, otherwise throws an error with rlang::abort
check_valid_statuses <- function(statuses) {
  if(is.null(statuses)) {
    return(TRUE)
  }

  valid_statuses <- get_valid_statuses()
  purrr::walk(statuses, function(.status) {
    if(!.status %in% valid_statuses) {
      rlang::abort(glue::glue("Invalid status provided: `{.status}` is not a valid RSQ job status. You can get valid statuses with rrsq::get_valid_statuses()"))
    }
  })
  return(TRUE)
}

#' get_valid_statuses
#'
#' helper function to return the current valid RSQ statuses
#'
#' @return a named list containing all valid RSQ statuses
get_valid_statuses <- function() {
  return(CONSTANTS$JOB_STATUS)
}

#' get_jobs
#'
#' function to query an RSQ API and return a data.frame of all jobs
#'
#' @param api_config an api_config object.
#' @param users an optional parameter. If set, will only return jobs for the indicated users. Can be a string or character vector.
#' @param status an optional parameter. If set, will only return jobs containing the listed statuses.
#' @return a data.frame of all current jobs.
get_jobs <- function(
  api_config,
  users = NULL,
  statuses = NULL
) {
  check_api_config(api_config)
  check_valid_statuses(statuses)

  # if(is.null(users)) {
  #   users = c()
  # }
  # if(is.null(statuses)) {
  #   statuses = c()
  # }

  endpoint <- get_endpoint_url(
    api_config = api_config,
    endpoint = CONSTANTS$ENDPOINTS$JOBS
  )

  # A bit of a conundrum.
  # I want to be let users query multiple statuses at once. Do I make up to four small API calls, but usually only one or two?
  # or do I always make a big API call and then filter down later?
  # Answer for now: Since RSQ has always been hosted locally (thus far), we can expect there to be minimal latency
  # between the client and server. Therefore, I think it is reasonable to make multiple small requests, as the overhead is
  # likely to be a bit lower in practice.
  if(!is.null(statuses)) {
    jobs_df <- purrr::map_dfr(statuses, function(.status) {
      return(api_get(endpoint, status=.status) %>% jobs_to_df())
    })
  } else {
    jobs_df <- api_get(endpoint) %>% jobs_to_df()
  }

  # Does RSQ's API provide an option to filter by users API-side?
  if(!is.null(users)) {
    jobs_df <- jobs_df %>% dplyr::filter(user %in% users)
  }

  return(jobs_df)
}

