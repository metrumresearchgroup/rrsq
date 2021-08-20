# plumbing # ----

# Starting out by modeling this roughly on the Bridge API SDK.

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
  if(is.na(port) || is.null(port) || port == "") {
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

#' check_response
#'
#' function to check an HTTP response code and throw a relevant error, if applicable.
#'
#' @param resp an HTTP Response object
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
  if (!httr::status_code(resp) %in% c(200, 202)) {
    rlang::abort(httr::http_status(resp)$message,
                 CONSTANTS$ERRORS$SERVER,
                 resp = resp)
  }
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

#' submit a Get request to the RSQ API
#'
#' @param endpoint_url the full API URL for the endpoint to receive the GET request
#' @param ... query parameters to be sent with the API request.
#' @param return_df boolean to indicate if the response content should be converted to a DF via the "simplify_df" option of jsonlite::fromJSON.
#' @return a list-object (if return_df is false) or data.frame (if return_df is true) containing the HTTP response content.
api_get <- function(
  endpoint_url,
  ...,
  return_df = FALSE
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

  return(extract_json(resp, simplify_df = return_df))

}

#' submit a POST request to the RSQ API
#' @param endpoint_url the full API URL for the endpoint to receive the GET request
#' @param body a list object that will be converted into a JSON body and attached to the REST query.
#' @param ... query parameters sent with the API request.
#' @param return_df boolean to indicate if the response content should be converted to a DF via the "simplify_df" option of jsonlite::fromJSON.
#' @return a list-object (if return_df is false) or data.frame (if return_df is true) containing the HTTP response content.
api_post <- function(
  endpoint_url,
  body = NULL,
  ...,
  return_df = FALSE
) {
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
    encode = "json",
    config = NULL
  )

  check_response(resp)


  return(extract_json(resp, simplify_df = return_df))

}

#' submit a PUT request to the RSQ API
#' @param endpoint_url the full API URL for the endpoint to receive the PUT request
#' @param body a list object that will be converted into a JOSN body and attached to the REST query.
#' @param ... query parameters sent with the API request
#' @param return_df boolean to indicate if the response content should be converted to a DF via the "simplify_df" option of jsonlite::fromJSON.
#' @return a list-object (if return_df is false) or data.frame (if return_df is true) containing the HTTP response content.
api_put <- function(
  endpoint_url,
  body = NULL,
  ...,
  return_df = FALSE
) {
  query_params <- lapply(
    purrr::compact(rlang::list2(...)),
    function(.x) {
      if (rlang::is_bare_numeric(.x)) {
        return(.x)
      }
      return(I(.x))
    }
  )

  resp <- httr::PUT(
    url = endpoint_url,
    body = body,
    encode = "json",
    config = NULL
  )

  check_response(resp)


  return(extract_json(resp, simplify_df = return_df))
}



# end plumbing # ----
