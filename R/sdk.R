## User-facing functions ----------------------------------------

#' create_api_config
#'
#' function to create a top level "API config" object, which stores basic, root-level information
#' about the specific RSQ API to be queried. The API config object can be used to construct endpoints,
#' as well as to store authorization information (if this becomes applicable in the future)
#'
#' @param host the hostname of server hosting the desired RSQ API. Should include the HTTP/HTTPS schema.
#' @param port the port on which the API his hosted
#' @param root the root of the API, without leading or trailing slashes.
#' @param test_ping boolean value. If TRUE, will ping the RSQ Service to verify that is is responding.
#'   Will throw an error if the service is not responsive.
#' @param test_ping_timeout an integer value (in seconds) determining how long the test_ping should
#'   wait for the RSQ service to reply before determining that the service is unresponsive.
#' @return an object of class "rsq_api_config" (set by CONSTANTS$CLASSES$API_CONFIG) that can be passed
#' into other functions to facilitate the submission of API queries.
create_api_config <- function(
  host = get_api_host(),
  port = get_api_port(),
  root = get_api_root(),
  test_ping = TRUE,
  test_ping_timeout = 10
) {
  config_obj <- list(
    # scheme = scheme,
    host = host,
    port = port,
    root = root
  )

  class(config_obj) <- append(class(config_obj), CONSTANTS$CLASSES$API_CONFIG)

  if(test_ping) {
    is_responsive <- ping(config_obj, timeout=test_ping_timeout)
    if(!is_responsive) {
      base_url <- get_endpoint_url(config_obj, "")
      rlang::abort(
        message = glue::glue("The RSQ service at {base_url} is not responding after {test_ping_timeout} seconds."),
        class = CONSTANTS$ERRORS$SERVICE_UNRESPONSIVE
      )
    }
  }

  return(config_obj)
}

#' get_job
#'
#' function to query an RSQ API and return the current status/info of single job.
#' job can be returned as a list object with class `rsq_job`, or as a data.frame.
#'
#' @param api_config an api_config object
#' @param job_id the numeric ID of the RSQ job to return, OR an object of class "rsq_job"
#' @param return_df boolean. If TRUE, results will be returned as a one-row data.frame.
#'   Otherwise, results will be returned as a list-object.
get_job <- function(
  api_config,
  job,
  return_df = FALSE
) {
  check_api_config(api_config)
  if(inherits(job, CONSTANTS$CLASSES$RSQ_JOB)) {
    job_id = job$id
  } else {
    job_id = job
  }

  endpoint_url <- get_endpoint_url(
    api_config = api_config,
    endpoint = glue::glue("{CONSTANTS$ENDPOINTS$JOB}/{job_id}")
  )

  api_results <- api_get(endpoint_url = endpoint_url)
  class(api_results) <- append(class(api_results), CONSTANTS$CLASSES$RSQ_JOB)

  if(return_df) {
    return(api_results %>% job_to_df())
  } else {
    return(api_results)
  }
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
  # Or do I always make a big API call and then filter down later?
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

#' submit job2
#'
#' submit a job to RSQ using an rsq_api_config object.
#' .submit_job is already a function, so we have added a "2" to this to avoid confusion.
#'
#' @param api_config an rsq_api_config object
#' @param srvr rsq server url
#' @param user user submitting job
#' @param context job context
#' @param rscript_path path to rscript
#' @param work_dir directory to start R from
#' @param r_path path to R executable
#' @param renv list of environment variables
#' @return
#' an `rsq_job` object.
submit_job2 <- function(
  api_config,
  user,
  context,
  rscript_path,
  work_dir = NULL,
  r_path = NULL,
  renv = NULL
) {
  check_api_config(api_config)
  endpoint_url <- get_endpoint_url(api_config, CONSTANTS$ENDPOINTS$JOB)

  r_details <- unbox_details(collect_r_details())
  if (!is.null(work_dir)) {
    r_details$work_dir <- work_dir
  }
  if (!is.null(r_path)) {
    r_details$r_path <- r_path
  }
  if (!is.null(renv)) {
    r_details$renv <- renv
  }
  submission_values <- list(
    "id"= jsonlite::unbox(0),
    "status"= jsonlite::unbox("QUEUED"),
    "user"= jsonlite::unbox(user),
    "context"= jsonlite::unbox(context),
    "rscript"= list(
      "r_path"= jsonlite::unbox(r_details$r_path),
      "work_dir"= jsonlite::unbox(r_details$work_dir),
      "rscript_path"= jsonlite::unbox(rscript_path),
      "renv"= r_details$renv
    )
  )

  job <- api_post(
    endpoint_url = endpoint_url,
    body = submission_values
  )

  class(job) <- append(class(job), CONSTANTS$CLASSES$RSQ_JOB)
  return(job)
}

#' ping
#'
#' function to hit the "ping" endpoint of an RSQ service
#' in order to verify that the service is responding.
#'
#' @param api_config an rsq_api_config object
#' @param timeout parameter to determine how long (in seconds) the ping should wait for a response. Defaults to 10 seconds.
#' @return TRUE if the service is responsive, FALSE otherwise.
ping <- function(api_config, timeout = 10) {
  check_api_config(api_config)
  endpoint_url <- get_endpoint_url(api_config, CONSTANTS$ENDPOINTS$PING)

  # We want to see the response object itself, so we're just using HTTR directly here instead of the functions.
  ping_result <- httr::GET(url = endpoint_url, httr::timeout(timeout))
  # check_response(ping_result)

  if(is.null(ping_result)) {
    return(FALSE)
  } else {
    return(TRUE)
  }
}


#' get_version
#'
#' function to return the version of the running RSQ service
#'
#' @param api_config an rsq_api_config object
#' @return a list object containing the version of the RSQ service.
get_version <- function(api_config) {
  check_api_config(api_config)
  endpoint_url <- get_endpoint_url(api_config, CONSTANTS$ENDPOINTS$VERSION)

  ## Using httr directly here because this endpoint doesn't return JSON.
  ## I think that should be changed on the RSQ side -- downstream services
  ## should be able to expect a consistent return type.
  resp <- httr::GET(endpoint_url)
  check_response(resp)
  result <- httr::content(resp)
  return(result)
}

#' cancel_job
#'
#' function to hit the "cancel job" endpoint to cancel an RSQ job.
#'
#' @param api_config an rsq_api_config object
#' @param job the numeric ID of the RSQ job to return, OR an object of class "rsq_job"
#' @return the response content from the cancel job API call.
cancel_job <- function(api_config, job) {
  check_api_config(api_config)
  if(inherits(job, CONSTANTS$CLASSES$RSQ_JOB)) {
    job_id = job$id
  } else {
    job_id = job
  }

  endpoint_url_base <- get_endpoint_url(api_config, CONSTANTS$ENDPOINTS$JOB_CANCEL)
  endpoint_url <- glue::glue("{endpoint_url_base}/{job_id}")

  results <- api_put(endpoint_url)
  return(results)
}

## End user-facing functions  ---------------------------------


#################################################################
#################################################################
#################################################################
#################################################################


## Internal functions  ----------------------------------------
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
## End internal functions  ------------------------------------
