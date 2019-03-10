
#' Create a new rsq server manager
#'
#' The Babylon generator creates a new 'Babylon'-object, which contains logic to interact
#' with a running bbq server from the babylon ecosystem.
#'
#' @format NULL
#' @usage NULL
#'
#'
#' @importFrom R6 R6Class
#' @importFrom glue glue
#'
#' @section Initialization:
#' A new rsq object is initialized with the new() method:
#'
#' `rsq <- RSimpleQueue$new()`
#'
#' and can optionally take some initialization arguments:
#'
#' * `host` - the hostname the server is running on - defaults to 'http://localhost'
#' * `port` - port the server is listening on - defaults to 8999.
#' * `verbose` - whether internal status information should be displayed
#' * `mustWork` - on initialization, check and confirm a rsq server is listening on the host/port configuration set
#'
#' @section Methods:
#' * `submit_job(...)` - check ?submit_models for arguments, the server is automatically set internally
#' * `get_jobs(status, STATUSES)` - get information about models that have been submitted
#' * `get_job(id)` - get information about a model
#' * `poll(.ids, until, timeout, interval, print, parse)` - poll a vector of models by ID name until completion
#'     * ids - vector of job ids
#'     * until - status criteria to poll against, default to COMPLETED or ERROR
#'     * timeout - length of time to poll before stopping
#'     * print - whether to print the status of all jobs each poll
#'
#' @examples \dontrun{
#' rsq <- RSimpleQueue$new()
#' rsq$get_jobs()
#'
#' # get all ids
#' bbq$get_jobs %>% map_dbl("ID")
#'
#' # find all queued models
#' bbq$get_jobs(status = "QUEUED") %>% map_dbl("ID")
#'
#' bbq$poll(1:5) # poll for models 1-5 to complete
#'
#' # get all outputs for completed runs
#' rsq$get_jobs(status = "COMPLETED") %>% map_chr(~ .x$Result$Output)
#' }
#' @docType class
#' @rdname rsq
#' @export
RSimpleQueue <-
  R6::R6Class("RSimpleQueue",
              public = list(
                lgr = NULL,
                initialize = function(init = NULL,
                                      host = "http://localhost",
                                      port = 8950,
                                      verbose = TRUE,
                                      must_work = TRUE,
                                      lgr = NULL) {
                  if (!is.null(init)) {
                    stop("init functionality not yet implemented")
                  }
                  if (is.null(lgr)) {
                    self$lgr <- logrrr::Logrrr$new()
                  } else {
                    self$lgr <- lgr
                  }
                  private$address <- ifelse(!is.null(port), glue("{host}:{port}"), host)
                  if (must_work) {
                    if(!self$ping()) {
                      stop(glue("server not responding at {srvr}", srvr = private$address))
                    }
                  }
                },
                ping = function() {
                  resp <- safe_get(glue("{address}/ping", address = private$address))$result
                  if (!is.null(resp)) {
                    return(TRUE)
                  }
                  return(FALSE)
                },
                get_version = function() {
                  if (!is.null(private$version)) {
                    return(private$version)
                  }
                  resp <- safe_get(glue("{address}/version", address = private$address))$result
                  if (!is.null(resp)) {
                    version <- rawToChar(resp$content)
                    private$version <- version
                    return(version)
                  }
                  return(FALSE)
                },
                submit_job = function(
                                      user,
                                      context,
                                      rscript_path,
                                      work_dir = NULL,
                                      r_path = NULL,
                                      .envs = NULL,
                                      .no_submit = FALSE,
                                      .parse = TRUE
                )  {
                  resp <- .submit_job(
                    srvr = glue("{private$address}/job"),
                    user = user,
                    context = context,
                    rscript_path = rscript_path,
                    work_dir = work_dir,
                    r_path = r_path,
                    .envs = .envs,
                    .no_submit = .no_submit
                  )
                  output <- if (.parse && !.no_submit) {
                    parse_response(resp)
                  } else {
                    resp
                  }
                  return(output)
                },
                get_jobs = function(status = NULL, STATUSES = c("QUEUED", "RUNNING", "COMPLETED", "ERROR"), parse = TRUE) {
                  if (is.null(status)) {
                    jobs_resp <- safe_get(glue("{address}/jobs", address = private$address))$result
                  } else {
                    if (!(status %in% STATUSES)) {
                      stop(glue("invalid status, must be one of: {paste0(STATUSES, collapse = ", ")}"))
                    }
                    jobs_resp <- safe_get(glue("{address}/jobs?status={status}", address = private$address))$result
                  }
                  if (parse) {
                    return(parse_response(jobs_resp))
                  }
                  return(jobs_resp)
                },
                get_job = function(id, parse = TRUE) {
                  job_resp <- safe_get(glue("{address}/job/{id}", address = private$address))$result
                  if (parse) {
                    return(parse_response(job_resp))
                  }
                  return(job_resp)
                },
                cancel_job = function(id, parse = TRUE) {
                  job_resp <- safe_put(glue("{address}/job/cancel/{id}", address = private$address))$result
                  if (parse) {
                    return(parse_response(job_resp))
                  }
                  return(job_resp)
                }
              ),
              private = list(
                address = NULL,
                version = NULL
              )
  )
