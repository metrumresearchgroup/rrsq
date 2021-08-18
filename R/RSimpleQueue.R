
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

                #' @description
                #' Constructor for an RSQ object, which can be used to query a running RSQ instance.
                #' @param init not used
                #' @param port the
                #' @param host the host URL of the running RSQ service
                #' @param port the port of the running RSQ service
                #' @param verbose not used
                #' @param must_work boolean, if TRUE, will try to ping the given host:port and will throw an error if the server isn't responding.
                #' @param lgr an optional logrrr::Logrrr object to be used with the RSQ object.
                #'
                #' @return an RSQ object, which can be used to query a running RSQ instance
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

                #' @description
                #' Utility method to ensure that an RSQ endpoint can be reached by pinging it.
                #' @return TRUE if the address is responsive, FALSE otherwise.
                ping = function() {
                  resp <- safe_get(glue("{address}/ping", address = private$address))$result
                  if (!is.null(resp)) {
                    return(TRUE)
                  }
                  return(FALSE)
                },

                #' @description
                #' Returns the version of the running RSQ executable.
                #' @details
                #' `version` is stored as a private variable on the RSQ object after being retrieved from the RSQ endpoint.
                #' Subsequent calls to this method return this private variable, rather than the value returned by RSQ.
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

                #' @description
                #' Method to submit a job to RSQ.
                #' @details
                #' RSQ jobs are executed by pointing RSQ at an RScript living on the filesystem and providing
                #' some extra parameters around the executing R Session, such as working directory and environment
                #' variables.
                #'
                #' Note that all RSQ jobs are run on the server executing RSQ. Therefore, the RScript to run
                #' must exist on the same server, and any paths or configurations must be constructed relative
                #' to the RSQ-executing server.
                #' @param user the username of the person submitting the job.
                #' @param context a string providing some context for the job. This is open-ended and can be used
                #' later to help identify which job is which.
                #' @param rscript_path the path to the <script>.R file to be executed.
                #' @param work_dir the directory in which to execute the R script. Note that .Rprofiles from that directory
                #' will be applied as normal during R script execution.
                #' @param r_path the path to the R executable to use. Will default to the system's default R installation.
                #' @param renv a list of environment variables to be set in the R session.
                #' @param .no_submit a boolean value. If TRUE, this function will return the JSON of the RSQ submit-job request,
                #' but will not actually submit the request. Default is FALSE.
                #' @param .parse a boolean value. If TRUE (default), this method will return the JSON response of the RSQ
                #' service as an R list. If FALSE, this method will return the RSQ response as raw JSON.
                #' @return
                #' An R object (list) or a json string, depending on the value of `.parse`, with details for the submitted job.
                submit_job = function(
                                      user,
                                      context,
                                      rscript_path,
                                      work_dir = NULL,
                                      r_path = NULL,
                                      renv = NULL,
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
                    renv = renv,
                    .no_submit = .no_submit
                  )
                  output <- if (.parse && !.no_submit) {
                    parse_response(resp)
                  } else {
                    resp
                  }
                  return(output)
                },

                #' @description
                #' Method to get details about all available jobs
                #' @param status an optional job status to filter results on. Options are "QUEUED", "RUNNING", "COMPLETED", and "ERROR"
                #' @param STATUSES not intended for use by end-users.
                #' @param parse a boolean value. If TRUE (default), this method will return the JSON response of the RSQ
                #' service as an R list. If FALSE, this function will return the RSQ response as raw JSON.
                #' @return
                #' An R object (list) or a json string, depending on the value of `parse`, containing the details for all jobs with
                #' the given status (or simply all jobs if status is NULL.)
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

                #' @description
                #' Method to return the details for a single job. Requires the job ID.
                #' @param id the numeric ID of the job to be acquired.
                #' @param parse a boolean value. If TRUE (default), this method will return the JSON response of the RSQ
                #' service as an R list. If FALSE, this function will return the RSQ response as raw JSON.
                #' @return
                #' An R object (list) or a json string, depending on the value of `parse`, containing the details for the requested job.
                get_job = function(id, parse = TRUE) {
                  job_resp <- safe_get(glue("{address}/job/{id}", address = private$address))$result
                  if (parse) {
                    return(parse_response(job_resp))
                  }
                  return(job_resp)
                },

                #' @description
                #' Method to cancel a job with the given ID.
                #' @param id the numeric ID of the job to be canceled.
                #' @param parse a boolean value. If TRUE (default), this method will return the JSON response of the RSQ
                #' service as an R list. If FALSE, this function will return the RSQ response as raw JSON.
                #' @return
                #' An R object (list) or a json string, depending on the value of `parse`, with the response from the RSQ service.
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
