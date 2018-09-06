#' submit job
#' @param srvr rsq server url
#' @param user user submitting job
#' @param context job context
#' @param rscript_path path to rscript
#' @param work_dir directory to start R from
#' @param r_path path to R executable
#' @param renv list of environment variables
#' @param .no_submit return json that would be submitted but don't make request
#' @export
.submit_job <- function(srvr,
                        user,
                        context,
                        rscript_path,
                        work_dir = NULL,
                        r_path = NULL,
                        renv = NULL,
                        .no_submit = FALSE
) {
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
      "ID"= jsonlite::unbox(0),
      "Status"= jsonlite::unbox("QUEUED"),
      "User"= jsonlite::unbox(user),
      "Context"= jsonlite::unbox(context),
      "Rscript"= list(
        "r_path"= r_details$r_path,
        "work_dir"= r_details$work_dir,
        "rscript_path"= jsonlite::unbox(rscript_path),
        "renv"= r_details$renv
      )
  )
  submission_json <- jsonlite::toJSON(submission_values)
  if (.no_submit) {
    return(submission_json)
  }
  httr::POST(srvr, body = submission_json, encode = "json")
}
