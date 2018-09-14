#' Information about running R
#' @param .envs environment variables to retain
#' @param .pkgs packages to load in new session, if null defaults to all loaded in current session
#' @details
#' .envs will naturally grab all R_<var> variables, such as R_LIBS_SITE
#' to set
#' @export
collect_r_details <- function(.envs = NULL, .pkgs = NULL) {
  r_path <- file.path(R.home("bin"), "R")
  envs <- Sys.getenv()
  renvs <- as.list(envs[grepl(pattern = "^R_.*", names(envs))])
  if (!is.null(.envs)) {
    renvs <- unique(c(renvs, envs[which(names(envs) %in% .envs)]))
  }
  loaded_pkgs <- if (is.null(.pkgs)) {
    session <- sessionInfo()
    names(session$otherPkgs) %||% c("")
  } else {
    .pkgs
  }
  list(
    r_path = r_path,
    renv = renvs,
    work_dir = getwd()
  )
}

#' unbox only elements that should be scalars
#' @param .x list from collect_r_details()
#' @export
unbox_details <- function(.x) {
  .x$r_path <- jsonlite::unbox(.x$r_path)
  .x$renv <- lapply(.x$renv, jsonlite::unbox)
  .x$work_dir <- jsonlite::unbox(.x$work_dir)
  return(.x)
}

