#' Information about running R
#' @param .envs environment variables to retain
#' @param .pkgs packages to load in new session, if null defaults to all loaded in current session
#' @details
#' .envs will naturally grab all R_<var> variables, such as R_LIBS_SITE
#' to set
#' @export
collect_r_details <- function(.envs = NULL, .pkgs = NULL) {
  rpath <- file.path(R.home("bin"), "R")
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
    rpath = rpath,
    renvs = renvs,
    loaded_pkgs = loaded_pkgs,
    cwd = getwd()
  )
}

#' unbox only elements that should be scalars
#' @param .x list from collect_r_details()
#' @export
unbox_details <- function(.x) {
  .x$rpath <- jsonlite::unbox(.x$rpath)
  .x$renvs <- lapply(.x$renvs, jsonlite::unbox)
  .x$cwd <- jsonlite::unbox(.x$cwd)
  return(.x)
}

