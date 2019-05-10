#' Information about running R
#' @param .envs environment variables to retain
#' @param .pkgs packages to load in new session, if null defaults to all loaded in current session
#' @details
#' the resulting environment variables will always have R_LIBS_SITE,
#' R_LIBS_USER, and R_LIBS set to the .libPaths() to forceably make
#' sure that the remote session will leverage the same libpaths
#' @importFrom utils sessionInfo modifyList
#' @return list with r_path, renv (list with set environment variables), work_dir
#' @export
collect_r_details <- function(.envs = NULL, .pkgs = NULL) {
  r_path <- file.path(R.home("bin"), "R")
  envs <- Sys.getenv()
  lib_paths <- paste0(.libPaths(), collapse = ":")
  renvs <- list(
    R_LIBS_SITE = lib_paths,
    R_LIBS_USER = lib_paths,
    R_LIBS = lib_paths
  )
  if (!is.null(.envs)) {
    .envs <- .envs[!(.envs %in% c("R_LIBS_SITE", "R_LIBS_USER", "R_LIBS"))]
    renvs <- modifyList(renvs, as.list(envs[which(names(envs) %in% .envs)]))
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

