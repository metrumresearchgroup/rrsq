#' Information about running R
#' @param .envs environment variables to retain
#' @param .pkgs packages to load in new session
#' @details
#' .envs will naturally grab all R_<var> variables, such as R_LIBS_SITE
#' to set
#' @export
collect_r_details <- function(.envs = NULL, .pkgs = NULL) {
  rpath <- file.path(R.home("bin"), "R")
  envs <- Sys.getenv()
  renvs <- envs[grepl(pattern = "R_.*", names(envs))]
  if (!is.null(.envs)) {
    renvs <- unique(c(renvs, envs[which(names(envs) %in% .envs)]))
  }
  if (!is.null(.pkgs)) {
    session <- sessionInfo()
    loaded_pkgs <- names(session$otherPkgs)
  }
  list(
    rpath = rpath,
    renvs = renvs,
    loaded_pkgs = loaded_pkgs,
    cwd = getwd()
  )
}
