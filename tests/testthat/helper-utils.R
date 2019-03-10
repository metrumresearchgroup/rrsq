skip_if_no_rsq <- function(){
  if (identical(Sys.getenv("RSQ_ACTIVE"), "true")) {
    return(invisible(TRUE))
  }
  skip("No running rsq")
}
