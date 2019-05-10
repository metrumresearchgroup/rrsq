context("test-collect-r-details")

DEFAULT_LIB_STRING <- paste0(.libPaths(), collapse = ":")
DEFAULT_ENV_NAMES <- c(
 "R_LIBS_SITE" ,
 "R_LIBS_USER",
 "R_LIBS"
)

Sys.setenv(RRSQTESTENV1 = "test1", RRSQTESTENV2 = "test2")

describe("collect_r_details handles environment variables", {
  it("sets default R lib paths env variables", {
    renv <- collect_r_details()$renv
    expect_true(all(names(renv) %in% DEFAULT_ENV_NAMES))
    expect_equal(length(names(renv)), length(DEFAULT_ENV_NAMES))
    expect_true(all(purrr::map_lgl(unlist(renv), ~ .x == DEFAULT_LIB_STRING)))
  })
  it("will keep requested env", {
    renv <- collect_r_details(.envs = c("RRSQTESTENV1", "RRSQTESTENV2"))$renv
    expect_equal(renv$RRSQTESTENV1, "test1")
    expect_equal(renv$RRSQTESTENV2, "test2")
  })
  it("will not keep unrequested env", {
    renv <- collect_r_details(.envs = c("RRSQTESTENV1"))$renv
    expect_equal(renv$RRSQTESTENV1, "test1")
    expect_null(renv$RRSQTESTENV2)
  })
})

