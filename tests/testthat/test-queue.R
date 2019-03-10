context("test-queue")

poll_queue <- function(rsq, .id) {
  jr <- rsq$get_job(.id)
  ..times <- 1
  while(!(jr$status %in% c("COMPLETED", "ERROR"))){
    if (..times < 5) {
      ..times <- ..times + 1
      jr <- rsq$get_job(.id)
    } else {
      stop("failed retrieving job submission, timed out after 5 attempts")
    }
    Sys.sleep(0.2)
  }
  jr
}

describe("submitting jobs to real queue", {
  skip_if_no_rsq()
  # expect that the queue is running and the environment
  # variable RSQ_ACTIVE is set to true
  rsq <- RSimpleQueue$new()
  it("captures custom environment variables", {
    withr::with_envvar(c("RRSQTEST1" = "test1"), {
      j1 <- rsq$submit_job(
        user = "devin",
        context = "env",
        work_dir = getwd(),
        rscript_path = "test_scripts/test_env.R",
        .envs = "RRSQTEST1"
      )
    })
    expect_equal(j1$status, "QUEUED")
    expect_true("RRSQTEST1" %in% names(j1$rscript$renv))

    # will take at least 150 ms or so to start up R and run the external script
    Sys.sleep(0.25)
    jr <- poll_queue(rsq, j1$id)
    expect_equal(jr$status, "COMPLETED")
    expect_equal(jr$result$output, "RRSQTEST1 value: test1")
    expect_equal(jr$result$exit_code, 0)
  })

  it("captures ignores non-captured custom environment variables", {
    withr::with_envvar(c("RRSQTEST1" = "test1"), {
      j2 <- rsq$submit_job(
        user = "devin",
        context = "env",
        work_dir = getwd(),
        rscript_path = "test_scripts/test_env.R"
      )
    })
    expect_equal(j1$status, "QUEUED")
    expect_false("RRSQTEST1" %in% names(j1$rscript$renv))

    # will take at least 150 ms or so to start up R and run the external script
    Sys.sleep(0.25)
    jr <- poll_queue(rsq, j2$id)
    expect_equal(jr$status, "COMPLETED")
    expect_equal(jr$result$output, "RRSQTEST1 value: ")
    expect_equal(jr$result$exit_code, 0)
  })
})
