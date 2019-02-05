# Remember, on Mac, this context line is REQUIRED. This is possibly a bug with testthat version 2.0.0.
context("Test jobs_to_df function")

testthat::setup({

})

testthat::test_that(desc = "Dataframe is returned", {

  # Initialize this to a valid object.
  jobs = dget(file = system.file("tests/testthat/job_list_four_items.txt", package = "rrsq"))
  print(length(jobs))
  expect_true(TRUE)

  tryCatch({
    result <- jobs_to_df(jobs)
  }, error = function(e){
    testthat::fail(message = paste0("Error occured during test: ", e$message))
  })
  testthat::expect_equal(object = length(result), expected = 4)
  testthat::expect_true(is.data.frame(result))

})

testthat::test_that("Should always pass", {
  expect_true(TRUE)
})
