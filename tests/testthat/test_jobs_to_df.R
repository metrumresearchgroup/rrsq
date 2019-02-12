library(rrsq)

# Remember, on Mac, this context line is REQUIRED. This is possibly a bug with testthat version 2.0.0.
context("Test jobs_to_df and job_to_df functions")

testthat::test_that(desc = "Dataframe is returned from job_to_df (singular)", {
  jobs <- dget(file = file.path("test_data/job_list_nine_items.txt"))
  job = jobs[[2]]
  tryCatch({
    result <- job_to_df(job)
  }, error = function(e){
    testthat::fail(message = paste0("Error occured during test: ", e$message))
  })
  testthat::expect_true(is.data.frame(result))
})


testthat::test_that(desc = "Dataframe is returned from jobs_to_df", {
  jobs <- dget(file = file.path("test_data/job_list_nine_items.txt"))

  result <- tryCatch({
    jobs_to_df(jobs)
  }, error = function(e){
    testthat::fail(message = paste0("Error occured during test: ", e$message))
  })
  testthat::expect_equal(object = nrow(result), expected = 9)
  testthat::expect_true(is.data.frame(result))
  testthat::expect_equal(object = length(result), expected = 10)

})

testthat::test_that(desc = "Test jobs_to_df returns NULL after NULL argument", {
  result <- jobs_to_df(NULL)
  testthat::expect_null(result)
})

