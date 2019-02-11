# Remember, on Mac, this context line is REQUIRED. This is possibly a bug with testthat version 2.0.0.
context("Test jobs_to_df and job_to_df functions")

# testthat::test_that(desc = "Dataframe is returned from job_to_df (singular)", {
#   expect_true(file.exists(system.file("tests/testthat/job_list_four_items.txt", package = "rrsq")))
#   jobs <- dget(file = system.file("tests/testthat/job_list_four_items.txt", package = "rrsq"))
#   job = jobs[[2]]
#   tryCatch({
#     result <- rrsq::job_to_df(job)
#   }, error = function(e){
#     testthat::fail(message = paste0("Error occured during test: ", e$message))
#   })
#   testthat::expect_true(is.data.frame(result))
# })
#
#
# testthat::test_that(desc = "Dataframe is returned from jobs_to_df", {
#   jobs <- dget(file = system.file("tests/testthat/job_list_four_items.txt", package = "rrsq"))
#
#   result <- tryCatch({
#     rrsq::jobs_to_df(jobs)
#   }, error = function(e){
#     testthat::fail(message = paste0("Error occured during test: ", e$message))
#   })
#   testthat::expect_equal(object = nrow(result), expected = 9)
#   testthat::expect_true(is.data.frame(result))
#
# })

#
# testthat::test_that(desc = "I'm not crazy", {
#   expect_true(file.exists(system.file("tests/testthat/job_list_four_items.txt", package = "rrsq")))
# })
#
# testthat::test_that(desc = "This works better", {
#   expect_true(file.exists(system.file("inst/test_data/job_list_four_items.txt", package = "rrsq")))
# })
#
#
testthat::test_that(desc = "This works better2", {
  expect_true(file.exists("job_list_four_items.txt"))
})

testthat::test_that(desc = "Dataframe is returned from job_to_df (singular)", {
  jobs <- dget(file = "job_list_four_items.txt")
  job = jobs[[2]]
  tryCatch({
    result <- rrsq::job_to_df(job)
  }, error = function(e){
    testthat::fail(message = paste0("Error occured during test: ", e$message))
  })
  testthat::expect_true(is.data.frame(result))
})


testthat::test_that(desc = "Dataframe is returned from jobs_to_df", {
  jobs <- dget(file = "job_list_four_items.txt")

  result <- tryCatch({
    rrsq::jobs_to_df(jobs)
  }, error = function(e){
    testthat::fail(message = paste0("Error occured during test: ", e$message))
  })
  testthat::expect_equal(object = nrow(result), expected = 9)
  testthat::expect_true(is.data.frame(result))

})
