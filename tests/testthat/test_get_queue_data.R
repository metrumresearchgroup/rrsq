
context("Test get_queue_data")

library(rrsq)

testthat::test_that("Returns null when null, no jobs in queue", {
  queue_obj <- list() # RSimpleQueue$new() # -- If we have to initialize a new RSimpleQueue, tests become dependent on an RSQ running.
  .user <- "Cats_and_Oranges"
  jobs <- dget(file = file.path("test_data/job_list_zero_items.txt"))

  mockery::stub(
    get_queue_data,
    "queue_obj$get_jobs",
    jobs
  )

  result <- get_queue_data(queue_obj, .user)
  testthat::expect_null(result)
})

testthat::test_that("Returns null when null, jobs in queue for other users only", {
  queue_obj <- list() # RSimpleQueue$new()
  .user <- "Cats_and_Oranges"
  jobs <- dget(file = file.path("test_data/job_list_three_items.txt"))

  mockery::stub(
    get_queue_data,
    "queue_obj$get_jobs",
    jobs
  )

  result <- get_queue_data(queue_obj, .user)
  testthat::expect_null(result)
})

testthat::test_that("Returns queue objects when applicable", {

  queue_obj <- list() # RSimpleQueue$new()
  .user = "Professor Pouch" # Should be two entries
  jobs <- dget(file = file.path("test_data/job_list_nine_items.txt"))

  mockery::stub(
    get_queue_data,
    "queue_obj$get_jobs",
    jobs
  )
  result <- get_queue_data(queue_obj, .user)

  testthat::expect_equal(
    object = nrow(result),
    expected = 2
  )
})

