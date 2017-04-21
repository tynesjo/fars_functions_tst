make_filename <- function(year) 

# FARS Code Tests ==========================================================

context("File Loading")


testthat::test_that("make_filename", {
  result <- farsfun::make_filename(2017)
  testthat::expect_is(result, "character")
  testthat::expect_true(length(result) == 1)
})
