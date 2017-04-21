make_filename <- function(year) 

# FARS Code Tests ==========================================================

context("File Loading")


# ------------------------------------------------------------------------------


"make_filename" %>%
test_that({
  result <- farsfun::make_filename(2017)
  testthat::expect_is(result, "character")
  testthat::expect_true(length(result) == 1)
})
