# `testthat` Package Initialization File =======================================

# NOTE: all unit tests can be found under the /testthat subfolder.

library(testthat)
devtools::load_all("pkg")
testthat::test_package("pkg")
