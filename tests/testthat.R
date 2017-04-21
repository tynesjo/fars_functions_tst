# `testthat` Package Initialization File =======================================

# NOTE: all unit tests can be found under the /testthat subfolder.

library(testthat)
load_all("pkg")
test_package("pkg")
