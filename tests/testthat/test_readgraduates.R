context("Reading Names from Text File")

test_that("readgraduates outputs a name list", {
  expect_that(readgraduates(2015), is_a("data.frame"))
})
