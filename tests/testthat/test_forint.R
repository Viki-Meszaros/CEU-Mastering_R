library(mr)

test_that("forint() returns string", {
  expect_match(forint(42), "42 HUF")
  expect_error(forint("42"), "Assertion on 'x' failed: Type of input must be 'number'")
})
