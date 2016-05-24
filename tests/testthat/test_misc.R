context("running mgsub test")

test_that("mgsub avoids recursive overlap problems", {

  asdf = c(4, 0, 1, 1, 3, 0, 2, 0, 1, 1)

  res = mgsub(c("0", "1", "2"), c("10", "11", "12"), asdf)

  expect_equal(res, c("4", "10", "11", "11", "3", "10", "12", "10", "11", "11"))

})
