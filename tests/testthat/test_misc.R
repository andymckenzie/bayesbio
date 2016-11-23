context("running mgsub test")

test_that("mgsub avoids recursive overlap problems", {

  asdf = c(4, 0, 1, 1, 3, 0, 2, 0, 1, 1)

  res = mgsub(c("0", "1", "2"), c("10", "11", "12"), asdf)

  expect_equal(res, c("4", "10", "11", "11", "3", "10", "12", "10", "11", "11"))

})

test_that("makeMatSym matrix works as expected on square matrices", {

  mat_test = matrix(1:9, nrow = 3)
  mat_test_upper = makeMatSym(mat_test, replaceUpper = TRUE)
  expect_equal(mat_test_upper[1,2], 4)
  mat_test_lower = makeMatSym(mat_test, replaceUpper = FALSE)
  expect_equal(mat_test_lower[2,1], 2)

})
