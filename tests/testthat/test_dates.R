context("running time/date tests")

df1 = data.frame(a = as.POSIXct(c("2010/05/01", "2010/05/10", "2010/05/30")), b = c(3, 4, 5))
df2 = data.frame(c = as.POSIXct(c("2010/05/02", "2010/05/03", "2010/05/23", "2010/05/22")), d = c(1, 2, 3, 4))

merged_df = nearestTime(df1, df2, "a", "c")

test_that("nearestTime works as expected on input", {

  expect_equal(colnames(merged_df), c("a", "b", "d"))

  expect_equal(merged_df$d, c(1, 2, 3))

})
