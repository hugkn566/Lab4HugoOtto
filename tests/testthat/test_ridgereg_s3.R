context("ridgereg")

data("iris")

test_that("linreg rejects errounous input", {
  expect_error(ridgereg(Petal.Length~Sepdsal.Width+Sepal.Length, data=iris))
  expect_error(ridgereg(Petal.Length~Sepdsal.Width+Sepal.Length, data=irfsfdis))
})


test_that("output is similar to lm.ridge", {
  ridgereg_mod <- ridgereg(Petal.Length~Species, data=iris)
  lm.ridge_mod <- MASS::lm.ridge(Petal.Length~Species, data=iris)
  expect_equal(round(ridgereg_mod$coef[2], digits = 2), round((lm.ridge_mod$coef[1] / lm.ridge_mod$scales[1]), digits = 2))
  expect_equal(round(ridgereg_mod$coef[3], digits = 2), round((lm.ridge_mod$coef[2] / lm.ridge_mod$scales[2]), digits = 2))
})

