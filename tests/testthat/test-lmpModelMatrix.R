set.seed(1)
design <- data.frame(
  factA = factor(c(rep(1, 5), rep(2, 5))),
  factB = factor(rep(c(1, 2), 5))
)
outcomes <- matrix(rnorm(30), ncol = 3)
formula <- "~ factA+factB"
lmpDL <- list(design = design, outcomes = outcomes, formula = formula)

resLMM <- lmpModelMatrix(lmpDL)

test_that("lmpModelMatrix runs correctly", {
  expect_type(resLMM, "list")
  expect_length(resLMM, 5)
  expect_equal(resLMM$modelMatrix[, "factA1"],
    c(1, 1, 1, 1, 1, -1, -1, -1, -1, -1),
    ignore_attr = "names"
  )
  expect_equal(
    as.vector(resLMM$modelMatrixByEffect$factA),
    c(1, 1, 1, 1, 1, -1, -1, -1, -1, -1)
  )
  expect_equal(resLMM$effectsNamesUnique, c("Intercept", "factA", "factB"))
  expect_equal(resLMM$effectsNamesAll, c("Intercept", "factA", "factB"))
})
