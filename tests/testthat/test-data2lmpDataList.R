set.seed(1)
design <- data.frame(
  factA = factor(c(rep(1, 5), rep(2, 5))),
  factB = factor(rep(c(1, 2), 5))
)
rownames(design) = as.character(seq_len(10))

outcomes <- matrix(rnorm(30), ncol = 3)
rownames(outcomes) = seq_len(10)
formula <- "~ factA+factB"

se_UCH = SummarizedExperiment(assays = list(counts = t(outcomes),
                                            counts2 = t(outcomes*2)),
                              colData = design,
                              metadata = list(formula = formula))


res = data2lmpDataList(se_UCH)

test_that("data2lmpDataList based on SE runs correctly", {
  expect_type(res, "list")
  expect_length(res, 3)
  expect_equal(res$design, design)
  expect_equal(res$outcomes, outcomes)
  expect_equal(as.formula(res$formula), as.formula(formula))
})

res = data2lmpDataList(se_UCH, assay_name = "counts2")

test_that("data2lmpDataList based on SE runs correctly", {
  expect_type(res, "list")
  expect_length(res, 3)
  expect_equal(res$design, design)
  expect_equal(res$outcomes, outcomes*2)
  expect_equal(as.formula(res$formula), as.formula(formula))
})


res = data2lmpDataList(outcomes = outcomes, design = design, formula = formula)

test_that("data2lmpDataList manually defined runs correctly", {
  expect_type(res, "list")
  expect_length(res, 3)
  expect_equal(res$design, design)
  expect_equal(res$outcomes, outcomes)
  expect_equal(as.formula(res$formula), as.formula(formula))
})


res = data2lmpDataList(se_UCH, assay_name = "counts",
                       outcomes = outcomes*4)

test_that("data2lmpDataList defined both manually and based on SE runs correctly", {
  expect_type(res, "list")
  expect_length(res, 3)
  expect_equal(res$design, design)
  expect_equal(res$outcomes, outcomes*4)
  expect_equal(as.formula(res$formula), as.formula(formula))
})




test_that("data2lmpDataList with sampled rownames of design runs correctly", {
  expect_warning(
    data2lmpDataList(outcomes = outcomes[sample(1:10,replace = FALSE),],
                     design = design, formula = formula, verbose = FALSE)
  )
})

### misspecified

test_that("data2lmpDataList with misspecified arguments fails", {
  expect_error(data2lmpDataList(se = outcomes))
  expect_error(data2lmpDataList(se_UCH, assay_name = "aaa"))
  expect_error(data2lmpDataList(outcomes = outcomes[-1,],
                                design = design, formula = formula))
  expect_error(data2lmpDataList())
  expect_error(data2lmpDataList(design = design, formula = formula))
  expect_error(data2lmpDataList(outcomes = outcomes, formula = formula))
  expect_error(data2lmpDataList(outcomes = outcomes, design = design))
})


test_that("data2lmpDataList with misspecified design rownames fails", {
  design2 = design
  rownames(design2)[1:2] = c("aaa", "bbb")
  expect_error(data2lmpDataList(outcomes = outcomes,
                                design = design2, formula = formula))
})


test_that("data2lmpDataList with no outcomes rownames fails", {
  outcomes2 = outcomes
  rownames(outcomes2) = NULL
  expect_error(data2lmpDataList(outcomes = outcomes2,
                                design = design, formula = formula))
})
