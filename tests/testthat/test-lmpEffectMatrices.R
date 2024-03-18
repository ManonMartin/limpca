set.seed(1)
design <- data.frame(
    factA = factor(c(rep(1, 5), rep(2, 5))),
    factB = factor(rep(c(1, 2), 5))
)
outcomes <- matrix(rnorm(30), ncol = 3)
formula <- "~ factA+factB"
lmpDL <- list(design = design, outcomes = outcomes, formula = formula)
mM <- model.matrix(as.formula(lmpDL$formula), lmpDL$design)
mMBE <- list(
    Intercept = mM[, "(Intercept)", drop = FALSE],
    factA = mM[, "factA2", drop = FALSE],
    factB = mM[, "factB2", drop = FALSE]
)
eNU <- eNA <- c("Intercept", "factA", "factB")


resLmpModelMatrix <- list(
    lmpDataList = lmpDL,
    modelMatrix = mM,
    modelMatrixByEffect = mMBE,
    effectsNamesUnique = eNU,
    effectsNamesAll = eNA
)

res_EM <- lmpEffectMatrices(resLmpModelMatrix)


test_that("lmpEffectMatrices runs correctly with defaults", {
    expect_type(res_EM, "list")
    expect_length(res_EM, 12)
    expect_equal(res_EM$effectMatrices$Intercept, matrix(c(
        rep(0.008029017, 10),
        rep(0.3359562, 10),
        rep(0.3200447, 10)
    ), ncol = 3),
    tolerance = 1e-06, ignore_attr = "dimnames"
    )
    expect_equal(res_EM$predictedvalues[1, 1], 0.008029017, ignore_attr = "names")
    expect_equal(res_EM$parameters[2, 1], c(factA2 = -0.054754666))
    expect_equal(res_EM$type3SS["factA"], c(factA = 1.0191787))
    expect_equal(res_EM$variationPercentages["factA"],
        c(factA = 3.877278),
        tolerance = 1e-06
    )
})
