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

inArg <- list(
    lmpDataList = lmpDL, modelMatrix = mM,
    modelMatrixByEffect = mMBE,
    effectsNamesUnique = eNU,
    effectsNamesAll = eNA
)

res_contrastSS <- contrastSS(inArg)

test_that("contrastSS runs correctly", {
    expect_equal(
        res_contrastSS,
        list(Intercept = c(1, 0, 0), factA = c(0, 1, 0), factB = c(0, 0, 1))
    )
})
