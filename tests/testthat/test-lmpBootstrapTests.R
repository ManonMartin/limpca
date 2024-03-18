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
eM <- list(
    Intercept = matrix(c(
        rep(0.7, 10),
        rep(0.9, 10), rep(0.5, 10)
    ), ncol = 3),
    factA = matrix(rnorm(30), ncol = 3),
    factB = matrix(rnorm(30), ncol = 3)
)
err <- matrix(rnorm(30, sd = 0.2), ncol = 3)
pv <- outcomes + err
residuals <- outcomes - err
parameters <- matrix(rnorm(9),
    nrow = 3, ncol = 3,
    dimnames = list(c("(Intercept)", "factA2", "factB2"), NULL)
)
type3SS <- c(Intercept = 100, factA = 68, factB = 50, Residuals = 13)
vP <- c(factA = 68, factB = 10, Residuals = 22)
vPP <- NULL

resLmpEffectMatrices <- list(
    lmpDataList = lmpDL,
    modelMatrix = mM,
    modelMatrixByEffect = mMBE,
    effectsNamesUnique = eNU,
    effectsNamesAll = eNA,
    effectMatrices = eM,
    predictedvalues = pv,
    residuals = residuals,
    parameters = parameters,
    type3SS = type3SS,
    variationPercentages = vP,
    varPercentagesPlot = vPP
)
set.seed(2)
res_BT <- lmpBootstrapTests(resLmpEffectMatrices = resLmpEffectMatrices, nboot = 2)

test_that("lmpBootstrapTests runs correctly", {
    expect_type(res_BT, "list")
    expect_length(res_BT, 4)
    expect_equal(res_BT$f.obs, c(factA = 36.615385, factB = 26.923077))
})
