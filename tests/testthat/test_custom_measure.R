context("Custom Measures")

test_that("Can create custom measure", {
  cl = msr("classif.ce")
  rg = msr("regr.mse")

  m1 = msr("multiout.custom", measures = list(classif = cl, regr = rg))
  m2 = msr("multiout.custom", measures = list(classif = cl, regr = rg), name = "test")
  m3 = msr("multiout.custom", measures = list(regr = rg))
  m4 = msr("multiout.custom",
    measures = list(classif = cl, regr = rg),
    weights = c(X2d = 1, wav = 0, t3 = 0)
  )
  m4 = msr("multiout.custom",
    measures = list(classif = cl, regr = rg),
    weights = c(X2d = 1, wav = 0, t3 = 0)
  )
  m5 = MeasureMultiOutputWeightedAvg$new(
    measures = list(classif = cl, regr = rg),
    weights = c(classif = 1, regr = 0)
  )
  m6 = MeasureMultiOutputWeightedAvg$new(
    measures = list(X2d = cl, wav = msr("classif.bacc"), t3 = rg),
    weights = c(classif = 1, regr = 0)
  )
  t = generate_tasks(lrn("multiout.featureless"))[[1]]
  p = lrn("multiout.featureless")$train(t)$predict(t)
  expect_number(p$score(m1), lower = 0, upper = 1)
  expect_number(p$score(m2), lower = 0, upper = 1)
  expect_warning(expect_number(p$score(m3), lower = 0, upper = 1))
  s4 = p$score(m4)
  expect_number(s4, lower = 0, upper = 1)
  expect_true(s4 == min(prop.table(table(t$data(cols = "X2d")[[1]]))))
  expect_number(p$score(m5), lower = 0, upper = 1)
  expect_number(p$score(m6), lower = 0, upper = Inf)
})