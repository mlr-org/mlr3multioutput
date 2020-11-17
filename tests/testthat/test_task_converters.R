context("task_converters")

test_that("Task converters regr", {
  t = tsk("linnerud")
  tn = as_task_regr(t, target = "Pulls")
  expect_task_regr(tn)
  expect_true(all(tn$feature_types == t$feature_types))
  expect_true(tn$target_names == "Pulls")
})

test_that("Task converters classif", {
  t = generate_tasks(lrn("multioutput.featureless"))[[1]]
  tn = as_task_classif(t, target = "X2d")
  expect_task_classif(tn)
  expect_true(all(tn$feature_types == t$feature_types))
  expect_true(tn$target_names == "X2d")
})
