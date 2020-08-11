context("task_converters")

test_that("Task converters", {
  t = tsk("linnerud")
  tn = as_task_regr(t, target = "Pulls")
  assert_task(tn, task_type = "regr")
  assert_true(all(tn$feature_types == t$feature_types))
  assert_true(tn$target_names == "Pulls")
})
