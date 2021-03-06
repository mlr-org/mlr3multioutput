context("TaskMultioutput")

test_that("Basic ops on linnerud task", {
  task = tsk("linnerud")
  expect_task(task)
  expect_identical(task$target_names, c("Pulls", "Squats", "Jumps"))
})

test_that("Featureless task", {
  b = as_data_backend(data.table(ids = 1:30, "t" = rnorm(30)))
  task = TaskMultioutput$new(id = "zero_feat_task", b, target = "t")
  expect_output(print(task))
  b = task$backend
  expect_backend(b)
  expect_task(task)
  expect_data_table(task$data(), ncols = 2L)
  l = lrn("multioutput.featureless")
  p = l$train(task)$predict(task)
  expect_prediction(p)
})
