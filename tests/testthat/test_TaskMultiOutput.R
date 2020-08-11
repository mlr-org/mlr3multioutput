context("TaskMultiOutput")

test_that("Basic ops on linnerud task", {
  task = tsk("linnerud")
  expect_task(task)
  expect_identical(task$target_names, c("Pulls", "Squats", "Jumps"))
})

test_that("0 feature task", {
  b = as_data_backend(data.table(ids = 1:30, "t" = rnorm(30)))
  task = TaskMultiOutput$new(id = "zero_feat_task", b, target = "t")
  expect_output(print(task))
  b = task$backend
  expect_backend(b)
  expect_task(task)
  expect_data_table(task$data(), ncols = 2L)
  l = lrn("multiout.featureless")
  p = l$train(task)$predict(task)
  expect_prediction(p)
})

