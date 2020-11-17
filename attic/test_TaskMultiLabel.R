test_that("Featureless task", {

  b = as_data_backend(data.table(
    ids = 1:30,
    t1 = as.factor(sample(c(TRUE, FALSE), 30, replace = TRUE)),
    t2 = as.factor(sample(c(TRUE, FALSE), 30, replace = TRUE)),
    t3 = as.factor(sample(c(TRUE, FALSE), 30, replace = TRUE))
  ))
  task = TaskMultiLabel$new(id = "zero_feat_task", b, target = c("t1", "t2", "t3"))
  expect_output(print(task))
  b = task$backend
  expect_backend(b)
  expect_task(task)
  expect_data_table(task$data(), ncols = 2L)
  l = lrn("multioutput.featureless")
  p = l$train(task)$predict(task)
  expect_prediction(p)
})

