context("test_LearnerMultiOutput")

test_that("predict on newdata works / multiout", {
  task = tsk("linnerud")$filter(1:10)
  learner = lrn("multiout.featureless")
  expect_error(learner$predict(task), "trained")
  learner$train(task)
  expect_task(learner$state$train_task)
  newdata = tsk("linnerud")$filter(11:21)$data()

  # passing the task
  p = learner$predict_newdata(newdata = newdata, task = task)
  expect_data_table(as.data.table(p), nrows = 10)
  expect_set_equal(as.data.table(p)$row_id, 1:10)
  expect_null(p$truth)

  # rely on internally stored task representation
  p = learner$predict_newdata(newdata = newdata, task = NULL)
  expect_data_table(as.data.table(p), nrows = 10)
  expect_set_equal(as.data.table(p)$row_id, 1:10)
  expect_null(p$truth)
})

test_that("reset()", {
  task = tsk("linnerud")
  lrn = lrn("multiout.featureless")

  lrn$train(task)
  expect_list(lrn$state, names = "unique")
  expect_learner(lrn$reset())
  expect_null(lrn$state)
})

test_that("empty predict set (#421)", {
  task = tsk("linnerud")
  learner = lrn("multiout.featureless")
  resampling = rsmp("holdout", ratio = 1)
  hout = resampling$instantiate(task)
  model = learner$train(task, hout$train_set(1))
  pred = learner$predict(task, hout$test_set(1))
  expect_true(any(grepl("No data to predict on", learner$log$msg)))
})
