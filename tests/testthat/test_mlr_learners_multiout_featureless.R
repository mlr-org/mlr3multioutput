context("mlr_learners_multiout_featureless")

test_that("autotest", {
  learner = lrn("multioutput.featureless")
  expect_learner(learner)
  result = run_autotest(learner)
  expect_true(result, info = result$error)
})

test_that("Learner properties are respected", {
  task = tsk("linnerud")
  learner = mlr_learners$get("multioutput.featureless")
  expect_learner(learner, task)
  p = learner$train(task)$predict(task)
  expect_prediction_multioutput(p)
})
