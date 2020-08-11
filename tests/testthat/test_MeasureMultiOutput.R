context("MeasureMultiOutput")

test_that("Multioutput measures", {
  keys = mlr_measures$keys("multiout")
  task = tsk("linnerud")
  learner = mlr_learners$get("multiout.featureless")
  learner$train(task)
  p = learner$predict(task)
  for (key in keys) {
    m = mlr_measures$get(key)
    if (m$task_type == "multiout") {
      perf = m$score(prediction = p, task = task, learner = learner)
      expect_number(perf, na.ok = FALSE, lower = m$range[1], upper = m$range[2])
    }
  }
})
