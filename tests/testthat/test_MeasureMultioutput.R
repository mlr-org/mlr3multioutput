context("MeasureMultioutput - All measures")

test_that("Multioutput measures", {
  keys = mlr_measures$keys("multioutput")
  task = tsk("linnerud")
  learner = mlr_learners$get("multioutput.featureless")
  learner$train(task)
  p = learner$predict(task)
  for (key in keys) {
    m = mlr_measures$get(key)
    if (m$task_type == "multioutput") {
      perf = m$score(prediction = p, task = task, learner = learner)
      expect_number(perf, na.ok = FALSE, lower = m$range[1], upper = m$range[2])
    }
  }
})
