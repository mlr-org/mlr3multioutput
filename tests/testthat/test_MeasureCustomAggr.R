context("MeasureMultioutput - Custom Aggregation")

test_that("Multioutput measures", {
  msrs = list("regr" = msr("regr.mse"), "classif" = msr("classif.ce"))
  ms = list(
    MeasureMultioutputCustomAggr$new(measures = msrs),
    MeasureMultioutputCustomAggr$new(measures = msrs, aggfun = mean),
    MeasureMultioutputCustomAggr$new(measures = msrs, aggfun = sum),
    MeasureMultioutputCustomAggr$new(measures = msrs, aggfun = max)
  )
  tsks = list(tsk("linnerud"), tsk("flags"))
  for (task in tsks) {
    learner = mlr_learners$get("multioutput.featureless")
    learner$train(task)
    p = learner$predict(task)
    for (m in ms) {
      perf = m$score(prediction = p, task = task, learner = learner)
      expect_number(perf, na.ok = FALSE, lower = m$range[1], upper = m$range[2])
  }}
})
