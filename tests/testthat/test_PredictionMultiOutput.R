context("PredictionMultioutput")

test_that("Construction", {
  task = tsk("linnerud")
  prds = setNames(map(task$target_names, function(nm) {
    PredictionRegr$new(row_ids = task$row_ids, truth = task$data(cols = nm)[[1]], response = task$row_ids)
  }), task$target_names)
  p = PredictionMultiOutput$new(row_ids = task$row_ids, predictions = prds)
  expect_prediction_multioutput(p)
})
