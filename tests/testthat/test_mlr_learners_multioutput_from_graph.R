context("mlr_learners_multioutput_graphs")

test_that("graph with list of prd output", {
  task = tsk("linnerud")
  gr = po("multioutputsplit") %>>%
    po("multiplicityexply", outnum = 3) %>>%
    gunion(list(
      Pulls = lrn("regr.rpart", id = "rp1"),
      Squats = lrn("regr.rpart", id = "rp2"),
      Jumps = lrn("regr.rpart", id = "rp3")
   )) %>>%
   po("multiplicityimply", innum = task$target_names) %>>%
   po("multioutputunite")
  gr$train(task)
  prds = gr$predict(task)
  expect_prediction_multioutput(prds[[1]])

  gl = GraphLearner$new(gr)
  expect_learner(gl, task = task)
  gl$train(task)
  prds = gl$predict(task)
  expect_prediction_multioutput(prds)
})

test_that("graph with sme-target-type approach", {
  task = tsk("linnerud")
  gr = po("multioutputsplit") %>>%
    lrn("regr.rpart") %>>%
    po("multioutputunite")
  gr$train(task)
  prds = gr$predict(task)
  expect_prediction_multioutput(prds[[1]])

  gl = GraphLearner$new(gr)
  expect_learner(gl, task = task)
  gl$train(task)
  prds = gl$predict(task)
  expect_prediction_multioutput(prds)
})
