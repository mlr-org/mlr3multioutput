context("mlr_pipeops_multioutsplit")

test_that("PipeOpSplitMultiout for linnerud", {
  task = tsk("linnerud")
  p = PipeOpSplitMultiout$new()

  ot = p$train(list(task))
  expect_list(ot, types = "Multiplicity")
  map(ot$output, expect_task)
  expect_true(all(names(ot$output) == task$target_names))

  op = p$predict(list(task))
  expect_list(op, types = "Multiplicity")
  map(op$output, expect_task)
  expect_true(all(names(op$output) == task$target_names))

  gr = po("multioutsplit") %>>% po("multiplicityexply", outnum = 3)
  map(gr$train(task), expect_task)
})

test_that("PipeOpTargetSplit for generated task", {
  task = generate_tasks(lrn("multioutput.featureless"))[[1]]
  p = PipeOpSplitMultiout$new()

  ot = p$train(list(task))
  expect_list(ot, types = "Multiplicity")
  map(ot$output, expect_task)
  expect_true(all(names(ot$output) == task$target_names))

  op = p$predict(list(task))
  expect_list(op, types = "Multiplicity")
  map(op$output, expect_task)
  expect_true(all(names(op$output) == task$target_names))
})
