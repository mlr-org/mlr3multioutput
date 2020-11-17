lapply(list.files(system.file("testthat", package = "mlr3"), pattern = "^helper.*\\.[rR]", full.names = TRUE), source)

generate_tasks.LearnerMultioutput = function(learner, N = 20L) { # nolint
  set.seed(1)
  # 2 classif 1 regr target
  d = list(
    "2d" = mlbench::mlbench.2dnormals(N, cl = 2, r = 2, sd = 0.1),
    "wav" = mlbench::mlbench.waveform(N)
  )
  dt = cbind(map_dtc(d, "x"), map_dtc(d, "classes"))
  set(dt, j = "t3", value = rnorm(N, sd = .1))
  task = TaskMultioutput$new("sanity", dt, target = c("X2d", "wav", "t3"))
  list(task)
}

registerS3method("generate_tasks", "LearnerMultioutput", generate_tasks.LearnerMultioutput,
  envir = parent.frame()
)

sanity_check.PredictionMultioutput = function(prediction, task, ...) { # nolint
  prediction$score(measures = msr("multioutput.default"), task = task) > 0
}

registerS3method("sanity_check", "PredictionMultioutput", sanity_check.PredictionMultioutput,
  envir = parent.frame()
)


expect_prediction_multioutput = function(p) {
  with(.GlobalEnv, expect_prediction)(p)
  checkmate::expect_r6(p, "PredictionMultioutput",
    public = c("row_ids", "truth", "predict_types")
  )
}