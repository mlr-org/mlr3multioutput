library(mlr3)
library(mlr3multioutput)
library(checkmate)
library(testthat)

lapply(list.files(system.file("testthat", package = "mlr3"),
  pattern = "^helper.*\\.[rR]$", full.names = TRUE
), source)

generate_tasks.LearnerMultiOutput = function(learner, N = 20L) { # nolint
  set.seed(1)
  # 2 classif 1 regr target
  d = list(
    "2d" = mlbench::mlbench.2dnormals(N, cl = 2, r = 2, sd = 0.1),
    "wav" = mlbench::mlbench.waveform(N)
  )
  dt = cbind(map_dtc(d, "x"), map_dtc(d, "classes"))
  set(dt, j = "t3", value = rnorm(N, sd = .1))
  task = TaskMultiOutput$new("sanity", dt, target = c("X2d", "wav", "t3"))
  list(task)
}

registerS3method("generate_tasks", "LearnerMultiOutput", generate_tasks.LearnerMultiOutput,
  envir = parent.frame()
)

sanity_check.PredictionMultiOutput = function(prediction, task, ...) { # nolint
  prediction$score(measure = msr("multiout.default"), task = task) > 0
}

registerS3method("sanity_check", "PredictionMultiOutput", sanity_check.PredictionMultiOutput,
  envir = parent.frame()
)


expect_prediction_multiout = function(p) {
  expect_prediction(p)
  checkmate::expect_r6(p, "PredictionMultiOutput",
    public = c("row_ids", "truth", "predict_types")
  )
}