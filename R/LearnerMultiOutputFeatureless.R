#' @title Featureless MultiOutput Learner
#'
#' @name mlr_learners_multiout.featureless
#' @include LearnerMultiOutput.R
#'
#' @description
#' A simple [LearnerMultiOutput] which assigns first n observations to MultiOutputer 1,
#' second n observations to MultiOutputer 2, and so on.
#' Hyperparameter `num.MultiOutputers` controls the number of MultiOutputers.
#'
#' @templateVar id MultiOutput.featureless
#' @template section_dictionary_learner
#'
#' @export
LearnerMultiOutputFeatureless = R6Class("LearnerMultiOutputFeatureless",
  inherit = LearnerMultiOutput,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize(
        id = "multiout.featureless",
        feature_types = mlr_reflections$task_feature_types,
        predict_types = "response",
        param_set = ParamSet$new(),
        properties = c("missings", "twoclass", "multiclass")
      )
    }
  ),
  private = list(
    .train = function(task) {
      pv = self$param_set$get_values(tags = "train")
      n = task$nrow
      if (pv$num.MultiOutputers > n) {
        stop("number of MultiOutputers must lie between 1 and nrow(data)",
          call. = FALSE
        )
      } else if (pv$num.MultiOutputers == n) {
        MultiOutput = seq_along(1:n)
      } else {
        times = c(
          rep.int(n / pv$num.MultiOutputers, pv$num.MultiOutputers - 1),
          n - (pv$num.MultiOutputers - 1) * floor(n / pv$num.MultiOutputers)
        )

        MultiOutput = rep.int(seq_along(1:pv$num.MultiOutputers),
          times = times
        )
      }
      set_class(
        list(MultiOutput = MultiOutput, features = task$feature_names),
        "MultiOutput.featureless_model"
      )
    },
    .predict = function(task) {
      n = task$nrow
      pv = self$param_set$get_values(tags = "train")
      if (n <= pv$num.MultiOutputers) {
        partition = seq_along(1:n)
      } else {
        times = c(
          rep.int(n / pv$num.MultiOutputers, pv$num.MultiOutputers - 1),
          n - (pv$num.MultiOutputers - 1) * floor(n / pv$num.MultiOutputers)
        )

        partition = rep.int(seq_along(1:pv$num.MultiOutputers),
          times = times
        )
      }
      PredictionMultiOutput$new(task = task, partition = partition)
    }
  )
)
