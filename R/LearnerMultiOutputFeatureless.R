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
      # Fit a featureless learner for every target separately
      # by converting to a base-task ("regr", "classif", ...).
      mods = imap(task$task_types, function(type, tn) {
        tn = convert_task(task$clone(), target = tn, new_type = type)
        lrn(paste0(type, ".featureless"))$train(tn)
      })
      set_class(list(models = mods, features = task$feature_names), "multiout.featureless_model")
    },
    .predict = function(task) {
      # Predict per-target, aggregate into PredictionMultiOutput
      prds = imap(task$task_types, function(type, tn) {
        t = convert_task(task$clone(), target = tn, new_type = type)
        self$state$model$models[[tn]]$predict(t)
      })
      PredictionMultiOutput$new(task, predictions = prds)
    }
  )
)
