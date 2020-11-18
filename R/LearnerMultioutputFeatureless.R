#' @title Featureless Multioutput Learner
#'
#' @name mlr_learners_multioutput.featureless
#' @include LearnerMultioutput.R
#'
#' @description
#' A simple [LearnerMultioutput] which assigns first n observations to Multioutputer 1,
#' second n observations to Multioutputer 2, and so on.
#' Hyperparameter `num.Multioutputers` controls the number of Multioutputers.
#'
#' @templateVar id Multioutput.featureless
#' @template section_dictionary_learner
#'
#' @export
LearnerMultioutputFeatureless = R6Class("LearnerMultioutputFeatureless",
  inherit = LearnerMultioutput,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function() {
      super$initialize(
        id = "multioutput.featureless",
        feature_types = mlr_reflections$task_feature_types,
        predict_types = "response",
        param_set = ParamSet$new(),
        properties = c("missings", "twoclass", "multiclass", "multilabel", "multioutput")
      )
    }
  ),
  private = list(
    .train = function(task) {
      # Fit a featureless learner for every target separately
      # by converting to a base-task ("regr", "classif", ...).
      tsks = convert_to_basic_tasks(task)
      mods = imap(task$task_types, function(type, tn) {
        lrn(paste0(type, ".featureless"))$train(tsks[[tn]])
      })
      set_class(list(models = mods, features = task$feature_names), "multioutput.featureless_model")
    },
    .predict = function(task) {
      # Predict per-target, aggregate into PredictionMultioutput
      prds = imap(task$task_types, function(type, tn) {
        t = convert_task(task$clone(), target = tn, new_type = type)
        self$state$model$models[[tn]]$predict(t)
      })
      PredictionMultioutput$new(task, predictions = prds)
    }
  )
)
