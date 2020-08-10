#' @title MultiOutput Learner
#'
#' @description
#' This Learner specializes [mlr3::Learner] for MultiOuter problems:
#' * `task_type` is set to `"MultiOut"`.
#' * Creates [Prediction]s of class [PredictionMultiOut].
#' * Possible values for `predict_types` are all `predict_types` available
#'   for other `supervised` Tasks.
#'
#' Predefined learners can be found in the [mlr3misc::Dictionary] [mlr3::mlr_learners].
#'
#' @template param_id
#' @template param_param_set
#' @template param_predict_types
#' @template param_feature_types
#' @template param_learner_properties
#' @template param_data_formats
#' @template param_packages
#' @template param_man
#'
#' @export
#' @examples
#' library(mlr3)
#' ids = mlr_learners$keys("^MultiOutput")
#' ids
#'
#' # get a specific learner from mlr_learners:
LearnerMultiOutput = R6Class("LearnerMultiOutput",
  inherit = Learner,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id, param_set = ParamSet$new(), predict_types = "response",
      feature_types = character(), properties = character(),
      packages = character()) {
      super$initialize(
        id = id, task_type = "multiout", param_set = param_set,
        predict_types = predict_types,
        feature_types = feature_types, properties = properties, packages = packages
      )
    }
  )
)
