#' @title Multioutput Learner
#'
#' @description
#' This Learner specializes [mlr3::Learner] for Multioutputer problems:
#' * `task_type` is set to `"Multioutput"`.
#' * Creates [Prediction]s of class [PredictionMultioutput].
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
#' ids = mlr_learners$keys("^Multioutput")
#' ids
#'
#' # get a specific learner from mlr_learners:
LearnerMultioutput = R6Class("LearnerMultioutput",
  inherit = Learner,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id, param_set = ParamSet$new(), predict_types = character(),
      feature_types = character(), properties = character(),
      packages = character()) {
      super$initialize(
        id = id, task_type = "multioutput", param_set = param_set,
        predict_types = predict_types,
        feature_types = feature_types, properties = properties, packages = packages
      )
    }
  )
)
