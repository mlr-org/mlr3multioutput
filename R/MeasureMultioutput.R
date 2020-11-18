#' @title Multioutput Measure
#'
#' @description
#' This measure specializes [mlr3::Measure] for Multioutput Task:
#'
#' * `task_type` is set to `"multioutput"`.
#' * Possible values for `predict_type` are all values from `mlr_reflections$learner_predict_types`.
#'
#' Predefined measures can be found in the [mlr3misc::Dictionary] [mlr3::mlr_measures].
#'
#' @template param_id
#' @template param_range
#' @template param_minimize
#' @template param_average
#' @template param_aggregator
#' @template param_predict_type
#' @template param_measure_properties
#' @template param_predict_sets
#' @template param_task_properties
#' @template param_packages
#' @template param_man
#'
#' @seealso
#' Example measures:
#' msr("multioutput.custom")
#' msr("multioutput.customaggr")
#' @export
MeasureMultioutput = R6Class("MeasureMultioutput",
  inherit = Measure, cloneable = FALSE,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    initialize = function(id, range, minimize = NA, aggregator = NULL, properties = character(),
      predict_type = "response", task_properties = character(),
      packages = character(), man = NA_character_) {
      super$initialize(id,
        task_type = "multioutput", range = range, minimize = minimize,
        aggregator = aggregator, properties = properties,
        predict_type = predict_type, task_properties = task_properties,
        packages = packages, man = man
      )
    }
  )
)

get_default_measures = function() {
  map(mlr_reflections$default_measures[which(!(names(mlr_reflections$default_measures) == "multioutput"))], msr)
}