#' @title MultiOutput custom aggregation Measure
#'
#' @description
#' Computes an aggregation over measured scores for each target.
#'
#' * measures that should be maximized are automatically multiplied by '-1' internally during
#'   aggregation and `minimize` is therefore set to `TRUE`
#' * `task_type` is set to `"multioutput"`.
#' * Possible values for `predict_type` are all values from `mlr_reflections$learner_predict_types`.
#'   They are currently collected by accessing each [`Measure`]s "predict_type" slot.
#'   Currently limited to only a single 'predict_type' across all measures.
#' * `packages` are all packages required from the supplied measures.
#' * The `range` is automatically computed based on the specified `weights`.
#'
#' Predefined measures can be found in the [mlr3misc::Dictionary] [mlr3::mlr_measures].
#'
#'
#' @seealso
#' Example measures:
#' @export
MeasureMultiOutputCustomAggr = R6Class("MeasureMultiOutputCustomAggr",
  inherit = MeasureMultiOutput,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @template param_measure_multioutput_weightedavg
    #'
    #' @return A [`MeasureMultiOutput`]
    initialize = function(name = "custom_aggregation", measures, aggfun = min) {
      self$measures = map(assert_named(measures, type =  "unique"), assert_measure)
      self$aggfun = assert_function(aggfun)
      super$initialize(
        id = paste0("multioutput.", name),
        range = c(-Inf, Inf),
        minimize = TRUE,
        predict_type = assert_string(unique(map_chr(measures, "predict_type"))),
        packages = assert_character(unique(map_chr(measures, "packages"))),
        properties = unlist(map(measures, "properties")),
        man = paste0("mlr3multioutput::mlr_measures_multioutput.", name)
      )
    },
    #' @description
    #' Returns scores for each measure in self$measures separately.
    #'
    #' @param prediction [`PredictionMultiOutput`]\cr
    #'   Prediction to score.
    #' @param task [`TaskMultiOutput`]\cr
    #'   Task to score.
    #' @param ... (`any`)\cr
    #'   Currently not used.
    #'
    #' @return A `numeric()` vector of scores.
    score_separate = function(prediction, task, ...) {
      wnames = names(self$weights)
      mnames = names(self$measures)
      imap_dbl(prediction$predictions, function(x, nm) {
        if (nm %in% wnames) {
          # Measures is named with targets
          x$score(self$measures[[nm]])
        } else if (all(wnames %in% mlr_reflections$task_types$type)) {
          # Measures is named list with task_types
          x$score(self$measures[[x$task_type]])
        }
      })
    },
    #' @field measures (`list()`)\cr
    #' Access the stored measures.
    measures = NULL,
    #' @field aggfun (`function()`)\cr
    #' Set or get the aggregation function.
    aggfun = NULL
  ),
  private = list(
    .score = function(prediction, task, ...) {
      scores = imap_dbl(prediction$predictions, function(x, nm) {
        if (nm %in% names(self$measures)) {
          # Measures are named with targets
          sc = x$score(self$measures[[nm]])
          if (self$measures[[nm]]$minimize) sc else -sc
        } else {
          # Measures is named list with task_types
          m = self$measures[[x$task_type]]
          if (is.null(m)) {
            warning(paste0("No measure for task_type ", x$task_type, " specified. Returning 0."))
            return(0.)
          }
          sc = x$score(m)
          if (m$minimize) sc else -sc
        }
      })
      self$aggfun(scores)
    }
  )
)
