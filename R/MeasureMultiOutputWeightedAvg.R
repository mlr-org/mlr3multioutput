#' @title MultiOutput weighted average Measure
#'
#' @description
#' Computes a weighted average over measured scores for each target.
#'
#' * measures that should be maximized are automatically multiplied by '-1' internally during aggregation
#'   and `minimize` is therefore set to `TRUE`
#' * `task_type` is set to `"multiout"`.
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
MeasureMultiOutputWeightedAvg = R6Class("MeasureMultiOutputWeightedAvg",
  inherit = MeasureMultiOutput,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @template param_measure_multioutput_weightedavg
    #'
    #' @return A [`MeasureMultiOutput`]
    initialize = function(name = "weightedavg", measures, weights = NULL) {
      self$measures = map(assert_named(measures, type =  "unique"), assert_measure)
      self$weights = assert_numeric(weights, null.ok = TRUE)
      super$initialize(
        id = paste0("multiout.", name),
        range = private$.compute_range(),
        minimize = TRUE,
        predict_type = assert_string(unique(map_chr(measures, "predict_type"))),
        packages = assert_character(unique(map_chr(measures, "packages"))),
        properties = unlist(map(measures, "properties")),
        man = paste0("mlr3multioutput::mlr_measures_multiout.", name)
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
    #' @field weights (`numeric()`)\cr
    #' Access the stored weights.
    weights = NULL
  ),
  private = list(
    .score = function(prediction, task, ...) {
      wnames = names(self$weights)
      if (all(wnames %in% mlr_reflections$task_types$type) || is.null(wnames)) {
        wts = imap_dbl(prediction$predictions, function(x, nm) {
          if (is.null(wnames)) 1           # No weights
          else self$weights[[x$task_type]] # weights by task_type
        })
      } else {
        wts = self$weights
      }

      mnames = names(self$measures)
      scores = imap_dbl(prediction$predictions, function(x, nm) {
        if (nm %in% wnames) {
          # Measures is named with targets
          sc = x$score(self$measures[[nm]])
        } else if (all(wnames %in% mlr_reflections$task_types$type)) {
          # Measures is named list with task_types
          sc = x$score(self$measures[[x$task_type]])
        }
        if (self$measures[[x$task_type]]$minimize) sc else -sc
      })

      weighted.mean(scores[names(wts)], wts)
    },
    .compute_range = function() {
      if (length(self$weights) == length(self$measures))
        wts == self$weights
      else (is.null(self$weights))
        wts = rep(1, length(self$measures))
      c(
        min(map_dbl(self$measures, function(x) x$range[[1]]) * wts),
        max(map_dbl(self$measures, function(x) x$range[[2]]) * wts)
      )

    }
  )
)
