#' @title MultiOutput weighted average Measure
#'
#' @description
#' Computes a weighted average over measured scores for each target.
#'
#' * `task_type` is set to `"multiout"`.
#' * Possible values for `predict_type` are all values from `mlr_reflections$learner_predict_types`.
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
        range = c(0, Inf),
        minimize = assert_flag(unique(map_lgl(measures, "minimize"))),
        predict_type = assert_string(unique(map_chr(measures, "predict_type"))),
        packages = assert_string(unique(map_chr(measures, "packages"))),
        properties = unlist(map(measures, "properties")),
        man = paste0("mlr3multioutput::mlr_measures_multiout.", name)
      )
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
          if (is.null(wnames)) 1      # No weights
          else self$weights[[x$task_type]] # weights by task_type
        })
      } else {
        wts = self$weights
      }

      mnames = names(self$measures)
      scores = imap_dbl(prediction$predictions, function(x, nm) {
        if (nm %in% wnames) {
          # Measures is list named with targets
          x$score(self$measures[[nm]])
        } else if (all(wnames %in% mlr_reflections$task_types$type)) {
          # Measures is named list with task_types
          x$score(self$measures[[x$task_type]])
        }
      })
      weighted.mean(scores[names(wts)], wts)
    }
  )
)