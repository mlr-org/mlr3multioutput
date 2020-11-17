#' @title Prediction Object for MultiOutputer Analysis
#'
#' @description
#' This object wraps the predictions returned by a learner of class [LearnerMultiOutput], i.e.
#' the predicted partition and MultiOutputer probability.
#'
#' @family Prediction
#' @export
PredictionMultiOutput = R6Class("PredictionMultiOutput",
  inherit = Prediction,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param task ([TaskMultiOutput])\cr
    #'   Task, used to extract defaults for `row_ids`.
    #'
    #' @param row_ids (`integer()`)\cr
    #'   Row ids of the predicted observations, i.e. the row ids of the test set.
    #' @param predictions (`list()`)\cr
    #'   (Named) list of per-target predictions. Used to construct the `Prediction`-object.
    #'
    #' @param check (`logical(1)`)\cr
    #'   If `TRUE`, performs argument checks and predict type conversions.
    #' @param ... (`list()`)\cr
    #'   (Named) list of per-target truths. Only used for compatibility with `Prediction$new()`.
    initialize = function(task = NULL, row_ids = task$row_ids, predictions = list(), check = TRUE, ...) {
      pdata = list(row_ids = row_ids, predictions = map(predictions, as_prediction_data))
      pdata = discard(pdata, is.null)
      class(pdata) = c("PredictionDataMultioutput", "PredictionData")

      if (check) {
        pdata = check_prediction_data(pdata)
      }

      if (!is.null(task) && !is.null(pdata$predictions)) {
        assert_true(all(names(pdata$predictions) == task$target_names))
      }

      self$task_type = "multioutput"
      self$man = "mlr3multioutput::PredictionMultiOutput"
      self$data = pdata
      self$predict_types = intersect(unique(unlist(lapply(pdata$predictions, names))), c("response", "prob"))
    },
    #' @description
    #' Printer for the Prediction object.
    #'
    #' @param ... (`any`)\cr
    #'   Not used.
    print = function(...) {
      if (length(self$predictions) == 0L) {
        catf("%s for 0 observations", format(self))
      } else {
        data = as.data.table(self)
        catf("%s for %i observations", format(self), nrow(data))
        catf("Targets: %s", paste(names(self$predictions), sep = ","))
        print(data, nrows = 10L, topn = 3L, class = FALSE, row.names = FALSE, print.keys = FALSE)
      }
    },
    #' @description
    #' Returns scores for each measure separately.
    #'
    #' @param measures `list`\cr
    #'   List of [`MeasureMultiOutput`] to score.
    #' @param task [`TaskMultiOutput`]\cr
    #'   Task to use for scoring
    #'
    #' @return A `numeric()` vector of scores.
    score_separate = function(measures, task) {
      map(measures, assert_measure)
      if (!missing(task)) assert_task(task)
      imap(measures, function(x) {
        x$score_separate(as_prediction(self$predictions), task)
      })
    }
  ),
  active = list(
    #' @field predictions (`list()`)\cr
    #' Access the stored predictions.
    predictions = function() {
      map(self$data$predictions, as_prediction) %??% rep(NA_real_, length(self$data$row_ids))
    },

    #' @field missing (`integer()`)\cr
    #' Returns `row_ids` for which the predictions are missing or incomplete.
    missing = function() {
      unique(unlist(map(self$data$predictions, is_missing_prediction_data)))
    },
    #' @field row_ids (`integer()`)\cr
    #' Access the stored row_ids.
    row_ids = function(rhs) {
      if (!missing(rhs)) stopf("Field/Binding is read-only")
      self$data$predictions[[1]]$row_ids
    }
  )
)


#' @export
as.data.table.PredictionMultiOutput = function(x, ...) { #nolint
  cbind(
    "row_id" = x$row_ids,
    imap_dtc(x$predictions, function(x, n) {
      dt = as.data.table(x)[, row_id := NULL]
  }))
}
