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
    #'
    #' @param predictions (`list()`)\cr
    #'   (Named) list of per-target predictions. Used to construct the `Prediction`-object.
    initialize = function(task = NULL, row_ids = task$row_ids, predictions = NULL) {
      assert_true(all(names(predictions) == task$target_names))
      if (length(row_ids) > 0L) {
        assert_true(all.equal(
          Reduce(function(x,y) if (identical(x,y)) x else FALSE,  map(predictions, "row_ids")),
          assert_row_ids(row_ids)
        ))
      }
      self$predict_types = unique(map_chr(predictions, "predict_types"))
      self$task_type = "multiout"
      self$data$predictions = map(predictions, assert_prediction)
    },
    #' @description
    #' Printer for the Prediction object.
    #'
    #' @param ... (`any`)\cr
    #'   Not used.
    print = function(...) {
      if (!nrow(self$predictions[[1]]$data$tab)) {
        catf("%s for 0 observations", format(self))
      } else {
        data = as.data.table(self)
        catf("%s for %i observations", format(self), nrow(data))
        catf("Targets: %s", paste(names(self$data$predictions), sep = ","))
        print(data, nrows = 10L, topn = 3L, class = FALSE, row.names = FALSE, print.keys = FALSE)
      }
    }
  ),
  active = list(
    #' @field predictions (`list()`)\cr
    #' Access the stored predictions.
    predictions = function() {
      self$data$predictions %??% rep(NA_real_, length(self$data$row_ids))
    },

    #' @field missing (`integer()`)\cr
    #' Returns `row_ids` for which the predictions are missing or incomplete.
    missing = function() {
      unique(unlist(map(self$predictions, "missing")))
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

#' @export
c.PredictionMultiOutput = function(..., keep_duplicates = TRUE) {

  dots = list(...)
  assert_list(dots, "PredictionMultiOutput")
  assert_flag(keep_duplicates)
  if (length(dots) == 1L || TRUE) {
    return(dots[[1L]])
  }

  predict_types = map(dots, "predict_types")
  if (!every(predict_types[-1L], setequal, y = predict_types[[1L]])) {
    stopf("Cannot rbind predictions: Different predict_types in objects.")
  }

  tab = map_dtr(dots, function(p) subset(p$data$tab), .fill = FALSE)
  prob = do.call(rbind, map(dots, "prob"))

  if (!keep_duplicates) {
    keep = !duplicated(tab, by = "row_id", fromLast = TRUE)
    tab = tab[keep]
    prob = prob[keep, , drop = FALSE]
  }

  PredictionMultiOutput$new(row_ids = tab$row_id, partition = tab$partition, prob = prob)
}
