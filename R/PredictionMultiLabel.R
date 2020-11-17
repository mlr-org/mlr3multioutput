#' @title Prediction Object for Multilabel Analysis
#'
#' @description
#' This object wraps the predictions returned by a learner of class [LearnerMultiOutput], i.e.
#' the predicted partition and MultiOutputer probability.
#'
#' @family Prediction
#' @export
PredictionClassif = R6Class("PredictionMultiLabel", inherit = Prediction,
  cloneable = FALSE,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #'
    #' @param task ([TaskClassif])\cr
    #'   Task, used to extract defaults for `row_ids` and `truth`.
    #'
    #' @param row_ids (`integer()`)\cr
    #'   Row ids of the predicted observations, i.e. the row ids of the test set.
    #'
    #' @param truth (`factor()`)\cr
    #'   Matrix of True (observed) labels. See the note on manual construction.
    #'
    #' @param response (`character()` | `factor()`)\cr
    #'   Matrix of predicted class labels.
    #'   One vector for each label in the test set.
    #'   Character vectors are automatically converted to factors.
    #'   See the note on manual construction.
    #'
    #' @param prob (`matrix()`)\cr
    #'   Numeric matrix of posterior class probabilities with one column for each class
    #'   and one row for each observation in the test set.
    #'   Columns must be named with class labels, row names are automatically removed.
    #'   If `prob` is provided, but `response` is not, the class labels are calculated from
    #'   the probabilities using [max.col()] with `ties.method` set to `"random"`.
    #'
    #' @param check (`logical(1)`)\cr
    #'   If `TRUE`, performs some argument checks and predict type conversions.
    initialize = function(task = NULL, row_ids = task$row_ids, truth = task$truth(), response = NULL, prob = NULL, check = TRUE) {
      pdata = list(row_ids = row_ids, truth = truth, response = response, prob = prob)
      pdata = discard(pdata, is.null)
      class(pdata) = c("PredictionDataMultiLabel", "PredictionData")

      if (check) {
        pdata = check_prediction_data(pdata)
      }
      self$task_type = "multilabel"
      self$man = "mlr3::PredictionMultiLabel"
      self$data = pdata
      self$predict_types = intersect(c("response", "prob"), names(pdata))
    },


    #' @description
    #' Sets the prediction response based on the provided threshold.
    #' See the section on thresholding for more information.
    #'
    #' @param threshold (`numeric()`).
    #' @param ties_method (`character(1)`)\cr
    #'   One of `"random"`, `"first"` or `"last"` (c.f. [max.col()]) to determine how to deal with
    #'   tied probabilities.
    #'
    #' @return
    #' Returns the object itself, but modified **by reference**.
    #' You need to explicitly `$clone()` the object beforehand if you want to keeps
    #' the object in its previous state.
    set_threshold = function(threshold, ties_method = "random") {
      if (!is.matrix(self$data$prob)) {
        stopf("Cannot set threshold, no probabilities available")
      }
      lvls = levels(self$truth)

      if (length(threshold) == 1L) {
        assert_number(threshold, lower = 0, upper = 1)
        if (length(lvls) != 2L) {
          stopf("Setting a single threshold only supported for binary classification problems")
        }
        prob = cbind(self$data$prob[, 1L], threshold)
      } else {
        assert_numeric(threshold, any.missing = FALSE, lower = 0, upper = 1, len = length(lvls))
        assert_names(names(threshold), permutation.of = lvls)
        threshold = threshold[lvls] # reorder thresh so it is in the same order as levels

        # multiply all rows by threshold, then get index of max element per row
        prob = self$data$prob %*% diag(1 / threshold) # can generate Inf for threshold 0
        prob[is.na(prob)] = 0 # NaN results from 0 * Inf, replace with 0, c.f. #452
      }

      ind = max.col(prob, ties.method = ties_method)
      self$data$response = factor(lvls[ind], levels = lvls)
      invisible(self)
    }
  ),


  active = list(
    #' @field response (`factor()`)\cr
    #' Access to the stored predicted class labels.
    response = function(rhs) {
      assert_ro_binding(rhs)
      self$data$response %??% factor(rep(NA, length(self$data$row_ids)), levels(self$data$truth))
    },

    #' @field prob (`matrix()`)\cr
    #' Access to the stored probabilities.
    prob = function(rhs) {
      assert_ro_binding(rhs)
      self$data$prob
    }
  )
)

#' @export
as.data.table.PredictionClassif = function(x, ...) { # nolint
  tab = as.data.table(x$data[c("row_ids", "truth", "response")])
  setnames(tab, "row_ids", "row_id")

  if ("prob" %in% x$predict_types) {
    prob = as.data.table(x$data$prob)
    setnames(prob, names(prob), paste0("prob.", names(prob)))
    tab = rcbind(tab, prob)
  }

  tab[]
}
