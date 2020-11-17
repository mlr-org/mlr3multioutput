#' @export
as_prediction.PredictionDataMultioutput = function(x, check = TRUE) { # nolint
  invoke(PredictionMultiOutput$new, check = check, .args = x)
}


#' @export
check_prediction_data.PredictionDataMultioutput = function(pdata) { # nolint
  pdata$row_ids = assert_row_ids(pdata$row_ids)
  n = length(pdata$row_ids)

  if (length(pdata$row_ids) > 0L) {
    assert_true(all.equal(
      Reduce(function(x, y) if (identical(x,y)) x else FALSE,  map(pdata$predictions, "row_ids")),
      assert_row_ids(pdata$row_ids)
    ))
  }
  if (!is.null(pdata$predictions)) {
  #   pdata$predictions = map(predictions, assert_prediction) # assert_prediction_data
  }

  pdata
}


#' @export
is_missing_prediction_data.PredictionDataMultioutput = function(pdata) { # nolint
  miss = logical(length(pdata$row_id))

  response = map(pdata$prediction, function(x) x$response)
  if (!is.null(pdata$response)) {
    miss = miss | apply(sapply(response, is.na), 1, any)
  }

  pdata$row_ids[miss]
}


#' @export
c.PredictionDataMultioutput = function(..., keep_duplicates = TRUE) {
  dots = list(...)
  assert_list(dots, "PredictionDataMultioutput")
  assert_flag(keep_duplicates)
  if (length(dots) == 1L || TRUE) {
    return(dots[[1L]])
  }

  predict_types = names(mlr_reflections$learner_predict_types$multiout)
  predict_types = map(dots, function(x) intersect(names(x), predict_types))
  predict_types = map(dots, "predict_types")
  if (!every(predict_types[-1L], setequal, y = predict_types[[1L]])) {
    stopf("Cannot rbind predictions: Different predict_types in objects.")
  }

  tab = map_dtr(dots, function(p) subset(p$data), .fill = FALSE)
  prob = do.call(rbind, map(dots, "prob"))

  if (!keep_duplicates) {
    keep = !duplicated(tab, by = "row_id", fromLast = TRUE)
    tab = tab[keep]
    prob = prob[keep, , drop = FALSE]
  }

  PredictionMultiOutput$new(row_ids = tab$row_id, partition = tab$partition, prob = prob)
}



#' Convert a data.table prediction object from learners that inherently can do e.g. multilabel.
#'
#' @param data [`data.table`]\cr
#'   Contains predictions, must have row_id.
# Input is of the form:
# data = list(
#   targetname = list(
#     row_ids = 1:10,
#     truth = 1,
#     response = 1,
#     prob = 1,
#   ),
#   target2 = list(
#     row_ids = 1:10,
#     truth = 1,
#     response = 1,
#     prob = 1,
#   )
# )
# This is already saved in the task
# target_types = list("targetname" = "regr", "target2" = "classif")
#' @param task_types [`character`]\cr
#'   Named character vector of per-target task-types.
#'   E.g. c(tgt1 = "regr", tgt2 = "classif")
as.PredictionDataMultioutput = function(data, target_types) {
  assert_equal(names(data), names(target_types))
  imap(target_types, function(x, i) {
    invoke(PredictionXXX$new, data[[i]])
  })
}
# FIXME Improve docs, write and test code.
# new_prediction_data(
#         list(row_ids = row_ids, truth = truth, response = response, prob = prob),
#         task_type = "classif"
#       )