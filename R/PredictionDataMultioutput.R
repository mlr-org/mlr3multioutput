#' @export
as_prediction.PredictionDataMultioutput = function(x, check = TRUE, ...) { # nolint
  invoke(PredictionMultioutput$new, check = check, .args = x)
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
  # FIXME What should happen here
  # if (!is.null(pdata$predictions)) {
  #   pdata$predictions = map(predictions, assert_prediction) # assert_prediction_data
  #}

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
  if (length(dots) == 1L) {
    return(dots[[1L]])
  }

  preds = map(dots, as_prediction)
  target_names = names(preds[[1]]$predictions)

  p = map(preds, "predictions")

  pc = map(target_names, function(i) do.call(c, lapply(p, `[[`, i)))
  names(pc) = target_names

  PredictionMultioutput$new(predictions = pc)
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
  expect_equal(names(data), names(target_types))
  imap(target_types, function(x, i) {
    invoke(mlr3:::new_prediction_data, data[[i]], task_type = x)
  })
}
# FIXME Improve docs, write and test code.
# Add as_prediction here?
