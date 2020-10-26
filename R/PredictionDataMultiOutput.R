#' @export
as_prediction.PredictionDataMultiOutput = function(x, check = TRUE) { # nolint
  invoke(PredictionMultiOutput$new, check = check, .args = x)
}


#' @export
check_prediction_data.PredictionDataMultiOutput = function(pdata) { # nolint
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
is_missing_prediction_data.PredictionDataMultiOutput = function(pdata) { # nolint
  miss = logical(length(pdata$row_id))

  response = map(pdata$prediction, function(x) x$response)
  if (!is.null(pdata$response)) {
    miss = miss | apply(sapply(response, is.na), 1, any)
  }

  pdata$row_ids[miss]
}


#' @export
c.PredictionDataMultiOutput = function(..., keep_duplicates = TRUE) {
  browser()
  dots = list(...)
  assert_list(dots, "PredictionDataMultiOutput")
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
