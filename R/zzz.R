#' @import data.table
#' @import mlr3misc
#' @import paradox
#' @import mlr3
#' @import checkmate
#' @importFrom R6 R6Class
"_PACKAGE"

register_mlr3 = function() {

  x = utils::getFromNamespace("mlr_reflections", ns = "mlr3")

  if (length(x$task_types[list("multiout"), on = "type", which = TRUE, nomatch = NULL]) == 0L) {
    x$task_types = setkeyv(rbind(x$task_types, rowwise_table(
      ~type, ~package, ~task, ~learner, ~prediction, ~measure,
      "multiout", "mlr3multiout", "TaskMultiOutput", "LearnerMultiOutput", "PredictionMultiOutput", "MeasureMultiOutput"
    )), "type")
    x$task_col_roles$multiout = x$task_col_roles$regr
    x$task_properties$multiout = x$task_properties$regr
    x$measure_properties$multiout = x$measure_properties$regr
    x$learner_predict_types$multiout = unique(unname(unlist(mlr_reflections$learner_predict_types)))
    x$task_target_types = rowwise_table(
      ~type, ~task_type,
      "factor", "classif",
      "numeric", "regr",
      "integer", "regr",
      "ordered", "ordinal"
     )
  }


  x = utils::getFromNamespace("mlr_tasks", ns = "mlr3")
  x$add("linnerud", load_task_linnerud)

  # x = utils::getFromNamespace("mlr_learners", ns = "mlr3")
  # x$add("multiout.featureless", LearnerMultiOutputFeatureless)


  # x = utils::getFromNamespace("mlr_measures", ns = "mlr3")
  # x$add("clust.db", MeasureClustInternal, name = "db")


}

.onLoad = function(libname, pkgname) { # nolint
  # nocov start
  backports::import(pkgname)
  register_mlr3()
} # nocov end
