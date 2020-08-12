#' @import data.table
#' @import mlr3misc
#' @import paradox
#' @import mlr3
#' @import mlr3pipelines
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
    x$task_col_roles$multiout = c(unique(unlist(x$task_col_roles)), c("target_regr", "target_classif"))
    x$task_properties$multiout = x$task_properties$regr
    x$measure_properties$multiout = x$measure_properties$regr
    x$default_measures$multiout = "multiout.default"

    # predict_types are predict_types of all other task types.
    prd_types = do.call("c", unname(x$learner_predict_types))
    x$learner_predict_types$multiout = prd_types[!duplicated(prd_types)]
    x$learner_properties$multiout = unique(unlist(x$learner_properties))
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

  x = utils::getFromNamespace("mlr_learners", ns = "mlr3")
  x$add("multiout.featureless", LearnerMultiOutputFeatureless)
  # x$add("multiout.plsreg2", LearnerMultiOutputPLSReg2)

  x = utils::getFromNamespace("mlr_measures", ns = "mlr3")
  defs = map(mlr_reflections$default_measures[which(!(names(mlr_reflections$default_measures) == "multiout"))], msr)
  x$add("multiout.default",
    MeasureMultiOutputWeightedAvg$new("default", defs),
    name = paste0("multiobj", map_chr(defs, "id"), sep = "_"))
  x$add("multiout.custom", MeasureMultiOutputWeightedAvg)
}

.onLoad = function(libname, pkgname) { # nolint
  # nocov start
  backports::import(pkgname)
  register_mlr3()
} # nocov end

