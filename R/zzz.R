#' @import data.table
#' @import mlr3misc
#' @import paradox
#' @import mlr3
#' @import mlr3pipelines
#' @import checkmate
#' @importFrom R6 R6Class
"_PACKAGE"

register_mlr3 = function() { # nolint
  x = utils::getFromNamespace("mlr_reflections", ns = "mlr3")

  if (!grepl("multioutput", x$task_types[, "type"])) {
    x = utils::getFromNamespace("mlr_reflections", ns = "mlr3")
    x$task_types = setkeyv(rbind(x$task_types, rowwise_table(
      ~type, ~package, ~task, ~learner, ~prediction, ~measure,
      "multioutput", "mlr3multioutput", "TaskMultioutput", "LearnerMultioutput", "PredictionMultioutput", "MeasureMultioutput"
    )), "type")
    x$task_col_roles$multioutput = c(unique(unlist(x$task_col_roles)), c("target_regr", "target_classif"))
    x$task_properties$multioutput = c(x$task_properties$regr, c("multilabel", "multioutput"))
    x$measure_properties$multioutput = c(x$measure_properties$regr, c("multilabel", "multioutput"))

    # predict_types are predict_types of all other task types.
    prd_types = do.call("c", unname(x$learner_predict_types))
    x$learner_predict_types$multioutput = prd_types[!duplicated(prd_types)]
    x$learner_properties$multioutput = c(unique(unlist(x$learner_properties)), c("multilabel", "multioutput"))
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
  x$add("flags", load_task_flags)

  x = utils::getFromNamespace("mlr_learners", ns = "mlr3")
  x$add("multioutput.featureless", LearnerMultioutputFeatureless)
  x$add("multioutput.cforest", LearnerMultioutputCForest)
  x$add("multioutput.keras", LearnerMultioutputKeras)
  x$add("multioutput.kerasff", LearnerMultioutputKerasFF)
  x$add("multioutput.kerascnn", LearnerMultioutputKerasCNN)
  x$add("multioutput.kerascnnworderror", LearnerMultioutputKerasCNNWordError)

  x = utils::getFromNamespace("mlr_measures", ns = "mlr3")
  defs = map(mlr_reflections$default_measures[which(!(names(mlr_reflections$default_measures) == "multioutput"))], msr)
  x$add("multioutput.default",
    MeasureMultioutputWeightedAvg$new("default", defs),
    name = paste0("multiobj", map_chr(defs, "id"), sep = "_")
  )
  x$add("multioutput.custom", MeasureMultioutputWeightedAvg, measures = defs)

  x = utils::getFromNamespace("mlr_reflections", ns = "mlr3")
  x$default_measures$multioutput = "multioutput.default"
}

register_mlr3pipelines = function() {
  x = utils::getFromNamespace("mlr_pipeops", ns = "mlr3pipelines")

  x$add("multioutputsplit", PipeOpSplitMultioutput)
  x$add("multioutputunite", PipeOpPredictionMultioutputUnite)
  x$add("multioutputlrn", PipeOpMultioutputLearner)
}

.onLoad = function(libname, pkgname) { # nolint
  # reticulate::configure_environment(pkgname)
  # suppressMessages(try(keras::use_implementation("tensorflow"), silent = TRUE))
  # nocov start
  register_mlr3()
  if (requireNamespace("mlr3pipelines", quietly = TRUE)) {
    register_mlr3pipelines()
  }

  setHook(packageEvent("mlr3", "onLoad"), function(...) register_mlr3(), action = "append")
  setHook(packageEvent("mlr3pipelines", "onLoad"), function(...) register_mlr3pipelines(), action = "append")
  backports::import(pkgname)
} # nocov end


.onUnload = function(libpath) { # nolint
  # nocov start
  event = packageEvent("mlr3", "onLoad")
  hooks = getHook(event)
  pkgname = vapply(hooks[-1], function(x) environment(x)$pkgname, NA_character_)
  setHook(event, hooks[pkgname != "mlr3multioutput"], action = "replace")

  event = packageEvent("mlr3pipelines", "onLoad")
  hooks = getHook(event)
  pkgname = vapply(hooks[-1], function(x) environment(x)$pkgname, NA_character_)
  setHook(event, hooks[pkgname != "mlr3multioutput"], action = "replace")

  library.dynam.unload("mlr3multioutput", libpath)
} # nocov end

# silence R CMD check for callbacks:
utils::globalVariables("model") # nocov end
