#' @title flags Task
#'
#' @name mlr_tasks_flags
#' @format [R6::R6Class] inheriting from [TaskMultioutput].
#'
#' @section Construction:
#' ```
#' mlr_tasks$get("flags")
#' tsk("flags")
#' ```
#'
#' @description
#' A multi-output task for the [mldr.datasets::flags] data set.
NULL

load_task_flags = function(id = "flags") {
  d = remove_named(load_dataset("flags", "mldr.datasets")$dataset, c(".labelcount", ".SCUMBLE"))
  d[, sapply(d, is.character)] = as.data.frame(lapply(d[, sapply(d, is.character)], as.factor))
  target = c("red", "green", "blue", "yellow", "white", "black", "orange")
  d[, target] = as.data.frame(lapply(d[, target], as.factor))
  b = as_data_backend(d)
  task = TaskMultioutput$new(id, b, target = c("red", "green", "blue", "yellow", "white", "black", "orange"))
  b$hash = task$man = "mlr3multioutput::mlr_tasks_flags"
  task
}
