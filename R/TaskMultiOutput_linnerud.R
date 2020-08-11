#' @title Linnerud Task
#'
#' @name mlr_tasks_linnerud
#' @format [R6::R6Class] inheriting from [TaskMultiOutput].
#'
#' @section Construction:
#' ```
#' mlr_tasks$get("linnerud")
#' tsk("linnerud")
#' ```
#'
#' @description
#' A multi-output task for the [plsdepot::linnerud] data set.
NULL

load_task_linnerud = function(id = "linnerud") {
  b = as_data_backend(load_dataset("linnerud", "plsdepot"))
  task = TaskMultiOutput$new(id, b, target = c("Pulls", "Squats", "Jumps"))
  b$hash = task$man = "mlr3multioutput::mlr_tasks_linnerud"
  task
}
