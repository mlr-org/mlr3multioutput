#' @title US Arrests Cluster Task
#'
#' @name mlr_tasks_usarrests
#' @format [R6::R6Class] inheriting from [TaskClust].
#'
#' @section Construction:
#' ```
#' mlr_tasks$get("usarrests")
#' tsk("usarrests")
#' ```
#'
#' @description
#' A cluster task for the [datasets::USArrests] data set.
NULL

load_task_linnerud = function(id = "linnerud") {
  b = as_data_backend(load_dataset("linnerud", "plsdepot"))
  task = TaskMultiOutput$new(id, b, target = c("Pulls", "Squats", "Jumps"))
  b$hash = task$man = "mlr3multioutput::mlr_tasks_multioutput"
  task
}
