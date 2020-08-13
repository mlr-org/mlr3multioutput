#' @title Multioutput Task
#'
#' @description
#' This task specializes [mlr3::Task] for multi-label problems.
#' The `task_type` is set to `"multilabel"`.
#'
#' Predefined tasks are stored in the [dictionary][mlr3misc::Dictionary] [mlr_tasks].
#'
#' @template param_id
#' @template param_backend
#' @family Task
#' @export
TaskMultiLabel= R6Class("TaskMultiLabel",
  inherit = TaskMultiOutput,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' @param target (`character`)\cr
    #'   Name of the target columns.
    initialize = function(id, backend, target) {
      task_types = rep("classif", length(target))
      super$initialize(id = id, task_type = "multilabel", backend = backend, target = target, task_types = task_types)
      browser()
    }
  )
)
