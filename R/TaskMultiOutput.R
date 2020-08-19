#' @title Multioutput Task
#'
#' @description
#' This task specializes [mlr3::Task] for multi-output problems.
#' The `task_type` is set to `"multiout"`.
#'
#' Predefined tasks are stored in the [dictionary][mlr3misc::Dictionary] [mlr_tasks].
#'
#' @template param_id
#' @template param_backend
#' @family Task
#' @export
TaskMultiOutput = R6Class("TaskMultiOutput",
  inherit = TaskSupervised,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' @param target (`character`)\cr
    #'   Name of the target columns.
    #' @param task_type [`character`]\cr
    #'   Named character vector of per-target task-types.
    #'   E.g. c(tgt1 = "regr", tgt2 = "classif")
    initialize = function(id, backend, target, task_type = "multiout") {
      super$initialize(id = id, task_type = task_type, backend = backend, target = target)
      self$task_type = check_task_type(self, task_type) %??% infer_task_type(self)
    },
    #' @field task_type (`character()`)\cr
    #' See `initialize`.
    task_type = NULL
  )
)

# infers task types from targets via 'mlr_reflections$task_target_types' lookup
infer_task_type = function(self) {
  dt = merge(self$col_info[id %in% self$target_names], mlr_reflections$task_target_types, by = "type")
  tt = setNames(dt$task_type, dt$id)
  tt[self$target_names]
}

check_task_type = function(self, task_type) {
  if (is.null(task_type)) return(task_type)
  assert_named(task_type, type = "unique")
  assert_true(all(task_type %in% mlr_reflections$task_type$type))
  assert_true(all(names(task_type) %in% self$target_names))
  return(task_type)
}
