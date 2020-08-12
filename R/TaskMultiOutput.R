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
    #' @param task_types [`character`]\cr
    #'   Named character vector of per-target task-types.
    #'   E.g. c(tgt1 = "regr", tgt2 = "classif")
    initialize = function(id, backend, target, task_types = NULL) {
      super$initialize(id = id, task_type = "multiout", backend = backend, target = target)
      self$task_types = check_task_types(self, task_types) %??% infer_task_types(self)
    },
    #' @field task_types (`character()`)\cr
    #' See `initialize`.
    task_types = NULL
  )
)

# infers task types from targets via 'mlr_reflections$task_target_types' lookup
infer_task_types = function(self) {
  dt = merge(self$col_info[id %in% self$target_names], mlr_reflections$task_target_types, by = "type")
  tt = setNames(dt$task_type, dt$id)
  tt[self$target_names]
}

check_task_types = function(self, task_types) {
  if (is.null(task_types)) return(task_types)
  assert_named(task_types, type = "unique")
  assert_true(all(task_types %in% mlr_reflections$task_types$type))
  assert_true(all(names(task_types) %in% self$target_names))
  return(task_types)
}

#' Convert to basic task_type
#'
#' Splits a [`TaskMultiOutput`] into several [`Task`]s of type "regr", "classif", ...
#' according to "task$task_types".
#' @param task [`TaskMultiOutput`]\cr
#'   Multi-output task to split into tasks of type `task$task_types`.
convert_to_basic_tasks = function(task) {
  assert_task(task, "multiout")
  tasks = imap(task$task_types, function(type, tn) {
    tn = convert_task(task$clone(), target = tn, new_type = type)
  })
}