#' @title Multioutput Task
#'
#' @description
#' This task specializes [mlr3::Task] for multi-output problems.
#' The `task_type` is set to `"multioutput"`.
#'
#' Predefined tasks are stored in the [dictionary][mlr3misc::Dictionary] [mlr_tasks].
#' Properties for this [mlr3::Task] are 'multilabel' if the [mlr3::Task] has only binary
#' classification targets and 'multioutput' else.
#'
#' @template param_id
#' @template param_backend
#' @param task_types (`character()`)\cr
#'   Vector named with target names specifying the task_type for each respective target.
#'   Will be inferred if not provided.
#' @param task_type (`character(1)`)\cr
#'   Task type of the constructed task. Defautls to `"multioutput"`.
#' @family Task
#' @export
TaskMultioutput = R6Class("TaskMultioutput",
  inherit = TaskSupervised,
  public = list(
    #' @description
    #' Creates a new instance of this [R6][R6::R6Class] class.
    #' @param target (`character`)\cr
    #'   Name of the target columns.
    #' @param task_types [`character`]\cr
    #'   Named character vector of per-target task-types.
    #'   E.g. c(tgt1 = "regr", tgt2 = "classif")
    initialize = function(id, backend, target, extra_args = list(task_types = NULL)) {
      map(target, assert_string)
      super$initialize(
        id = id, task_type = "multioutput", backend = backend,
        target = target, extra_args = extra_args
      )

      private$.update_class_property()
      self$task_types = check_task_types(self, self$extra_args$task_types) %??% infer_task_types(self)
    },
    #' @field task_types (`character()`)\cr
    #' See `initialize`.
    task_types = NULL,
    # FIXME: Add 'formula'.
    formula = function(rhs = ".") {
      as.formula(sprintf("%s ~ %s", paste0(self$target_names, collapse = " + "), paste0(rhs, collapse = " + ")))
    }
  ),
  private = list(
    .update_class_property = function() {
      # Checks whether all targets are binary & classif
      mlbl = all(map_lgl(self$data(cols = self$target_names), function(x) {
        test_factor(as.factor(x), len = self$nrow, n.levels = 2L)
      }))

      private$.properties = setdiff(private$.properties, c("multioutput", "multilabel"))
      private$.properties = union(private$.properties, if (mlbl) "multilabel" else "multioutput")
    }
  )
)

# infers task types from targets via 'mlr_reflections$task_target_types' lookup
infer_task_types = function(self) {
  dt = merge(self$col_info[id %in% self$target_names], mlr_reflections$task_target_types, by = "type")
  tt = setNames(dt$task_type, dt$id)
  tt[self$target_names]
}

check_task_types = function(self, task_type) {
  if (is.null(task_type)) return(task_type)
  assert_named(task_type, type = "unique")
  assert_true(all(task_type %in% mlr_reflections$task_type$type))
  assert_true(all(names(task_type) %in% self$target_names))
  return(task_type)
}
