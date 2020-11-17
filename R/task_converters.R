#' Convert to basic task_type
#'
#' Splits a [`TaskMultiOutput`] into several [`Task`]s of type "regr", "classif", ...
#' according to "task$task_types".
#' @param task [`TaskMultiOutput`]\cr
#'   Multi-output task to split into tasks of type `task$task_types`.
convert_to_basic_tasks = function(task) {
  assert_task(task, "multioutput")
  tasks = imap(task$task_types, function(type, tn) {
    tn = convert_task(task$clone(), target = tn, new_type = type, drop_original_target = TRUE)
  })
}

# nolint start
#' @export
as_task_classif.TaskMultiOutput = function(x, target = NULL, drop_original_target = FALSE, drop_levels = TRUE, ...) {
  mlr3:::convert_task(intask = x, target = target, new_type = "classif", drop_original_target = TRUE, drop_levels = TRUE)
}

#' @export
as_task_regr.TaskMultiOutput = function(x, target = NULL, drop_original_target = FALSE, drop_levels = TRUE, ...) {
  mlr3:::convert_task(intask = x, target = target, new_type = "regr", drop_original_target = TRUE, drop_levels = TRUE)
}
# nolint end