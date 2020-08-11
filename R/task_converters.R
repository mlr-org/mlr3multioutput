#' @export
as_task_classif.TaskMultiOutput = function(x, target = NULL, drop_original_target = FALSE, drop_levels = TRUE, ...) {
  mlr3:::convert_task(intask = x, target = target, new_type = "classif", drop_original_target = TRUE, drop_levels = TRUE)
}

#' @export
as_task_regr.TaskMultiOutput = function(x, target = NULL, drop_original_target = FALSE, drop_levels = TRUE, ...) {
  mlr3:::convert_task(intask = x, target = target, new_type = "regr", drop_original_target = TRUE, drop_levels = TRUE)
}