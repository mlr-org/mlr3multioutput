#' @param name (`character(1)`)\cr
#'   Name of the measure. Default: "weightedavg".
#' @param measures ([`list`])\cr
#'   Named list of measures to be applied to each "target".\cr
#'   Either named with `target_names`, mapping targets to measures or
#'   named with `task_types`, defining one measure per task_type.
#'   Defaults to `mlr_reflections$default_measures` for each task type.
#' @param weights (`numeric()`)\cr
#'   Named numeric defining weights for respective 'measures'.\cr
#'   Either named with `target_names`, mapping targets to weights or
#'   named with `task_types`, defining one weight per task_type.
#'   Defaults to `1` for each measure.
